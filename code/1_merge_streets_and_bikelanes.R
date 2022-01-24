# This script: imports street data for NYC

# NB this scripts uses three levels of street aggregation
#...1) Street: the full street, based on its name
#...2) StreetSegment: as defined by the NYC street centerline data
#...3) StreetSegmentPart: segments split into straight parts (i.e. straight segments have 1 part)

library('tidyverse')
library('data.table')
library('stringr')
library('geosphere')
library('nabor')
library('sf')
library('sp')

# start timer
ptm <- proc.time()


# panel parameters ---------------------------------------------------------------------------------
path_lion <- "data/raw/lion/lion_16d/lion.gdb"
path_dot_bikelanes <- "data/raw/dot/nyc-bike-routes/nyc_bike_routes_2017.shp"

# potential improvement: flexible versions for input
# maybe load all DOT versions to improve past snap shots


temp_res <- 'month'
distance_unit <- 'meter'  # or miles  



# subsets

# temporal
start_date <- '1899-01-01'
end_date <- '2099-09-09'

# spatial
boros <- 'all'

# road types
include_road_types <- c(....)


# import -------------------------------------------------------------------------------------------

# read bike lanes layer
dtBL <- data.table(read_sf("data/raw/bikeroutes/nyc-bike-routes/nyc_bike_routes_2017.shp"))
dtBL[, geometry := NULL]

# load lion, database with all streets in NYC
sfS <-read_sf(dsn = "data/raw/NYC_spatial/lion_16d/lion.gdb", layer = "lion")

# switch to WGS84 projection
sfS <- st_transform(sfS, crs = "+proj=longlat +datum=WGS84")

# transform all geoms to multiline strings
sfS <- st_cast(sfS, to = "MULTILINESTRING")

# get vector of all coordinates
vGeoms <- st_as_text(st_geometry(sfS))

# put in data.table
dtS <- data.table(sfS)[, .(Street,
                           StreetSegID = as.numeric(SegmentID), 
                           PhysicalID,
                           Number_Travel_Lanes,
                           Number_Park_Lanes,
                           Number_Total_Lanes,
                           RW_TYPE = gsub(" ", "", RW_TYPE),
                           Boro = RBoro, 
                           TrafDir,
                           BikeLane,
                           StreetWidth_Min, 
                           StreetWidth_Max,
                           SegmentLength = ifelse(distance_unit == 'meter', 
                                                  0.3048 * SHAPE_Length, 
                                                  SHAPE_Length)
)] 


# add geometry
dtS[, the_geom := vGeoms]


# merge  bikelanes with streets to get streetsegments ----------------------------------------------
dtBL[, StreetSegID := as.numeric(SegmentID)]


dtSS <- merge(unique(dtS), 
              unique(dtBL[, .(StreetSegID, instdate, moddate, comments, ft_facilit, tf_facilit )]),
              by = c("StreetSegID"), 
              all.x = T)


# add biketrafdir
dtSS[tf_facilit == "", tf_facilit := NA]
dtSS[ft_facilit == "", ft_facilit := NA]

dtSS[is.na(tf_facilit) & !is.na(ft_facilit) , BIKE_TRAFDIR := "FT"]
dtSS[!is.na(tf_facilit) & is.na(ft_facilit) , BIKE_TRAFDIR := "TF"]
dtSS[!is.na(tf_facilit) & !is.na(ft_facilit) , BIKE_TRAFDIR := "TW"]


# remove duplicates and select only normal streets (e.g. no highways)

dtSS[, max_width_boro := max(StreetWidth_Min, na.rm = T), by = Boro]

dtSS[, Boro := as.integer(mean(Boro, na.rm = T)), by = StreetSegID] # some have missing boro's

dtSS <- unique(dtSS)

dtSS <- dtSS[is.na(RW_TYPE) | RW_TYPE == "1"] # no highways
dtSS <- dtSS[!is.na(Boro)]

# remove duplicates in indicator probably redundant due to unique
dtSS <- dtSS[, .SD[1], by = StreetSegID]


# assign streetID
dtSS[, StreetID := paste0(Street, " Boro", Boro)]

# get nodes of each segment
dtSS[, lat_start := gsub("[,].*", "", the_geom)]
dtSS[, lat_start := as.numeric(gsub(".*[[:space:]]|)", "", lat_start))]
dtSS[, lon_start := gsub(".*[((]", "", the_geom)]
dtSS[, lon_start := as.numeric(gsub("[ ][0-9].*", "", lon_start))]

dtSS[, lat_end := gsub(".*[ ]-", "-", the_geom)]
dtSS[, lat_end := as.numeric(gsub("[))]|.*[ ]", "", lat_end))]
dtSS[, lon_end := gsub(".*[ ]-|.*,-", "-", the_geom)]
dtSS[, lon_end := as.numeric(gsub("[ ].*", "", lon_end))]


# set the installation time of bikelanes -----------------------------------------------------------
## The bike lane data works as follows (following manual inspection in google streetview)
#.. if protected path and ModDate != InstallDate, there was an upgrade from standard to protected 
# e.g. look at example here: 40°44'41.3"N 73°59'29.1"W
#.. if protected path and ModDate == InstallDate, there was an upgrade from nothing to protected 
#.. e.g. example here: 40°46'45.5"N 73°58'52.1"W

dtSS[, instdate := gsub("T", " ", instdate)]

dtSS[, moddate := gsub("T", " ", moddate)]

dtSS[, InstallDate := as.POSIXct(substr(instdate, 1, 19), 
                                 format = "%Y-%m-%d", 
                                 tz = "America/New_York")]

dtSS[, ModDate := as.POSIXct(substr(moddate, 1, 19), 
                             format = "%Y-%m-%d", 
                             tz ="America/New_York")]

dtSS[, instdate:=NULL][, moddate:=NULL]


# add arbitrary start and end date ("2099-09-09") to each route for timewindows

dStartDate <- as.POSIXct(start_date, tz = "America/New_York")

dEndDate <- as.POSIXct(end_date, tz = "America/New_York")


# add dStartDate to cases without bike infra
dtSS[is.na(InstallDate), InstallDate := dStartDate]


# expand the cases that are upgraded twice (so 3 time windows in total)
dtSS[ , InfraUpgrades := 1] # for normal cases

dtSS[InstallDate < ModDate  & (grepl("rotected|urbside", tf_facilit) | 
                                 grepl("rotected|urbside", ft_facilit) | 
                                 grepl("owngrade", comments)) , InfraUpgrades := 2]


# expand cases with two relevant time windows
dtSS <- dtSS[, .(TimeWindow = 2:(InfraUpgrades+1)), by = c(names(dtSS))]



# set the pre moddate type to standard for the above cases with any up/downgrade

dtSS[TimeWindow == 2 & InfraUpgrades == 2, ft_facilit := ifelse(ft_facilit != "", "Standard", "")]

dtSS[TimeWindow == 2 & InfraUpgrades == 2, tf_facilit := ifelse(tf_facilit != "", "Standard", "")]


# some cases are upgraded from shared to protected instead of from standard
dtSS[grepl("tandard", ft_facilit) & grepl("Upgraded from shared", comments) & TimeWindow ==2, 
     ft_facilit:=ifelse(ft_facilit!="", "Sharrows", "")]

dtSS[grepl("tandard", tf_facilit) & grepl("Upgraded from shared", comments) & TimeWindow ==2, 
     tf_facilit:=ifelse(tf_facilit!="", "Sharrows", "")]


# change the install date to mod date for cases  timewindow 3
dtSS[TimeWindow == 3, InstallDate := ModDate]

# for each segment location, get the dates of change
dtChangeDatesPerSegm <- unique(dtSS[, 
                                    .(TimeWindowFrom = c(dStartDate, unique(InstallDate), dEndDate)),
                                    by = .(StreetSegID)])

# order time per segment and add timewindow
dtChangeDatesPerSegm <- dtChangeDatesPerSegm[order(TimeWindowFrom)][order(StreetSegID)]


# apply shift and remove NA cols
dtChangeDatesPerSegm[, TimeWindowTo := data.table::shift(TimeWindowFrom, 1, type = "lead"),
                     by = .(StreetSegID)]

dtChangeDatesPerSegm <- dtChangeDatesPerSegm[!is.na(TimeWindowTo)]


# merge time data with initial data for each segment and timewindow
dtSS <- merge(
        dtSS,
        dtChangeDatesPerSegm,
        by = c("StreetSegID"),
        all.x = T,
        all.y = T,
        allow.cartesian = T
)

# view a sample street
# View(dtSS[Street=="2 AVENUE"])


# assign the correct counter and install date for first time window 
dtSS[TimeWindowFrom == dStartDate & TimeWindow == 2, TimeWindow := 1]
dtSS[TimeWindow == 1, InstallDate := dStartDate]

# remove cases with installdate out of time window
dtSS <- dtSS[InstallDate == TimeWindowFrom]

# set the infrastracture to blank before the installation date
dtSS[TimeWindow == 1, tf_facilit := ""]
dtSS[TimeWindow == 1, ft_facilit := ""]



# select only relevant cols
dtSS <- unique(dtSS[, .(Street, 
                        StreetID, 
                        StreetSegID, 
                        Boro, 
                        SegmentLength, 
                        the_geom,
                        lat_start, 
                        lon_start, 
                        lat_end, 
                        lon_end,
                        TrafDir, 
                        StreetWidth = ifelse(distance_unit == 'meter', 0.3048*StreetWidth_Min, StreetWidth_Min),
                        BikeLane, 
                        Number_Travel_Lanes,
                        Number_Park_Lanes,
                        Number_Total_Lanes,
                        BIKE_TRAFDIR, 
                        ft_facilit, tf_facilit,
                        TimeWindowFrom, TimeWindowTo, 
                        TimeWindowCounter = TimeWindow,
                        comments)])

# final variable additions -------------------------------------------------------------------------

# set NA facility to blank
dtSS[is.na(ft_facilit), ft_facilit := ""]
dtSS[is.na(tf_facilit), tf_facilit := ""]
dtSS[is.na(BIKE_TRAFDIR), BIKE_TRAFDIR := ""]
dtSS[is.na(comments), comments := ""]

# add maximum time window for non bike infra streets
dMaxTime <- dtSS[, max(TimeWindowTo, na.rm = T)]
dMinTime <- dtSS[, min(TimeWindowFrom, na.rm = T)]

dtSS[is.na(TimeWindowTo), TimeWindowTo := dMaxTime]
dtSS[is.na(TimeWindowFrom), TimeWindowFrom := dMinTime]


# morph into panel structure -----------------------------------------------------------------------


# tbc



# export -------------------------------------------------------------------------------------------

# stop timer
cat(sprintf("Run time: %1.4f minutes, ", ((proc.time() - ptm)[3])/60))
