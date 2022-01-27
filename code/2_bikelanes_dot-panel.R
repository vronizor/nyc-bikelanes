library('tidyverse')
library('data.table')
library('stringr')
library('geosphere')
library('nabor')
library('sf')
library('sp')


# datasets ---------------------------------------------------------------------------------

# paths -------------
# as of 2019
path_dot_bikelanes <- "2_data/1_raw/bike-lane-net/nyc_bicycle-network_2020/NYC_BICYCLE_NETWORK_18D_20200214.shp"

# DOT bike lanes exported in 2020 (state of 2019) asks for SegmentID in LION16d
# -> will only need this information when comparing with DCP panel
path_lion <- "2_data/1_raw/lion/lion16/lion/lion.gdb"

# load -------------
dot.sf = read_sf(path_dot_bikelanes)
dot_geom.v = st_as_text(st_geometry(dot.sf))

dot.dt <- data.table(dot.sf)

# trick to keep the geometry and run a unique on the data.table
dot.dt = dot.dt[, geometry := dot_geom.v]


# cleaning and categorization of facilities ---------------------------------------------------------------------------------

dot.dt[, lion16d_id := as.numeric(segmentid)]

nrow(dot.dt) - nrow(unique(dot.dt))
# 3 duplicates -> remove for now

dot.dt = unique(dot.dt)

# clean dates -------------
dot.dt[, instdate := gsub("T", " ", instdate)]
dot.dt[, moddate := gsub("T", " ", moddate)]

dot.dt[, install_date := as.POSIXct(substr(instdate, 1, 19), 
                                           format = "%Y-%m-%d", 
                                           tz = "America/New_York")]

dot.dt[, modif_date := as.POSIXct(substr(moddate, 1, 19), 
                                         format = "%Y-%m-%d", 
                                         tz ="America/New_York")]

dot.dt[, `:=`(instdate = NULL, moddate=NULL)]

# reminder: if never modified, modif date == install date
nrow(dot.dt[install_date == modif_date]) # majority of cases

dot.dt[install_date > modif_date]
# some modified dates are before the install date -> might be because of unknown install/modification date
# for now, we will consider those as never modified (not a huge issue if only looking at yearly)
dot.dt[install_date > modif_date, modif_date := install_date]

# clean categories -------------
unique(dot.dt[, facilitycl])
dot.dt[facilitycl == "i", facilitycl := "I"]
dot.dt[, .N, by = facilitycl]

unique(dot.dt[, tf_facilit])
unique(dot.dt[, ft_facilit])

# all combinations of Class and facilities
class_facilities = dot.dt[, .N, by = .(facilitycl, tf_facilit, ft_facilit)][order(facilitycl, -N)]
class_facilities

class_facilities_man = dot.dt[boro == 1, .N, by = .(facilitycl, tf_facilit, ft_facilit)][order(facilitycl, -N)]
# some inconsistencies to correct or drop
# I                 Standard              Standard   65
# II                Greenway              Greenway    9
# II          Protected Path                  <NA>    6
# II                    <NA>        Protected Path    3
# II          Protected Path        Protected Path    2
# III               Standard                  <NA>   15
# III               Curbside                  <NA>    5
# III                   <NA>              Standard    2
# drop for now until we know which variable to trust (107 obs)

precleancat = nrow(dot.dt)

dot.dt = dot.dt[!((facilitycl == "I" & tf_facilit == "Standard" & ft_facilit == "Standard") |
                    (facilitycl == "II" & tf_facilit == "Greenway" & ft_facilit == "Greenway") |
                    (facilitycl == "II" & tf_facilit == "Protected Path" & is.na(ft_facilit)) |
                    (facilitycl == "II" & is.na(tf_facilit) & ft_facilit == "Protected Path") |
                    (facilitycl == "II" & tf_facilit == "Protected Path" & ft_facilit == "Protected Path") |
                    (facilitycl == "III" & tf_facilit == "Standard" & is.na(ft_facilit)) |
                    (facilitycl == "III" & tf_facilit == "Curbside" & is.na(ft_facilit)) |
                    (facilitycl == "III" & is.na(tf_facilit) & ft_facilit == "Standard"))]

precleancat - nrow(dot.dt) # close enough
rm(precleancat)

# previous class -------------
# extrapolate previous class from comments
# if comments are available, use that to assign previous class

comments_all = dot.dt[!is.na(comments), 
                      .(comments, install_date, modif_date, facilitycl, tf_facilit, ft_facilit)]

comments_narrow = comments_all[comments %ilike% "grade|change|remove|add|install"][order(comments)]

comments_negativeout = comments_all[!(comments %ilike% "previously|auto-free|bridge|permitted|limited|dirt|Boardwalk open|cross with"),
                                    .(comments, install_date, modif_date, facilitycl, tf_facilit, ft_facilit)][order(comments)]

# (1) Clear up-/downgrades

## Downgrade
# from standard to shared (II -> III)
dot.dt[comments %ilike% "downgraded to shared|curbside lane changed to shared" & 
         facilitycl == "III", 
       facilitycl_prev := "II"]

# from protected to standard (I -> II)
dot.dt[comments %ilike% "Downgraded from PBL|downgraded to curbside when kwik-curb was removed|downgraded to lane from protected" &
         facilitycl == "II",
       facilitycl_prev := "I"]

## Upgrade
# from shared to standard (III -> II)
dot.dt[comments %ilike% "Upgraded from Signed|sharrows upgraded to standard|upgraded from shared to standard|
       shared installed 6/1/2010|shared lane implemented 6/1/2010" &
         facilitycl == "II",
       facilitycl_prev := "III"]

# from shared to protected (III -> I)
dot.dt[comments %ilike% "Upgraded from shared to protected" &
         facilitycl == "I",
       facilitycl_prev := "III"]

# from standard to protected (II -> I)
dot.dt[comments %ilike% "conventional lanes installed 10/17/2008|10/1/2014 lanes installed|
       2013 - two way lane added on west curb. 2015 - jersey barriers added to provide protection" &
         facilitycl == "I",
       facilitycl_prev := "II"]


## Checking and adding as we go (recursively, manually)
comments_all_rec = dot.dt[!is.na(comments), 
                      .(comments, install_date, modif_date, facilitycl, facilitycl_prev, tf_facilit, ft_facilit)]
comments_narrow_rec = comments_all_rec[comments %ilike% "grade|change|remove|add|install"][order(comments)]
comments_narrow_left = comments_narrow_rec[is.na(facilitycl_prev)]


# (2) Modification was an up-/downgrades

## Second best guess about whether modification was an up-/downgrade
# the comments do not say the previous facility type, but at least say whether it was up- or downgraded from something

modif_wcomments = dot.dt[modif_date > install_date & !is.na(comments) & is.na(facilitycl_prev) &
                           !(comments %ilike% "previously|auto-free|bridge|permitted|limited|dirt|Boardwalk open|cross with"),
                         .(comments, install_date, modif_date, facilitycl, facilitycl_prev, tf_facilit, ft_facilit)][order(comments)]

# Probable upgrades
dot.dt[comments %ilike% "2013 - two way lane added on west curb|5/24/15 shared lane added on right side" &
         is.na(facilitycl_prev),
       facilitycl_fromprev :="up"]

# Probable downgrades
dot.dt[comments %ilike% "downgrade to shared" &
         is.na(facilitycl_prev),
       facilitycl_fromprev :="down"]

# We assume for those that the up-/downgrade was from the directly adjacent class
dot.dt[facilitycl_fromprev == "up" & is.na(facilitycl_prev),
       facilitycl_prev := fcase(facilitycl == "I", "II",
                                facilitycl == "II", "III",
                                default = NA)]

dot.dt[facilitycl_fromprev == "down" & is.na(facilitycl_prev),
       facilitycl_prev := fcase(facilitycl == "II", "I",
                                facilitycl == "III", "II",
                                default = NA)]

# (3) For those modifications with inconclusive or absent comments, we assume the same type throughout

dot.dt[modif_date > install_date & is.na(facilitycl_prev),
       facilitycl_prev := facilitycl]

# clean up
dot.dt[, facilitycl_fromprev := NULL]
to_rm = ls()[grep("comments", ls())]
rm(list = to_rm)


# expand years  ---------------------------------------------------------------------------------
time_range = 2009:2019

dot_panel.dt = dot.dt[, .(year = time_range), by = names(dot.dt)]
dot_panel.dt[, `:=`(install_year = as.numeric(format(install_date, format = "%Y")),
                    modif_year = as.numeric(format(modif_date, format = "%Y")))]

dot_panel.dt[modif_date == install_date & year >= install_year,
             bike_lane_class := facilitycl]

dot_panel.dt[modif_date > install_date & year >= install_year & year < modif_year,
             bike_lane_class := facilitycl_prev]

dot_panel.dt[modif_date > install_date & year >= modif_year,
             bike_lane_class := facilitycl]

setcolorder(dot_panel.dt, c("segmentid", "street", "boro", 
                            "facilitycl", "facilitycl_prev", "install_date", "modif_date", "year", "bike_lane_class"))

View(dot_panel.dt[modif_date > install_date & !is.na(comments)])
