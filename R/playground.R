library(dplyr)
library(tidytransit)
summary(tidytransit::feedlist_df)
houston_feedlist <- filter(tidytransit::feedlist_df, loc_t == 'Houston, TX, USA')
houston_gtfs_url <- houston_feedlist[1, "url_d"]
houston <- tidytransit::read_gtfs(houston_gtfs_url, geometry = TRUE)
fort_collins<-tidytransit::read_gtfs("http://www.ridetransfort.com/img/site_specific/uploads/google_transit.zip")
plot(houston)
#


