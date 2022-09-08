# CENSUS MAP A DAY

# DAY 1 - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2 Geography
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/103902-2018-census-usually-resident-population-and-age-groups-by-statistical-area-2/

# CENSUS MAP A DAY
# DAY 2 - Geography of Timaru Urban Area (at 2017) - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# Create DHB from SA2 using a concordance and Health NZ regions
# https://www.google.com/url?sa=t&rct=j&q=&esrc

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

# LOAD in the SA2 file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")

SA2$SA22018_V1_00 <- as.numeric(as.character(SA2$SA22018_V1_00))

#static version
tm_shape(SA2) + 
  tm_polygons()+
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "Census Statistical Area 2 Geography, 2018, New Zealand", 
            main.title.size = 1)
