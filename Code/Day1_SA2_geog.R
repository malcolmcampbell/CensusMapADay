# CENSUS MAP A DAY
# DAY 1 - Population - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")

download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

SA2 <- st_read(dsn = 
                  "Data/statistical-area-2-2018-clipped-generalised.gpkg")

#static version
tm_shape(SA2) + 
  tm_polygons(col="dodgerblue2", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "Statistical Area 2 Geography, Census 2018, New Zealand", 
            main.title.size = 1) 

#interactive version
tmap_mode("view")
tm_shape(SA2) + 
  tm_polygons(col="dodgerblue2", border.col = "grey20", alpha = 0.25, 
              id = "SA22018_V1_NAME",
              popup.vars = c("SA22018_V1_00_NAME", "LAND_AREA_SQ_KM")) 

#END
