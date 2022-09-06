# CENSUS MAP A DAY
# DAY 4 - Territotial Authorities (at 2018) - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Territorial Authority
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92215-territorial-authority-2018-clipped-generalised/data/
# Unzip from GITHUB 
# use st_union command

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzterritorial-authority-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

TA2018 <- st_read(dsn = "Data/territorial-authority-2018-clipped-generalised.gpkg")

#static version
tmap_mode("plot")
tm_shape(TA2018) + 
  tm_polygons(col="seagreen2", id="TA2018_V1_00_NAME", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "Territorial Authorities, 2018, New Zealand", 
            main.title.size = 1) 


#interactive version
tmap_mode("view")
tm_shape(TA2018) + 
  tm_polygons(col="seagreen2", id="TA2018_V1_00_NAME", border.col = "grey20") 

# create an NZ coast (unified TAs)
NZ_union <- st_union (TA2018)
plot(st_geometry(NZ_union))

#static version
tmap_mode("plot")
tm_shape(NZ_union) +
  tm_polygons(col="seagreen2", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell \n use st_union") +
  tm_layout(main.title = "NZ Coastline, 2018, New Zealand", 
            main.title.size = 1) 

#interactive version
tmap_mode("view")
tm_shape(NZ_union) +
  tm_polygons(col="seagreen2", border.col = "grey20") 

#END