# CENSUS MAP A DAY
# DAY 3 - Geography of Auckland Territotial Authority - Super City (at 2018) - September 2022 

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# use st_union command

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")

#
SA2$SA22018_V1_00 <- as.numeric(as.character(SA2$SA22018_V1_00))

#static version
tm_shape(SA2) + 
  tm_polygons() 

# add a concordance / annual areas file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzgeographic-areas-file-2018-CSV.zip")

download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

GA2018 <- read_csv(file = "./Data/geographic-areas-file-2018.csv")

# using filters on the data. Auckland

GA2018_Auckland <- 
  GA2018 %>%
  filter(TA2018_name =="Auckland") %>%
  select(SA12018_code,SA22018_code,SA22018_name,
         UR2018_code,UR2018_name,IUR2018_code,IUR2018_name,
         TA2018_code, TA2018_name, UA2017_name, 
         REGC2018_code, REGC2018_name)

glimpse(GA2018_Auckland)

# join to SA2
Auckland <- inner_join(SA2, GA2018_Auckland, by = c ( "SA22018_V1_00" = "SA22018_code" ))
# create a union (unified Auckland)
Auckland_union <- st_union (Auckland)

#static version
tm_shape(Auckland_union) +
  tm_polygons(col="firebrick1", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "Auckland, Census 2018, New Zealand", 
            main.title.size = 1) 

#interactive version
tmap_mode("view")
tm_shape(Auckland_union) + 
  tm_polygons(col="firebrick1", border.col = "grey20", alpha = 0.25) 

#END
