# CENSUS MAP A DAY
# DAY 2 - Geography of Timaru Urban Area (at 2017) - September 2022

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

#using filters on the data.
# Timaru 

GA2018_Timaru <- 
  GA2018 %>%
  filter(UA2017_name =="Timaru") %>%
  select(SA12018_code,SA22018_code,SA22018_name,
         UR2018_code,UR2018_name,IUR2018_code,IUR2018_name,
         TA2018_code, TA2018_name, UA2017_name)

glimpse(GA2018_Timaru)


#END
  
  
Timaru <- inner_join(SA2, GA2018_Timaru, by = c ( "SA22018_V1_00" = "SA22018_code" ))

#static version
tm_shape(Timaru) +
tm_polygons(col="dodgerblue2", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "Timaru, Census 2018, New Zealand", 
            main.title.size = 1) 
  
#interactive version
tmap_mode("view")
tm_shape(Timaru) + 
  tm_polygons(col="dodgerblue2", border.col = "grey20", alpha = 0.25, 
              id = "SA22018_V1_NAME",
              popup.vars = c("SA22018_V1_00_NAME", "LAND_AREA_SQ_KM")) 
