# Workplaces SA2 map
# Christchurch building deprivation data
# Copyright A/Prof Malcolm Campbell 2024 Feb.
# Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

library(tidyverse)
library(beepr)
library(sf)
library(tmap)
library(downloader)


#dataSA1 <- readRDS("P:/DROPBOX/Research/Census2018Data/Census_SA1_merged.rds")
#dataSA2 <- read_csv ("P:/DROPBOX/Research/Census2018Data/SA2_workplaces_2018.csv")
dataSA2 <- read_csv ("https://raw.githubusercontent.com/malcolmcampbell/CensusMapADay/main/Data/SA2_workplaces_2018.csv")

glimpse(dataSA2)
names(dataSA2)


##############################################################################################
# OR
dataSA2 <- dataSA2 %>%
  select(-MAINTRAVELWORK, -`Main means of travel to work`, 
         -AGE, -`Age group`,
         -`SEX`, -`Sex`,
         - YEAR, -Year,
         -Flags ) #%>%
# not required for 2018 data  
#mutate( Year = recode(Year, '2001' = 'Workplace_2001', 
#                        '2006' = 'Workplace_2006', 
#                        '2013' = 'Workplace_2013') )
glimpse(dataSA2)
names(dataSA2)

# NOT NEEDED FOR 2018 data
# change to wide
#dataSA2_wide <- spread(dataSA2, Year , Value)
#glimpse(dataSA2_wide)
#names(dataSA2_wide)

#rm(dataSA2)

beep(3)

#############
# 1.C.) READING IN SAVED DATA - from "ReadShapeCensusXXXX_sf.R"
#############

url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")

#
SA2$SA22018_V1_00 <- as.numeric(as.character(SA2$SA22018_V1_00))

#static version
tm_shape(SA2) + 
  tm_polygons() 

#####################################
# JOIN DATA
#####################################

SA2_work <- left_join(x = SA2, y = dataSA2, by = c("SA22018_V1_00" = 'AREA'))

SA2tmap <- tm_shape (SA2_work) + 
  tm_polygons("Value", style="quantile", 
              title="Workplaces(n)",
              border.col = "black", lwd=0.002) +
  
  tm_layout (main.title="New Zealand, Workplace data, SA2, 2018") +
  tm_scale_bar(width=0.15, position=c("right","top")) +
  tm_compass(position=c("right","top"), type="rose", size=4) +
  tm_credits(text ="Source: Statistics New Zealand 
             \n Census 2018
             \n Created by A.Prof Malcolm Campbell") +
  tm_legend(frame=T)

SA2tmap

beep(3)

tmap_save(tm=SA2tmap, filename="SA2_Workplaces_2018.png", dpi=300, width=1800, height=2000)

tmap_mode("view")
SA2tmap

################# BONUS CARTOGRAM!
# cartogram workplace
#library(cartogram)
#workplace_cartogram <- cartogram_cont ( SA2_work, "Value", itermax = 10)
#tmap_mode("plot")
#SA2tmap <- tm_shape (workplace_cartogram) + 
#  tm_polygons("Value", style="quantile", 
#              title="Workplaces(n)",
#             border.col = "black", lwd=0.002) +
#  
# tm_layout (main.title="New Zealand Cartogram, Workplace data, SA2, 2018") +
# tm_scale_bar(width=0.15, position=c("right","top")) +
# tm_compass(position=c("right","top"), type="rose", size=4) +
# tm_credits(text ="Source: Statistics New Zealand 
#            \n Census 2018
#             \n Created by A.Prof Malcolm Campbell") +
#  tm_legend(frame=T)
#
#SA2tmapcarto
#
#tmap_save(tm=SA2tmapcarto, filename="SA2_Workplaces_2018_carto.png", dpi=300, width=1800, height=2000)
#
# END