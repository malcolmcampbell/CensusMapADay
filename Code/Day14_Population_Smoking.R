
# CENSUS MAP A DAY
# DAY 14 Health: Smokers - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# merge each geog separately

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

# LOAD in the RAW CENSUS data
# https://www.stats.govt.nz/information-releases/statistical-area-1-dataset-for-2018-census-updated-march-2020#total
url <- c("https://www3.stats.govt.nz/2018census/SA1Dataset/Statistical%20Area%201%20dataset%20for%20Census%202018%20-%20total%20New%20Zealand%20-%20CSV_updated_16-7-20.zip?_ga=2.239752323.1673071126.1663024898-530005258.1655210645")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

Part2 <- read_csv("./Data/Individual_part2_totalNZ-wide_format_updated_16-7-20.csv", 
                  na = c("...","C"))
Part2$Area_code <- as.numeric(as.character(Part2$Area_code))
# Take out population data using "contains" command
Part2_smoke <- Part2 %>% 
  select(Area_code_and_description, Area_code,Area_description, 
         contains("smoking") )
rm(Part2)

names(Part2_smoke)

# LOAD in the TA file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzterritorial-authority-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")
TA <- st_read(dsn = "Data/territorial-authority-2018-clipped-generalised.gpkg")
TA$TA2018_V1_00 <- as.numeric(as.character(TA$TA2018_V1_00))

#static version
tm_shape(TA) + 
  tm_polygons() 

# join back to a master file
TA <- left_join(TA, Part2_smoke, by = c ( "TA2018_V1_00"="Area_code" ))
glimpse(TA)

# create **new** change variables 2018
TA <- TA %>% 
  mutate(
    Census_2006_Smoking_pc = c((
      Census_2006_Smoking_status_01_Regular_smoker_CURP_15years_and_over/
        Census_2006_Smoking_status_Total_stated_CURP_15years_and_over)*100),
    
    Census_2013_Smoking_pc = c((
      Census_2013_Smoking_status_01_Regular_smoker_CURP_15years_and_over/
        Census_2013_Smoking_status_Total_stated_CURP_15years_and_over)*100),
    
    Census_2018_Smoking_pc = c((
      Census_2018_Smoking_status_01_Regular_smoker_CURP_15years_and_over/
        Census_2018_Smoking_status_Total_stated_CURP_15years_and_over)*100)
  )
glimpse(TA)

TA <- TA %>% 
  mutate(
    Census_2006_13_PC_chg = c(Census_2013_Smoking_pc - Census_2006_Smoking_pc) ,
    Census_2013_18_PC_chg = c(Census_2018_Smoking_pc -  Census_2013_Smoking_pc) 
  )
glimpse(TA)
#static version
tmap_mode("plot")

Census_2018_Smoking_pc_map_TA <- tm_shape(TA) +
  tm_polygons(col="Census_2018_Smoking_pc", 
              title="Regular Smoking, %, 2018",
              border.col = "grey20", n=10,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Reds") +  
  tm_layout(main.title = "Regular Smoking (Total Stated), %, 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Census_2018_Smoking_pc_map_TA

Census_2018_Smoking_pc_chng_map_TA <- tm_shape(TA) +
  tm_polygons(col="Census_2013_18_PC_chg", 
              title="Regular Smoking, % (total stated) change, 2013 to 2018",
              border.col = "grey20", n=10,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="-Spectral") +  
  tm_layout(main.title = "Regular Smoking, % change, 2013 to 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Census_2018_Smoking_pc_chng_map_TA

#END