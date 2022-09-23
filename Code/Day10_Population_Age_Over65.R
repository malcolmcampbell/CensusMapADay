# CENSUS MAP A DAY
# DAY 9 - Children Change - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# Create TA population data from SA2 using a concordance and PArt 1 census

library(tidyverse)
library(sf)
library(tmap) 
library(downloader)

# LOAD in the RAW CENSUS data
# https://www.stats.govt.nz/information-releases/statistical-area-1-dataset-for-2018-census-updated-march-2020#total
url <- c("https://www3.stats.govt.nz/2018census/SA1Dataset/Statistical%20Area%201%20dataset%20for%20Census%202018%20-%20total%20New%20Zealand%20-%20CSV_updated_16-7-20.zip?_ga=2.239752323.1673071126.1663024898-530005258.1655210645")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

Part1 <- read_csv("./Data/Individual_part1_totalNZ-wide_format_updated_16-7-20.csv", 
                  na = c("...","C"))
Part1$Area_code <- as.numeric(as.character(Part1$Area_code))
# Take out population data using "contains" command
Part1_pop <- Part1 %>% 
  select(Area_code_and_description, Area_code,Area_description, 
         contains("broad_groups") )
rm(Part1)

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
TA <- left_join(TA, Part1_pop, by = c ( "TA2018_V1_00"="Area_code" ))
glimpse(TA)

# create **new** change variables
TA <- TA %>% 
  mutate(Over65_2018_PC = 
           c(Census_2018_Age_broad_groups_4_65_years_and_over_CURP/
               Census_2018_Age_broad_groups_Total_CURP)*100)
glimpse(TA)

tmap_mode("plot")

#static version

Over65_2018_PC_map <- tm_shape(TA) +
  tm_polygons(col="Over65_2018_PC", 
              title="Over 65 years old",
              border.col = "grey20",
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Blues") +  
  tm_layout(main.title = "Over 65, %, 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Over65_2018_PC_map

#END