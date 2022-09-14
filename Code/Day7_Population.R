# CENSUS MAP A DAY
# DAY 7 - Population - September 2022

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

# LOAD in the RAW CENSUS data
# https://www.stats.govt.nz/information-releases/statistical-area-1-dataset-for-2018-census-updated-march-2020#total
url <- c("https://www3.stats.govt.nz/2018census/SA1Dataset/Statistical%20Area%201%20dataset%20for%20Census%202018%20-%20total%20New%20Zealand%20-%20CSV_updated_16-7-20.zip?_ga=2.239752323.1673071126.1663024898-530005258.1655210645")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

Part1 <- read_csv("./Data/Individual_part1_totalNZ-wide_format_updated_16-7-20.csv", 
                      na = c("...","C"))
Part1$Area_code <- as.numeric(as.character(Part1$Area_code))
# Take out 2018 CENSUS data using "starts_with" command
Part1_2018 <- Part1 %>% 
  select(Area_code_and_description, Area_code,Area_description, starts_with("Census_2018"))
rm(Part1)

# add a concordance / annual areas file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzgeographic-areas-file-2018-CSV.zip")

download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

GA2018 <- read_csv(file = "./Data/geographic-areas-file-2018.csv")

#using filters on the data.
GA2018 <- 
  GA2018 %>%
  select(SA22018_code, SA22018_name) %>%
  distinct()

glimpse(GA2018)

# slim down to just population
# Take out 2018 CENSUS data using "conatains" command
Part1_2018 <- Part1_2018 %>% 
  select(Area_code_and_description, Area_code,Area_description, contains("population"))


# join back to a master file
NZ <- left_join(Part1_2018, GA2018, by = c ( "Area_code" = "SA22018_code" ))
rm(Part1_2018, GA2018)

# LOAD in the SA2 file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")
SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")
SA2$SA22018_V1_00 <- as.numeric(as.character(SA2$SA22018_V1_00))
#static version
tm_shape(SA2) + 
  tm_polygons() 

# join back to a master file
NZ <- left_join(SA2, NZ, by = c ( "SA22018_V1_00"="Area_code" ))


#static version
tm_shape(NZ) + 
  tm_polygons() 
# constituency

#static version
NZ_pop_map <- tm_shape(NZ) +
  tm_polygons(col="Census_2018_usually_resident_population_count", 
              title="Usually resident population",
              border.col = "grey20",
              style="cont", lwd=0.0001) +  
  tm_layout(main.title = "Usually resident population, 2018, New Zealand", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
NZ_pop_map

#END