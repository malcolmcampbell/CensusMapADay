# CENSUS MAP A DAY
# DAY 8 - Population Change - September 2022

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
  select(Area_code_and_description, Area_code,Area_description, contains("population"))
rm(Part1)

# add a concordance / annual areas file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzgeographic-areas-file-2018-CSV.zip")

download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

GA2018 <- read_csv(file = "./Data/geographic-areas-file-2018.csv")

#using filters on the data.
GA2018 <- 
  GA2018 %>%
  select(SA22018_code, SA22018_name, TA2018_code, TA2018_name) %>%
  distinct()

glimpse(GA2018)

# join back to a master file
NZ <- left_join(Part1_pop, GA2018, by = c ( "Area_code" = "SA22018_code" ))
rm(Part1_pop, GA2018)

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

# spatial aggregation
TA <- aggregate(x = NZ[,8:15], 
                 by = list(NZ$TA2018_name), FUN = sum, na.rm = TRUE)
class(TA)
plot(st_geometry(TA))

# create **new** change variables
TA <- TA %>% 
  mutate(Pop_Change_2013_18 = 
           c(Census_2018_usually_resident_population_count - 
               Census_2013_usually_resident_population_count),
         Pop_Change_2013_18_PC = 
           c((Census_2018_usually_resident_population_count - 
                Census_2013_usually_resident_population_count)/
               Census_2013_usually_resident_population_count)*100)

#choose some colours
tmaptools::palette_explorer()

#static version
NZ_pop_map <- tm_shape(TA) +
  tm_polygons(col="Pop_Change_2013_18_PC", 
              title="% change",
              border.col = "grey20",
              style="cont", lwd=0.0001, 
              midpoint = NA, palette="-Spectral") +  
  tm_layout(main.title = "Population Change, %, 2013 to 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
NZ_pop_map

#END