# CENSUS MAP A DAY
# DAY 15 Home: Tenure - September 2022

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

Census_2006_Individual_home_ownership_03_Do_not_own_and_do_not_hold_in_a_family_trust_CURP_15years_and_over  

Part2 <- read_csv("./Data/Individual_part2_totalNZ-wide_format_updated_16-7-20.csv", 
                  na = c("...","C"))
Part2$Area_code <- as.numeric(as.character(Part2$Area_code))
# Take out population data using "contains" command
Part2_home <- Part2 %>% 
  select(Area_code_and_description, Area_code,Area_description, 
         contains("home_ownership") )
rm(Part2)

names(Part2_home)

# LOAD in the SA2 file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")#
#
SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")
#
SA2$SA22018_V1_00 <- as.numeric(as.character(SA2$SA22018_V1_00))

#static version
tm_shape(SA2) + 
  tm_polygons() 

# join back to a master file
SA2 <- left_join(SA2, Part2_home, by = c ( "SA22018_V1_00"="Area_code" ))
glimpse(SA2)

# create **new** change variables 2018
SA2 <- SA2 %>% 
  mutate(
    Census_2006_non_own_pc = c((
      Census_2006_Individual_home_ownership_03_Do_not_own_and_do_not_hold_in_a_family_trust_CURP_15years_and_over/
        Census_2006_Individual_home_ownership_Total_stated_CURP_15years_and_over)*100),
    
    Census_2013_non_own_pc = c((
      Census_2013_Individual_home_ownership_03_Do_not_own_and_do_not_hold_in_a_family_trust_CURP_15years_and_over /
        Census_2013_Individual_home_ownership_Total_stated_CURP_15years_and_over)*100),
    
    Census_2018_non_own_pc = c((
      Census_2018_Individual_home_ownership_03_Do_not_own_and_do_not_hold_in_a_family_trust_CURP_15years_and_over/
        Census_2018_Individual_home_ownership_Total_stated_CURP_15years_and_over)*100),
    
    Census_2018_own_pc = c((
      (Census_2018_Individual_home_ownership_01_Hold_in_a_family_trust_CURP_15years_and_over +
        Census_2018_Individual_home_ownership_02_Own_or_partly_own_CURP_15years_and_over) /
        Census_2018_Individual_home_ownership_Total_stated_CURP_15years_and_over)*100)
    
  )
glimpse(SA2)
head(SA2)

SA2 <- SA2 %>% 
  mutate(
    Census_2006_13_PC_chg = c(Census_2013_non_own_pc - Census_2006_non_own_pc) ,
    Census_2013_18_PC_chg = c(Census_2018_non_own_pc -  Census_2013_non_own_pc),
    Census_2006_18_PC_chg = c(Census_2018_non_own_pc -  Census_2006_non_own_pc)
  )
glimpse(SA2)
#static version
tmap_mode("plot")

Census_2018_non_own_pc_map_SA2 <- tm_shape(SA2) +
  tm_polygons(col="Census_2018_non_own_pc", 
              title="Non-home Owners, %, 2018",
              border.col = "grey20", n=10,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="-Spectral") +  
  tm_layout(main.title = "Non-home Owners, %, 2018, Statistical Area 2", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Census_2018_non_own_pc_map_SA2

Census_2018_own_pc_map_SA2 <- tm_shape(SA2) +
  tm_polygons(col="Census_2018_own_pc", 
              title="Home Owners, %, 2018",
              border.col = "grey20", n=10,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="-Spectral") +  
  tm_layout(main.title = "Home Owners, %, 2018, Statistical Area 2", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Census_2018_own_pc_map_SA2

#END