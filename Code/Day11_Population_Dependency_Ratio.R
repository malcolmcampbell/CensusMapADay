# CENSUS MAP A DAY
# DAY 11 - Dependency Ratio - September 2022
# https://www.who.int/data/gho/indicator-metadata-registry/imr-details/1119

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# Create TA population data from SA2 using a concordance and PArt 1 census

# Definition:
# The average number of economically dependent population per 100 economically productive population, 
# for a given ... geographic area, at a specific point in time. 
# In demographic terms, economically dependent population is defined as the sum of the population 
# under 15 years of age plus the population 65 years of age 
# economically productive population is defined as the population between 15 and 64 years of age, 
#...

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

# create **new** change variables 2018
TA <- TA %>% 
  mutate(Under15_2013_PC = 
           c(Census_2013_Age_broad_groups_1_Under_15_years_CURP/
               Census_2013_Age_broad_groups_Total_CURP)*100,
         Age15to29_2013_PC = 
           c(Census_2013_Age_broad_groups_2_15_to_29_years_CURP/
               Census_2013_Age_broad_groups_Total_CURP)*100,
         Age30to64_2013_PC = 
           c(Census_2013_Age_broad_groups_3_30_to_64_years_CURP/
               Census_2013_Age_broad_groups_Total_CURP)*100,
         Over65_2013_PC = 
           c(Census_2013_Age_broad_groups_4_65_years_and_over_CURP/
               Census_2013_Age_broad_groups_Total_CURP)*100)
glimpse(TA)

# create **new** change variables 2018
TA <- TA %>% 
  mutate(Under15_2018_PC = 
           c(Census_2018_Age_broad_groups_1_Under_15_years_CURP/
               Census_2018_Age_broad_groups_Total_CURP)*100,
         Age15to29_2018_PC = 
           c(Census_2018_Age_broad_groups_2_15_to_29_years_CURP/
               Census_2018_Age_broad_groups_Total_CURP)*100,
         Age30to64_2018_PC = 
           c(Census_2018_Age_broad_groups_3_30_to_64_years_CURP/
               Census_2018_Age_broad_groups_Total_CURP)*100,
         Over65_2018_PC = 
           c(Census_2018_Age_broad_groups_4_65_years_and_over_CURP/
               Census_2018_Age_broad_groups_Total_CURP)*100)
glimpse(TA)



# create **new** DEPENDENCY variables 2018
TA <- TA %>% 
  mutate(
    Dependency_2013_PC = c((Under15_2013_PC +  Over65_2013_PC)
                           / (Age15to29_2013_PC + Age30to64_2013_PC) *100),
      
    Dependency_2018_PC = c((Under15_2018_PC +  Over65_2018_PC)
                           / (Age15to29_2018_PC + Age30to64_2018_PC) *100),
    
    Change_2013_18_Dependency = Dependency_2018_PC - Dependency_2013_PC
  )
glimpse(TA)

#static version
tmap_mode("plot")

Dependency_2013_map <- tm_shape(TA) +
  tm_polygons(col="Dependency_2013_PC", 
              title="Dependency_2013_PC",
              border.col = "grey20",
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Greens") +  
  tm_layout(main.title = "Dependency_2013_PC, %, 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Dependency_2013_map

Dependency_2018_map <- tm_shape(TA) +
  tm_polygons(col="Dependency_2018_PC", 
              title="Dependency_2018_PC",
              border.col = "grey20",
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Greens") +  
  tm_layout(main.title = "Dependency_2018_PC, %, 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Dependency_2018_map

Change_2013_18_Dependency_map  <- tm_shape(TA) +
  tm_polygons(col="Change_2013_18_Dependency", 
              title="Change in Dependency Ratio %",
              border.col = "grey20",
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="-Spectral") +  
  tm_layout(main.title = "Change in Dependency Ratio, %, 2013-18, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Change_2013_18_Dependency_map 

# 3 in 1
tmap_arrange(Dependency_2013_map,
             Dependency_2018_map,
  Change_2013_18_Dependency_map)
#END