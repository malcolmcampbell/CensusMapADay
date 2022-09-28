# CENSUS MAP A DAY
# DAY 13 Median Income (household/personal) - September 2022

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
Part2_inc <- Part2 %>% 
  select(Area_code_and_description, Area_code,Area_description, 
         contains("income_Median") )
rm(Part2)

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
TA <- left_join(TA, Part2_inc, by = c ( "TA2018_V1_00"="Area_code" ))
glimpse(TA)

# create **new** change variables 2018
TA <- TA %>% 
  mutate(
    income_2018_2013_diff = c(
      Census_2018_Total_personal_income_Median_CURP_15years_and_over -
      Census_2013_Total_personal_income_Median_CURP_15years_and_over),
    income_2018_2013_diff_pc = c(((
      Census_2018_Total_personal_income_Median_CURP_15years_and_over -
        Census_2013_Total_personal_income_Median_CURP_15years_and_over)/
      Census_2013_Total_personal_income_Median_CURP_15years_and_over)*100)
  )
glimpse(TA)

#static version
tmap_mode("plot")

Personal_income_2013_map_TA <- tm_shape(TA) +
  tm_polygons(col="Census_2013_Total_personal_income_Median_CURP_15years_and_over", 
              title="Personal Income ($)",
              border.col = "grey20", n=5,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Greens") +  
  tm_layout(main.title = "Personal Income ($), 2013, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Personal_income_2013_map_TA

Personal_income_2018_map_TA <- tm_shape(TA) +
  tm_polygons(col="Census_2018_Total_personal_income_Median_CURP_15years_and_over", 
              title="Personal Income ($)",
              border.col = "grey20", n=5,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Greens") +  
  tm_layout(main.title = "Personal Income ($), 2018, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Personal_income_2018_map_TA

Personal_income_chng_2018_map_TA <- tm_shape(TA) +
  tm_polygons(col="income_2018_2013_diff", 
              title="Personal Income ($), Change",
              border.col = "grey20", n=5,
              style="quantile", lwd=0.0001, 
              midpoint = NA, palette="Greens") +  
  tm_layout(main.title = "Personal Income ($), Change, 2013 - 18, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Personal_income_chng_2018_map_TA

Personal_income_chng_PC_2018_map_TA <- tm_shape(TA) +
  tm_polygons(col="income_2018_2013_diff_pc", 
              title="Personal Income ($), % Change",
              border.col = "grey20", n=4,
              style="equal", lwd=0.0001, 
              midpoint = 0, palette="Spectral") +  
  tm_layout(main.title = "Personal Income ($), % Change, 2013 - 18, Territorial Authorities", 
            main.title.size = 1, legend.hist.width = 1) +
  tm_compass(position = "right", color.light = "grey90") + 
  tm_scale_bar(position=c("right")) +
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") 
Personal_income_chng_PC_2018_map_TA

tmap_arrange(
  Personal_income_2013_map_TA,Personal_income_2018_map_TA,
  Personal_income_chng_2018_map_TA,  Personal_income_chng_PC_2018_map_TA
)
#END