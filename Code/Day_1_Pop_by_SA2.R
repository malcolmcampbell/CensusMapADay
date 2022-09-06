# CENSUS MAP A DAY

# DAY 1 - September 2022

# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census usually resident population and age groups by Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/103902-2018-census-usually-resident-population-and-age-groups-by-statistical-area-2/

# Unzip from GITHUB and then read in KML file using ?gzcon - (De)compress I/O Through Connections

library(tidyverse, sf, tmap, downloader)

url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnz2018-census-usually-resident-population-and-age-groups-by-te-GPKG.zip")

download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

TALB <- st_read(dsn = 
          "Data/2018-census-usually-resident-population-and-age-groups-by-te.gpkg")

tm_shape(TALB) + 
  tm_polygons(col = "Pop_Total_2018", style = "cont", 
              n = 10, palette = "Greens", 
              border.col = "grey95",
              title = "Census usually resident population 2018")
