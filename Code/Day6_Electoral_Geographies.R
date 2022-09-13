# CENSUS MAP A DAY
# DAY 6 - Electoral Geography - September 2022

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

# LOAD in the SA2 file
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/statsnzstatistical-area-2-2018-clipped-generalised-GPKG.zip")
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./Data")

SA2 <- st_read(dsn = "Data/statistical-area-2-2018-clipped-generalised.gpkg")

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
GA2018_CON <- 
  GA2018 %>%
  select(SA22018_code, CON2018_code, CON2018_name, 
         MCON2018_code, MCON2018_name,
         GED2014_code, GED2014_name,
         GED2014_code, MED2014_name) %>%
  distinct()

glimpse(GA2018_CON)

# join back to a master file
NZ <- inner_join(SA2, GA2018_CON, by = c ( "SA22018_V1_00" = "SA22018_code" ))

# constituency
CON <- aggregate(x = NZ[, "AREA_SQ_KM"], 
                 by = list(NZ$CON2018_name), FUN = sum, na.rm = TRUE)
plot(st_geometry(CON))

# Maori Constituency
MCON <- aggregate(x = NZ[, "AREA_SQ_KM"], 
                 by = list(NZ$MCON2018_name), FUN = sum, na.rm = TRUE)
plot(st_geometry(MCON))

# General Electoral District (GED)
GED <- aggregate(x = NZ[, "AREA_SQ_KM"], 
                  by = list(NZ$GED2014_name), FUN = sum, na.rm = TRUE)
plot(st_geometry(GED))
GED <- GED %>%
  mutate(GED2014_name = Group.1)%>%
  select(-Group.1)
unique(GED$GED2014_name)

# Maori Electoral District (GED)
MED <- aggregate(x = NZ[, "AREA_SQ_KM"], 
                  by = list(NZ$MED2014_name), FUN = sum, na.rm = TRUE)
plot(st_geometry(MED))
MED <- MED %>%
  mutate(MED2014_name = Group.1)%>%
  select(-Group.1)

#static version
GED_map <- tm_shape(GED) +
  tm_polygons(col="yellow", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "General Electorates, 2014, New Zealand", 
            main.title.size = 1) 
GED_map
#static version
MED_map <- tm_shape(MED) +
  tm_polygons(col="MED2014_name", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "M\u101ori Electorates, 2014, New Zealand", 
            main.title.size = 1)
MED_map

#basic 2 panel map
tmap_arrange(GED_map, MED_map)

#########################################################
# BONUS - ADD the MPs

# download from web - list of MPs
url <- c("https://catalogue.data.govt.nz/datastore/dump/89069a40-abcf-4190-9665-3513ff004dd8?bom=True")
MPS <- read.csv(url, fileEncoding = 'UTF-8-BOM')
MPS
# take out list MPs
MPS <- MPS %>% 
  filter(Job.Title!="List Member")
#END
