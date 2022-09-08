# CENSUS MAP A DAY
# DAY 5 - DHB (at 2015) 20 to 4 HEALTH NZ - September 2022
# remake DHB and HNZ region boundaries
# Associate Professor Malcolm Campbell
# Attribution-Non Commercial-ShareAlike  # CC BY-NC-SA 

# 2018 Census Statistical Area 2
# source STATSNZ
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/
# Unzip from GITHUB 
# Create DHB from SA2 using a concordance and Health NZ regions
# https://www.google.com/url?sa=t&rct=j&q=&esrc

##############################################
# SHORT VERSION
library(tmap)
library(sf)
url <- c("https://github.com/malcolmcampbell/CensusMapADay/raw/main/Data/HNZ_regions.gpkg")
HNZ_regions <- st_read(url)

#static map version
tmap_mode("plot")
tm_shape(HNZ_regions) +
  tm_polygons(col="HNZ_region", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n 
             Created by A.Prof Malcolm Campbell \n
             using aggregate and st_union") +
  tm_layout(main.title = "Te Whatu Ora | Health New Zealand Regions, New Zealand", 
            main.title.size = 1)
# clear out 
rm(list=ls())
# END
################################################

##############################################
# LONG VERSION
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
GA2018_DHB <- 
  GA2018 %>%
  select(SA22018_code,DHB2015_code, DHB2015_name) %>%
  distinct()

glimpse(GA2018_DHB)

# join back to a master file
NZ <- inner_join(SA2, GA2018_DHB, by = c ( "SA22018_V1_00" = "SA22018_code" ))

# a brutal step by step SA2 to HEALTH NZ regions
# create vectors of new regions from old DHB names
NORTHERN <- c("Northland", "Waitemata", "Counties Manukau", "Auckland")
MIDLANDS <- c("Waikato", "Lakes", "Taranaki", "Bay of Plenty", "Tairawhiti")
CENTRAL <- c("Whanganui", "MidCentral", "Hutt Valley", "Wairarapa", "Hawke's Bay", "Capital and Coast")
SOUTHERN <- c("Nelson Marlborough", "West Coast", "Southern", "South Canterbury", "Canterbury")

DHB <- aggregate(x = NZ[, "AREA_SQ_KM"], 
                 by = list(NZ$DHB2015_name), FUN = sum, na.rm = TRUE)
plot(st_geometry(DHB))

#static version
tm_shape(DHB) +
  tm_polygons(col="DHB2015_name", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n Created by A.Prof Malcolm Campbell") +
  tm_layout(main.title = "District Health Boards, New Zealand", 
            main.title.size = 1) 


DHB <- DHB %>%
  mutate(DHB2015_name = Group.1)%>%
  select(-Group.1)


#############################
# BONUS HEALTH REGIONS #
##############################
DHB <-   DHB %>%
  mutate(HNZ_code = case_when(
    DHB2015_name %in% NORTHERN ~ "NORTHERN", grepl(paste(NORTHERN, collapse="|"), DHB2015_name) ~ "NORTHERN",
    DHB2015_name %in% MIDLANDS ~ "MIDLANDS", grepl(paste(MIDLANDS, collapse="|"), DHB2015_name) ~ "MIDLANDS",
    DHB2015_name %in% CENTRAL ~ "CENTRAL", grepl(paste(CENTRAL, collapse="|"), DHB2015_name) ~ "CENTRAL",
    DHB2015_name %in% SOUTHERN ~ "SOUTHERN", grepl(paste(SOUTHERN, collapse="|"), DHB2015_name) ~ "SOUTHERN",
    T ~ "TBC"))
glimpse(DHB)

DHB <- DHB %>%
  filter(HNZ_code!="TBC")

HNZ_regions <- aggregate(x = DHB[, "AREA_SQ_KM"], 
                         by = list(DHB$HNZ_code), FUN = sum, na.rm = TRUE)

# renaming
HNZ_regions <- HNZ_regions %>%
  mutate(HNZ_region = Group.1) %>%
  select(-Group.1)
glimpse(HNZ_regions)

#static version
tmap_mode("plot")
tm_shape(HNZ_regions) +
  tm_polygons(col="HNZ_region", border.col = "grey20") +  
  tm_credits(text ="Source: Statistics New Zealand \n 
             Created by A.Prof Malcolm Campbell \n
             using aggregate and st_union") +
  tm_layout(main.title = "Te Whatu Ora | Health New Zealand Regions, New Zealand", 
            main.title.size = 1) 

###############################################################
# creation of individual Health NZ regions if needed?
HNZ_NORTHERN <- NZ[NZ$DHB2015_name %in% NORTHERN, ]
HNZ_NORTHERN_union <- st_union ( HNZ_NORTHERN )
plot( st_geometry(HNZ_NORTHERN_union), col="dodgerblue2" )

HNZ_MIDLANDS <- NZ[NZ$DHB2015_name %in% MIDLANDS, ]
HNZ_MIDLANDS_union <- st_union ( HNZ_MIDLANDS )
plot( st_geometry(HNZ_MIDLANDS_union), col="magenta" )

HNZ_CENTRAL <- NZ[NZ$DHB2015_name %in% CENTRAL, ]
HNZ_CENTRAL_union <- st_union ( HNZ_CENTRAL )
plot( st_geometry(HNZ_CENTRAL_union), col="firebrick1" )

HNZ_SOUTHERN <- NZ[NZ$DHB2015_name %in% SOUTHERN, ]
HNZ_SOUTHERN_union <- st_union ( HNZ_SOUTHERN )
plot( st_geometry(HNZ_SOUTHERN_union), col="seagreen2" )

#############################################################
 
#END
