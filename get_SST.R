#Retrieving SST data for Jigging events
# 
# install.packages("ncdf4", dependencies = TRUE) 
# install.packages("parsedate", dependencies = TRUE)
# install.packages("rerddap", dependencies = TRUE)
# install.packages("sp")
# 
# install.packages("devtools")
# devtools::install_github("r-spatial/sf")
# devtools::install_github('ropensci/plotdap')
# devtools::install_github("rmendels/rerddapXtracto")
# install.packages("tidyverse")
# install.packages("httr", dependencies = TRUE)
# install.packages("xtractomatic")
library(xtractomatic)
library(rerddapXtracto)
library(ggplot2)
library(plotdap)
library(sf)
library(tidyverse)
require(rerddap)
require(rerddapXtracto)
require(xtractomatic)

# read data
squid <- read.csv("Dosidicus_Diet_FINAL_Portner_only.csv")

diet.comp <- squid[,c(1, 3:11,13, 19:40)]
diet.comp$capture_lon <- ifelse(diet.comp$capture_lon > 1, diet.comp$capture_lon*-1, diet.comp$capture_lon)
#Remove samples fro "Ensenada"
I <- which(diet.comp$capture_lat>30)
diet.comp <- diet.comp[-I,]
#REmove samples from IMECOCAL
I <- which(diet.comp$capture_lat==25.5767)
diet.comp <- diet.comp[-I,]
#REmove samples from Volcan Marias
I <- which(diet.comp$capture_lon>(-108))
diet.comp <- diet.comp[-I,]
I <- which(diet.comp$locality=="OFF MAGDALENA")
diet.comp <- diet.comp[-I,]


getsst <- diet.comp[,c(1,3,4,11)]
getsst$date <- as.character(getsst$date)
getsst$date <- as.Date(getsst$date, format = '%Y-%m-%d')

getsstMODIS <- getsst %>%
  filter(!is.na(capture_lat))%>%
  filter(date>"2002-07-05")

xpos <- getsstMODIS$capture_lon
ypos <- getsstMODIS$capture_lat
tpos <- getsstMODIS$date


urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'

gocSSTmodis8dInfo <- rerddap::info('erdMWsstd8day')
gocSSTmodis8d <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "erdMWsstd8day", xlen=.2, ylen=.2)

gocSSTmodis3dInfo <- rerddap::info('erdMWsstd3day')
gocSSTmodis3d <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "erdMWsstd3day", xlen=.2, ylen=.2)

gocSSTmodis1dInfo <- rerddap::info('erdMWsstd1day')
gocSSTmodis1d <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "erdMWsstd1day", xlen=.2, ylen=.2)


write.csv(gocSSTmodis8d, file="GOC_SST_MODIS_2002_8d.csv")
write.csv(gocSSTmodis3d, file="GOC_SST_MODIS_2002_3d.csv")
write.csv(gocSSTmodis1d, file="GOC_SST_MODIS_2002_1d.csv")


getsstPATHFINDER <- getsst %>%
  filter(!is.na(capture_lat))%>%
  filter(date<"2002-07-05")

xpos <- getsstPATHFINDER$capture_lon
ypos <- getsstPATHFINDER$capture_lat
tpos <- getsstPATHFINDER$date

urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
gocSSTpathfinderInfo <- rerddap::info('nodcPH2sstd1day')
gocSSTpathfinder <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "nodcPH2sstd1day", xlen=1, ylen=1)

write.csv(gocSSTpathfinder, file="GOC_SST_Pathfinder_1d_1x1degree.csv")

acoustics <- read.csv("prelim_satellitedata_extract_acousticmetrics copy.csv")
# acoustics$lon <- acoustics$Lon_M+360

getsst <- acoustics[,c(2:5)]
getsst$date <- as.character(getsst$Date_M)
getsst$date <- as.Date(getsst$date, format = '%Y%m%d')
getsst$date <- as.Date(getsst$date, format = '%Y-%m-%d')
getsst$date <- as.Date(getsst$date)
getsst$year <- substr(getsst$Date_M, 1, 4)

getsstMODIS1 <- getsst %>%
  filter(year=="2005")


xpos <- getsstMODIS1$Lon_M
ypos <- getsstMODIS1$Lat_M
tpos <- getsstMODIS1$date


urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'

gocSSTmodis3dInfo <- rerddap::info('erdMWsstd3day')
gocSSTmodis3d <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "erdMWsstd3day", xlen=.2, ylen=.2)

write.csv(gocSSTmodis3d, file="GOC_acoustics_CAPE01_SST_MODIS_2002_3d.csv")
write.csv(gocSSTmodis3d, file="GOC_acoustics_CAPE02and03_SST_MODIS_2002_3d.csv")



getchlMODIS1 <- getsst %>%
  filter(year=="2017")


xpos <- getchlMODIS1$Lon_M
ypos <- getchlMODIS1$Lat_M
tpos <- getchlMODIS1$date


urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'

gocCHLmodis3dInfo <- rerddap::info('erdMWchla3day')
gocCHLmodis3d <- xtracto(xpos = xpos, ypos = ypos, tpos= tpos, "erdMWchla3day", xlen=.2, ylen=.2)

