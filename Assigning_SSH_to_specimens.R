library(raster)
library(ncdf4)


### Open and compile ssh data from CMEMS for hammers

library (ncdf4)
library (tidyverse)
library (lubridate)


# open sample file to check variables
ssh_nc <- nc_open("global-reanalysis-phy-001-030-monthly_1527714778577.nc", write = TRUE)
print (ssh_nc)

### read prey data. "squid" is all data
squid <- read.csv("Dosidicus_Diet_FINAL_Portner_only.csv")
squid$capture_lon <- ifelse(squid$capture_lon > 1, squid$capture_lon*-1, squid$capture_lon)
squid$date  <- as.character(squid$date)
squid$date <- as.date(squid$date, format=)

squid.SSH <- squid[,c(1,3:13)]

dname <- "zos"
# get longitudegitude and latitudeitude
longitude <- ncvar_get(ssh_nc,"longitude")
nlongitude <- dim(longitude)
head(longitude)

latitude <- ncvar_get(ssh_nc,"latitude")
nlatitude <- dim(latitude)
head(latitude)

print(c(nlongitude,nlatitude))

# get time
time <- ncvar_get(ssh_nc,"time")
time <- time/24

tunits <- ncatt_get(ssh_nc,"time","units")
nt <- dim(time)
nt

tunits

# get temperature
ssh_array <- ncvar_get(ssh_nc,dname)
dlname <- ncatt_get(ssh_nc,dname,"longitudeg_name")
dunits <- ncatt_get(ssh_nc,dname,"units")
fillvalue <- ncatt_get(ssh_nc,dname,"_FillValue")
dim(ssh_array)

nc_close(ssh_nc)

# load some packages
library(chron)
library(latitudetice)
library(RColorBrewer)


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(date =time,origin=c(tmonth, tday, tyear))

# replace netCDF fill values with NA's
ssh_array[ssh_array==fillvalue$value] <- NA

# get a single slice or layer (January)
m <- 236
ssh_slice <- ssh_array[,,m]

image(longitude,latitude,ssh_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=longitude, lat=latitude)
cutpts <- c(0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6)
levelplot(ssh_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(11,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(longitude,latitude))
dim(lonlat)

ssh_vec <- as.vector(ssh_slice)
length(ssh_vec)

# create dataframe and add names
ssh_df01 <- data.frame(cbind(lonlat,ssh_vec))
names(ssh_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(ssh_df01), 10)

# reshape the array into vector
ssh_vec_long <- as.vector(ssh_array)
length(ssh_vec_long)

ssh_mat <- matrix(ssh_vec_long, nrow=nlongitude*nlatitude, ncol=nt)
dim(ssh_mat)

# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
ssh_wide <- data.frame(cbind(lonlat,ssh_mat))
names(ssh_wide) <- c("lon","lat","01-1996","02-1996","03-1996","04-1996","05-1996","06-1996","07-1996","08-1996","09-1996","10-1996","11-1996","12-1996",
                     "01-1997","02-1997","03-1997","04-1997","05-1997","06-1997","07-1997","08-1997","09-1997","10-1997","11-1997","12-1997",
                     "01-1998","02-1998","03-1998","04-1998","05-1998","06-1998","07-1998","08-1998","09-1998","10-1998","11-1998","12-1998",
                     "01-1999","02-1999","03-1999","04-1999","05-1999","06-1999","07-1999","08-1999","09-1999","10-1999","11-1999","12-1999",
                     "01-2000","02-2000","03-2000","04-2000","05-2000","06-2000","07-2000","08-2000","09-2000","10-2000","11-2000","12-2000",
                     "01-2001","02-2001","03-2001","04-2001","05-2001","06-2001","07-2001","08-2001","09-2001","10-2001","11-2001","12-2001",
                     "01-2002","02-2002","03-2002","04-2002","05-2002","06-2002","07-2002","08-2002","09-2002","10-2002","11-2002","12-2002",
                     "01-2003","02-2003","03-2003","04-2003","05-2003","06-2003","07-2003","08-2003","09-2003","10-2003","11-2003","12-2003",
                     "01-2004","02-2004","03-2004","04-2004","05-2004","06-2004","07-2004","08-2004","09-2004","10-2004","11-2004","12-2004",
                     "01-2005","02-2005","03-2005","04-2005","05-2005","06-2005","07-2005","08-2005","09-2005","10-2005","11-2005","12-2005",
                     "01-2006","02-2006","03-2006","04-2006","05-2006","06-2006","07-2006","08-2006","09-2006","10-2006","11-2006","12-2006",
                     "01-2007","02-2007","03-2007","04-2007","05-2007","06-2007","07-2007","08-2007","09-2007","10-2007","11-2007","12-2007",
                     "01-2008","02-2008","03-2008","04-2008","05-2008","06-2008","07-2008","08-2008","09-2008","10-2008","11-2008","12-2008",
                     "01-2009","02-2009","03-2009","04-2009","05-2009","06-2009","07-2009","08-2009","09-2009","10-2009","11-2009","12-2009",
                     "01-2010","02-2010","03-2010","04-2010","05-2010","06-2010","07-2010","08-2010","09-2010","10-2010","11-2010","12-2010",
                     "01-2011","02-2011","03-2011","04-2011","05-2011","06-2011","07-2011","08-2011","09-2011","10-2011","11-2011","12-2011",
                     "01-2012","02-2012","03-2012","04-2012","05-2012","06-2012","07-2012","08-2012","09-2012","10-2012","11-2012","12-2012",
                     "01-2013","02-2013","03-2013","04-2013","05-2013","06-2013","07-2013","08-2013","09-2013","10-2013","11-2013","12-2013",
                     "01-2014","02-2014","03-2014","04-2014","05-2014","06-2014","07-2014","08-2014","09-2014","10-2014","11-2014","12-2014",
                     "01-2015","02-2015","03-2015","04-2015","05-2015","06-2015","07-2015","08-2015","09-2015","10-2015","11-2015","12-2015",
                     "01-2016","02-2016","03-2016","04-2016","05-2016","06-2016","07-2016","08-2016","09-2016","10-2016","11-2016","12-2016")

### Preparing dataframe to examine changes in quantitative volume of prey groups ###
ssh_long <- ssh_wide %>% 
  gather(key   = month_year,
         value = ssh,
         dplyr::starts_with('0'),
         dplyr::starts_with('1'))

ssh_long$month <- substr(ssh_long$month_year,1,2)
ssh_long$year <- substr(ssh_long$month_year,4,7)

ssh_long$month <- as.integer(ssh_long$month)
ssh_long$year <- as.integer(ssh_long$year)


squid.SSH$lat_deg <- as.numeric(substr(squid.SSH$capture_lat, 1, 2))
squid.SSH$lat_min <- as.numeric(substr(squid.SSH$capture_lat, 3, 8))
squid.SSH$lat_min_bin <- as.numeric(ifelse(squid.SSH$lat_min>0.958335 &squid.SSH$lat_min<0.041665, 0.00000,
                                ifelse(squid.SSH$lat_min>0.041665 &squid.SSH$lat_min<0.124995, 0.08333,
                                       ifelse(squid.SSH$lat_min>0.124995 &squid.SSH$lat_min<0.208325, 0.16667,
                                              ifelse(squid.SSH$lat_min>0.208325 &squid.SSH$lat_min<0.291655, 0.25000,
                                                     ifelse(squid.SSH$lat_min>0.291665 &squid.SSH$lat_min<0.374985, 0.33333,
                                                            ifelse(squid.SSH$lat_min>0.374985 &squid.SSH$lat_min<0.458315, 0.41667,
                                                                   ifelse(squid.SSH$lat_min>0.458315 &squid.SSH$lat_min<0.541645, 0.50000,
                                                                          ifelse(squid.SSH$lat_min>0.541645 &squid.SSH$lat_min<0.624975, 0.58333,
                                                                                 ifelse(squid.SSH$lat_min>0.624975 &squid.SSH$lat_min<0.708305, 0.66667,
                                                                                        ifelse(squid.SSH$lat_min>0.708305 &squid.SSH$lat_min<0.791635, 0.75000,
                                                                                               ifelse(squid.SSH$lat_min>0.791635 &squid.SSH$lat_min<0.874965, 0.83333,
                                                                                                      ifelse(squid.SSH$lat_min>0.874965 &squid.SSH$lat_min<0.958335, 0.91667, "")))))))))))))

squid.SSH$lat_bin <- (squid.SSH$lat_deg+squid.SSH$lat_min_bin)

squid.SSH$lon_deg <- as.numeric(substr(squid.SSH$capture_lon, 1, 4))
squid.SSH$lon_min <- as.numeric(substr(squid.SSH$capture_lon, 5, 8))
squid.SSH$lon_min_bin <- as.numeric(ifelse(squid.SSH$lon_min>0.958335 &squid.SSH$lon_min<0.041665, 0.0000,
                                           ifelse(squid.SSH$lon_min>0.041665 &squid.SSH$lon_min<0.124995, 0.0833,
                                                  ifelse(squid.SSH$lon_min>0.124995 &squid.SSH$lon_min<0.208325, 0.1667,
                                                         ifelse(squid.SSH$lon_min>0.208325 &squid.SSH$lon_min<0.291655, 0.2500,
                                                                ifelse(squid.SSH$lon_min>0.291665 &squid.SSH$lon_min<0.374985, 0.3333,
                                                                       ifelse(squid.SSH$lon_min>0.374985 &squid.SSH$lon_min<0.458315, 0.4167,
                                                                              ifelse(squid.SSH$lon_min>0.458315 &squid.SSH$lon_min<0.541645, 0.5000,
                                                                                     ifelse(squid.SSH$lon_min>0.541645 &squid.SSH$lon_min<0.624975, 0.5833,
                                                                                            ifelse(squid.SSH$lon_min>0.624975 &squid.SSH$lon_min<0.708305, 0.6667,
                                                                                                   ifelse(squid.SSH$lon_min>0.708305 &squid.SSH$lon_min<0.791635, 0.7500,
                                                                                                          ifelse(squid.SSH$lon_min>0.791635 &squid.SSH$lon_min<0.874965, 0.8333,
                                                                                                                 ifelse(squid.SSH$lon_min>0.874965 &squid.SSH$lon_min<0.958335, 0.9167, "")))))))))))))

squid.SSH$lon_bin <- (squid.SSH$lon_deg-squid.SSH$lon_min_bin)

# ssh_long$latlon <- paste(ssh_long$lat, ssh_long$lon, sep = "-")
# squid.SSH$latlon <- paste(squid.SSH$lat_bin, squid.SSH$lon_bin, sep = "-")
# squid.SSH$ssh <- ifelse(ssh_long$lat == squid.SSH$lat_bin & ssh_long$lon == squid.SSH$lon_bin & ssh_long$year == squid.SSH$capture_year &ssh_long$month == squid.SSH$capture_month, ssh_long$ssh, "")

write.csv (select (sz.PA.SSH, Lance.code, adt, sla), file = "Data_tables/Sz_CMEMS_ssh.csv", row.names = FALSE)


