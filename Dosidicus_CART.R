# setwd
library(diet)
library(tidyr)

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


diet.comp$maturity.state <- ifelse(diet.comp$maturity.state == "3-Feb", "3",diet.comp$maturity.state)
#Add SST
sstPath <- read.csv("GOC_SST_Pathfinder_1d_1x1degree.csv")[,c(2:12)]
sstMODIS <- read.csv("GOC_SST_MODIS_2002_3d.csv")[,c(2:12)]
colnames(sstPath) <- c("mean.sst", "stdev.sst", "n", "satellite.date", "requested.lon.min","requested.lon.max","requested.lat.min","requested.lat.max", "requested.date", "median.sst", "mad.sst")
colnames(sstMODIS) <- c("mean.sst", "stdev.sst", "n", "satellite.date", "requested.lon.min","requested.lon.max","requested.lat.min","requested.lat.max", "requested.date", "median.sst", "mad.sst")

temp <- rbind(sstPath, sstMODIS)
temp$date <- substr(temp$satellite.date ,1, 10)

tempsst <- temp[,c(1,12)]
temp$date <- as.factor(temp$date)
tempsst <- unique(tempsst)

diet.comp.space <- subset(diet.comp, is.na(capture_lat))
I <- which(is.na(diet.comp$capture_lat))
diet.comp <- diet.comp[-I,]
diet.comp$date <- as.character(diet.comp$date)
temp$date1 <- as.character(temp$date1)
colnames(temp)[12] <- "date1"
diet.comp.sst <- cbind(diet.comp, temp)
diet.comp.sst$agree <- ifelse(diet.comp.sst$date == diet.comp.sst$date1, "yes", "no")

diet.comp.prep <- diet.comp.sst[,c(1:4,6:34)]

I <- which(diet.comp.prep$capture_lat<25.6)
diet.comp.prep <- diet.comp.prep[-I,]
I <- which(diet.comp.prep$capture_year == "2005")
diet.comp.prep <- diet.comp.prep[-I,]
I <- which(diet.comp.prep$capture_month >10 |diet.comp.prep$capture_month<6)
diet.comp.prep <- diet.comp.prep[-I,]
I <- which(diet.comp.prep$capture_year == "1997")
diet.comp.prep <- diet.comp.prep[-I,]
#convert to long-form data
diet.comp_long <- diet.comp.prep %>% 
  gather(key   = Group,
         value = PropN,
         dplyr::starts_with('F_'),
         dplyr::starts_with('C_'),
         dplyr::starts_with('M_'),
         dplyr::starts_with('R_'))

# squid_long$PropV <- squid_long$PropV/100
colnames(diet.comp_long) <- c("TripSetPredNo", "TripSetNo","Lat", "Lon", "Month", "Year","Sex", "Mat", "Length","Date", "SST", "Group", "Nprey")

I <- which(diet.comp_long$Nprey == 0)
diet.comp_long <- diet.comp_long[-I,]
# diet.comp_long <- diet.comp_long[,c(1:4, 6:12)]
# squid_long <- squid_long[,c(1:9, 11, 15, 16)]
#prepare unique fields for diet package
diet.comp_long$TripSetNo <- as.character(diet.comp_long$TripSetNo) 
# diet.comp_long$Month <- as.integer(diet.comp_long$Month)
# diet.comp_long$Mat <- as.factor(diet.comp_long$Mat)
# diet.comp_long$Year <- ifelse(diet.comp_long$Year == 2015, "15" , 
#                   ifelse(diet.comp_long$Year == 2016, "16", 
#                        ifelse(diet.comp_long$Year == 2017, "17", "")))
# diet.comp_long$Year <- as.integer(diet.comp_long$Year)
# diet.comp_long$Full <- diet.comp_long$Full/4

diet.comp_sum <- aggregate(Nprey~TripSetPredNo, diet.comp_long, FUN = sum)
colnames(diet.comp_sum) <- c("TripSetPredNo", "Sprey")
diet.comp_long <- merge(diet.comp_long, diet.comp_sum, by = "TripSetPredNo")
diet.comp_long$PropN <- diet.comp_long$Nprey/diet.comp_long$Sprey
diet.comp_long <- diet.comp_long[,c(1:12, 15)]
#cart <- merge(specimen, cart, by = "specimen")
cartfin <- diet.comp_long



cartfin$PredSpp <- "Dgigas"
cartfin$PredSpp <- as.factor(cartfin$PredSpp)
cartfin$SST <- as.integer(cartfin$SST)
cartfin$Group <- as.factor(cartfin$Group)
cartfin$Date <-as.POSIXct(cartfin$Date)
# cartfin$Full <- cartfin$Full*100
# cartfin$Full <- as.integer(cartfin$Full)
cartfin$Month <- as.factor(cartfin$Month)
cartfin$Mat <- as.factor(cartfin$Mat)

# Begin using diet package here

write.csv(cartfin, file = "cartfin.csv", row.names = FALSE)
cartdata <- read.pp(filenm = "cartfin.csv",  
                    labels = list(PredatorL = "TripSetPredNo", TripSetL = "TripSetNo",
                                  SpeciesL = "Group",SST="SST",
                                  DateL = "Date", WeightL = "PropN", PreyGrpL = "Group"),
                    Datef = "%m/%d/%Y", p = 0.001, Xvars = c("Lat", "Lon", "Year", "Month", "Length", "Mat", "Sex","SST"))

PreySquidSort <-read.csv("PreyTaxonSort.csv")
#data(PreyTaxonSort)
pal <- c(topo.colors(20)[1:7], heat.colors(20)[1:11], terrain.colors(20)[1:2])
val <- apc(x = cartdata, preyfile = PreySquidSort, palette = pal, check = TRUE)
node.colsY <- val$cols
dietPP <- val$x   # updated diet matrix with prey taxa codes
head(dietPP)

# Create a predator column
dietPP$Predator <- as.factor(rep("Dgigas", nrow(dietPP)))
head(dietPP)

x11()
pdf(file="explore4_noless25.6N_CART_comb.pdf")
explore.diet <- plot(x = dietPP, Xvar = c("Lat", "Lon", "Year", "Month", "Length", "Sex", "Mat", "SST"), LonID = "Lon",
                     LatID = "Lat",  mapxlim = c(-118, -105), mapylim = c(22, 35), Factor = "Predator",
                     SmXvar = c("Length", "Lat"), PredIDno = "TripSetPredNo", prey.cols = node.colsY,
                     col = "beige", cex=0.5)
dev.off()
############################################################################################
# Analysis of diet data
############################################################################################

# Fitting the model and Pruning



# Assigning prey colours for default palette
val <- apc(x = cartdata, preyfile = PreySquidSort, check = FALSE)
node.colsY <- val$cols
dietPP <- val$x   # updated diet matrix with Group assigned prey taxa codes

# Fitting the classification tree
#pdf(file="~/Documents/R/Alepisaurus/prune_comparisons/categoryn6_noBEAKS_minsplit200_minbuck100.pdf")
alep.dp <- dpart(Group ~ Lat+Lon+ Length+ Year,
                 data = dietPP, weights = W, minsplit = 100, minbucket = 50,
                 cp = 0.001)
plot(alep.dp, node.cols=node.colsY)
summary(alep.dp)
print(alep.dp, setID = "TripSetNo")
plotcp(alep.dp)
dev.off()

x11()
pdf(file="comb_CART_noless25.6N_minsplit-100_minbucket-50_NoTSN_cp001.pdf")
alep.pr <- prune(alep.dp, se = 1)
plot(alep.pr, node.cols = node.colsY)

# Variable importance ranking
vi <- importance(alep.pr)
dev.off()

# Maps of diversity
# Diversity index
mapxlim <- c(-118, -105)
mapylim <- c(22, 35)

x11()
pdf(file="diversityMAP.pdf")
D <- diversity(object = alep.pr, LatID = "Lat", LonID = "Lon", mapxlim = mapxlim, mapylim = mapylim, cex.axis = 1.1)

dev.off()
# Exploring nodes of the tree}
#The following piece of code is interactive. When the code is run, the user will be 
# asked to select a node for interrogation and exploration.
x11()
#val <- grab(object = alep.pr, LatID = "Lat", LonID = "Lon", setID = "TripSetNo",
#           node.cols = node.colsY, cex = 1, mapxlim = mapxlim, mapylim = mapylim,
#          mapcol = "gold3")
val <- grabmulti(object = alep.pr, LatID = "Lat", LonID = "Lon", setID = "TripSetNo",
                 node.cols = node.colsY, cex = 1, mapxlim = mapxlim, mapylim = mapylim,
                 mapcol = "gold3")

# Forming predictions
# predict distribution of prey composition for each predator
pdf(file="predict_categories.pdf")
alep.predator <- predict(alep.pr, type = "prob", pred.type = "predator", cex = 0.4,
                         predatorID = "TripSetPredNo")
dev.off()
# predict distribution of prey composition for each observation
# yft.pred.obs <- predict(yft.pr, type = "prob")

# predict classification  for each observation in the dataset
alep.predC <- predict(alep.pr, type = "class")   # predicted classification

# Residual analysis
alep.resid <- resids(alep.pr, LonID = "Lon", LatID = "Lat", predID = "TripSetPredNo",
                     plot = TRUE) # need to compute resids from bootstrapping
title(main = "Variogram of Residuals (CT)")

############################################################################################
# Bagging
############################################################################################

# Bagging with no spatial bootstrapping
alep.bag <- bagging(Group ~ Lat + Lon + Year + Length,
                    data = dietPP, weights = W, minsplit = 200,minbucket = 50,
                    cp = 0.001, nBaggs = 500, predID = "TripSetPredNo")
x11()
#pdf(file="~/Documents/R/Alepisaurus/prune_comparisons/SpeciesLeveln31_Variograms_of_residuals_bootstrapping_ms150mb50.pdf")
alep.bag.resid <- resids(alep.bag, LonID = "Lon", LatID = "Lat", predID = "TripSetPredNo",
                         plot = TRUE)
title(main = "Variogram of Residuals (BCT)")

# Bagging with spatial bootstrapping ngrids = 10
alep.bag.S10 <- bagging(Group ~  Lat + Lon + Year + Length,
                        data = dietPP, weights = W, minbucket = 50,
                        spatial = list(fit = TRUE, sizeofgrid = 0.5, LonID = "Lon",
                                       LatID = "Lat"), cp = 0.001, nBaggs = 500,
                        predID = "TripSetPredNo", Plot = TRUE)
alep.bag.S10.resid <- resids(alep.bag.S10, LonID = "Lon", LatID = "Lat",
                             predID = "TripSetPredNo", plot = TRUE)
title(main = "Variogram of Residuals (BCT)", sub = "Spatial ngrid = 10")
dev.off()
# Explore Bagged Predictions
x11()
bk = seq(0,0.5, by = 0.05)
pdf(file="predictionsSPATIAL.pdf")
alep.bag.l <- link(x = alep.bag.S10, object = alep.pr, LatID = "Latitude", LonID = "Longitude",
                   mapxlim = mapxlim, mapylim = mapylim, plot = TRUE, oob = TRUE, mfrow = c(2,2))
names(alep.bag.l)
plot(alep.bag.l)
dev.off()

x11()
#pdf(file = "~/Documents/R/Alepisaurus/prune_comparisons/Alep_SPATIAL_speciesLeveln31_ms150mb50.pdf")
valB <- grab(alep.bag, LatID = "Lat", LonID = "Lon",
             display.object = alep.pr, node.cols = node.colsY, cex = 0.8,
             mapxlim = mapxlim, mapylim = mapylim, mapcol = "gold3",
             oob = TRUE, onepage = TRUE, ylim = c(0,0.8))

dev.off()

# Partial Dependence Plots
#Partial dependence plots for factors:
x11()
partdep(object = alep.bag, Xvar = "Quarter", fact = TRUE, se = TRUE)
partdep(object = alep.bag, Xvar = "Year", fact = TRUE, se = TRUE)


#Partial dependence plots for continuous variables:
x11()
pdf(file=".pdf")
partdep(object = alep.bag, Xvar = "Length", ylim = c(0,0.4), fact = FALSE, se = TRUE)
dev.off()


#Partial dependence plots for interactions (Latitude and Longitude):
x11()
pdf("n31_ms150mb50_spatial-dependence.pdf")
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), plotmap = TRUE)

dev.off()

x11()
pdf("n31_ms150mb50_spatial-dependence_onlySMALLER1105.pdf")
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), var.cond = list("Length" < 1105),
        plotmap = TRUE)
dev.off()
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), var.cond = list(Year = 2014),
        plotmap = TRUE)
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), var.cond = list(Year = 2013),
        plotmap = TRUE)
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), var.cond = list(Year = 2010),
        plotmap = TRUE)
partdep(object = alep.bag, Xvar = c("Lon", "Lat"), var.cond = list(Year = 2011),
        plotmap = TRUE)


cartfin

library(ggplot2)
ggplot(cartfin, aes(Year, Full, group = Year))+geom_boxplot()
