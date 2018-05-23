library(marmap)
library(mapplots)
library(RColorBrewer)
library(ggplot2)
library(diet)
library(tidyr)

loc <- read.csv("WSC_trawl_metadata.csv")[,c(1,2,4,5,15)]
comp <- read.csv("WSC_trawl_composition.csv")[,c(1:5,8)]

coul <- brewer.pal(11, "RdBu")
coul <- colorRampPalette(coul)(100)
coul2 <- brewer.pal(8, "Accent")
coul2 <- colorRampPalette(coul2)(95)

I <- which(comp$Family == "Hyperiidae"|comp$Family == "Lysanassoidea"|comp$Family == ""|comp$Family == "Amphipoda"|comp$Type == "P"|comp$Type == "O")

comp <- comp[-I,]
comp$count <- 1

comp$group <- paste(comp$Type, comp$Family, sep = "-")
comp$group <- as.factor(comp$group)
comp$Tow <- as.factor(comp$Tow)
comp1 <- aggregate(count~Tow*group, comp, FUN = sum)
comp2 <- aggregate(count~Tow, comp, FUN = sum)
colnames(comp2) <- c("Tow", "sum")

comp3 <- merge(comp1, comp2, by = "Tow")
comp3$PropN <- comp3$count/comp3$sum

# trawlcomp <- aggregate(count~Tow*Species, comp, FUN = sum)

trawl <- merge(comp3, loc, by = "Tow")[,c(1,2,5:9)]

trawl$SST <- 21
trawl$TripSetNo <- "wsc"

trawl$start_lon <- as.factor(trawl$start_lon)
trawl$start_lon <- as.ordered()
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

diet <- ggplot(trawl, aes(x="", y = PropN, fill = group))+geom_bar(width = 1, stat = "identity")
diet <- diet +coord_polar("y", start = 0)+scale_fill_manual(values=sample(color,32))+facet_wrap(~Tow)
diet

# squid_long$PropV <- squid_long$PropV/100
colnames(trawl) <- c("TripSetPredNo", "Group", "PropN", "Date","Lat", "Lon","Depth", "SST", "TripSetNo")

cartfin <- trawl

cartfin$PredSpp <- "BBlock"
cartfin$PredSpp <- as.factor(cartfin$PredSpp)
cartfin$SST <- as.integer(cartfin$SST)
cartfin$Group <- as.factor(cartfin$Group)
cartfin$Date <-as.POSIXct(cartfin$Date, format ="%m-%d-%Y")


# Begin using diet package here

write.csv(cartfin, file = "cartfin.csv", row.names = FALSE)
cartdata <- read.pp(filenm = "cartfin.csv",  
                    labels = list(PredatorL = "TripSetPredNo", TripSetL = "TripSetNo",
                                  SpeciesL = "Group",SST="SST",
                                  DateL = "Date", WeightL = "PropN", PreyGrpL = "Group"),
                    Datef = "%m/%d/%Y", p = 0.001, Xvars = c("Lat", "Lon", "SST", "Depth"))

PreySquidSort <-read.csv("PreyTaxonSort.csv")
#data(PreyTaxonSort)
pal <- c(topo.colors(20)[1:5], heat.colors(20)[1:8], terrain.colors(20)[1:7])
val <- apc(x = cartdata, preyfile = PreySquidSort, palette = pal, check = TRUE)
node.colsY <- val$cols
dietPP <- val$x   # updated diet matrix with prey taxa codes
head(dietPP)

# Create a predator column
dietPP$Predator <- as.factor(rep("BBlock", nrow(dietPP)))
head(dietPP)

x11()
pdf(file="wsc_explore_Family.pdf")
explore.diet <- plot(x = dietPP, Xvar = c("Lat", "Lon", "SST", "Depth"), LonID = "Lon",
                     LatID = "Lat",  mapxlim = c(-150, -120), mapylim = c(20, 35), Factor = "Predator",
                     SmXvar = c("Lon", "Lat"), PredIDno = "TripSetPredNo", prey.cols = node.colsY,
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
alep.dp <- dpart(Group ~ Lat+Lon+Depth,
                 data = dietPP, weights = W, minsplit = 1, minbucket = 1,
                 cp = 0.01)
plot(alep.dp, node.cols=node.colsY)
summary(alep.dp)
print(alep.dp, setID = "TripSetNo")
plotcp(alep.dp)
dev.off()

x11()
pdf(file="wsc_cart-test_Family.pdf")
alep.pr <- prune(alep.dp, se = 1)
plot(alep.pr, node.cols = node.colsY)

# Variable importance ranking
vi <- importance(alep.pr)
dev.off()

# Maps of diversity
# Diversity index
mapxlim <- c(-150, -120)
mapylim <- c(20, 35)

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