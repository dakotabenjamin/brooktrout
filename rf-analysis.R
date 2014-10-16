# Predicting Brook Trout Streams with Random Forest
# Dakota Benjamin
# Case Western Reserve University
# Last Updated 9/24/2014
# Version 0.1

# Intro ----
#

# Initialization ----
rm(list=ls())
setwd("../brooktrout")

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL
library(raster)
library(RODBC)

# Load data ----

# Load the basic terrain analyses
# #catchment <- raster("tifs/repr/catchment.tif")
# tpi2k <- raster("Predictors/tpi2k.tif")
# tpi200 <- raster("Predictors/tpi200.tif")
# aspect <- raster("Predictors/aspect.tif")
# channel_altitude <- raster("Predictors/channel_altitude.tif")
# channel_base <- raster("Predictors/channel_base.tif")
# convergence <- raster("Predictors/convergence.tif")
# hcurv <- raster("Predictors/hcurv.tif")
# vcurv <- raster("Predictors/vcurv.tif")
# # There is something wrong with ls-factor ls_factor <- raster("Predictors/ls_factor.tif")
# relative_slope_position <- raster("Predictors/relative_slope_position.tif")
# shade <- raster("Predictors/shade.tif")
# #sinks <- raster("Predictors/sinks.tif")
# slope <- raster("Predictors/slope.tif")
# twi <- raster("Predictors/twi.tif")
# valley_depth <- raster("Predictors/valley_depth.tif")
# historicalforest <- raster("Predictors/historicalforest.tif")
# predictors <- addLayer(tpi2k, tpi200, aspect, channel_altitude, channel_base, convergence, hcurv, vcurv, relative_slope_position, shade, slope, valley_depth, twi, historicalforest)

#Soils
# need soils for both counties first. 
soils.cuy <- readOGR("Predictors//soils//cuy//spatial//", "soilmu_a_oh035")
soils.gea <- readOGR("Predictors//soils//geauga//spatial//", "soilmu_a_oh055")

#Sandstone
sandstone <- readOGR("Predictors//geology//PennsylvanianShape//Maps//", "ohbedpm-s")
sandstone <- subset(sandstone, AQUA= "Pm-s")
crs(sandstone) <- "+proj=lcc +lat_1=40.03333333333333 +lat_2=38.73333333333333 +lat_0=38 +lon_0=-82.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

#DEM for now, need twi next
dem <- raster("cuy_gea_dem.tif")


#Training Data -----
# conn<-odbcConnect("oldfield\\388_Fall2008\\fwdbrooktroutdata\\Brook Trout HSI 12-5-2006.mdb")
# train.tables <- sqlTables(conn)
# #info on streams, lat/long, etc.
# train.streams <- sqlFetch(conn,"tblStreamDetail")
# #Habitat- depth types, substrate
# train.habitat <- sqlFetch(conn,"tblHabitat")
# #Temperature- all, i think. 
# train.temps <- sqlFetch(conn,"tblTemperature")
# #Chemical data
# train.chem <- sqlFetch(conn,"tblChemical")

train.streams <- read.csv("streams/tblStreamDetail.txt")
#Change train.stream lat/long to State Plane
coordinates(train.streams) <- c("Longitude", "Latitude")
proj4string(train.streams) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
train.streams <- spTransform(train.streams, CRS("+proj=lcc +lat_1=41.7 +lat_2=40.43333333333333 +lat_0=39.66666666666666 +lon_0=-82.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

#Add to the train.streams table the average temps, etc.

#extract raster values for the points
ras <- raster::extract(dem,train.streams) # c(train.streams$Latitude, train.streams$Longitude)
train.streams@data = data.frame(train.streams, ras)
#train.streams <- na.omit(train.streams@data)

#Put all the data together ----

# sandstone
#transform to same crs as training
sandstone <- spTransform(sandstone, CRS("+proj=lcc +lat_1=41.7 +lat_2=40.43333333333333 +lat_0=39.66666666666666 +lon_0=-82.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs +towgs84=0,0,0"))
points.sand <- as.vector(over(x=train.streams, y=sandstone)$AQUA)
points.sand[is.na(points.sand)] <- "na"

# soils
#gea
#sptransform
soils.gea <- spTransform(soils.gea, CRS("+proj=lcc +lat_1=41.7 +lat_2=40.43333333333333 +lat_0=39.66666666666666 +lon_0=-82.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs +towgs84=0,0,0"))
points.soils.gea <- over(x=train.streams, y=soils.gea)

#all together now
train.streams@data <- data.frame(train.streams, points.sand, points.soils.cuy$MUSYM)


# Prediction Streams -----
#load streams

#add characteristics


# Random Forest Training ----

training <- data.frame(train.streams$bkt.Outlook, train.streams$ras, train.streams$points.sand.1)
names(training) <- c("outlook", "dem", "sandstone")

# set the seed
set.seed(2341)
train.rf <- randomForest(outlook ~ ., data=training, importance=T, ntree=1500, do.trace=100, proximity=T, na.action=na.exclude) # apply the proper mtry and ntree
print(train.rf)
# Variable Importance
par(mfrow=c(5,1))
for (i in 1:5) {
  barplot(sort(train.rf$importance[,i], dec=T),
          main=attributes(train.rf$importance)$dimnames[[2]][i], cex.names=0.6)
}

#Look at just Mean Decrease in Accuracy:
par(mfrow=c(1,1))
barplot(sort(train.rf$importance[,4], dec=T),main="Mean Decrease in Accuracy", cex.names=0.6)

#Outliers
outlier <- outlier(train.rf)
par(mfcol=c(1,1))
plot(outlier, type="h", main="Outlier data points in the RF")


# Predict classification for other streams ----

#Load Streams

rf.pred <- clusterR(xvars, predict, args=list(model=train.rf), progress='text')

