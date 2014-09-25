# Predicting Brook Trout Streams with Random Forest
# Dakota Benjamin
# Case Western Reserve University
# Last Updated 9/24/2014
# Version 0.1

# Intro ----
#

# Initialization ----
rm(list=ls())

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL
library(raster)

# Load data ----

# Load the basic terrain analyses
#catchment <- raster("tifs/repr/catchment.tif")
tpi2k <- raster("Predictors/tpi2k.tif")
tpi200 <- raster("Predictors/tpi200.tif")
aspect <- raster("Predictors/aspect.tif")
channel_altitude <- raster("Predictors/channel_altitude.tif")
channel_base <- raster("Predictors/channel_base.tif")
convergence <- raster("Predictors/convergence.tif")
hcurv <- raster("Predictors/hcurv.tif")
vcurv <- raster("Predictors/vcurv.tif")
# There is something wrong with ls-factor ls_factor <- raster("Predictors/ls_factor.tif")
relative_slope_position <- raster("Predictors/relative_slope_position.tif")
shade <- raster("Predictors/shade.tif")
#sinks <- raster("Predictors/sinks.tif")
slope <- raster("Predictors/slope.tif")
twi <- raster("Predictors/twi.tif")
valley_depth <- raster("Predictors/valley_depth.tif")
historicalforest <- raster("Predictors/historicalforest.tif")

predictors <- addLayer(tpi2k, tpi200, aspect, channel_altitude, channel_base, convergence, hcurv, vcurv, relative_slope_position, shade, slope, valley_depth, twi)

predictors <- addLayer(predictors, historicalforest)

#Training Data -----
conn<-odbcConnectAccess("C:\\Users\\Dakota\\Dropbox\\_CASE\\_SENIOR\\Brook Trout\\388_Fall2008\\fwdbrooktroutdata\\Brook Trout HSI 12-5-2006.mdb")
train.tables <- sqlTables(conn)
#info on streams, lat/long, etc.
train.streams <- sqlFetch(conn,"tblStreamDetail")
#Habitat- depth types, substrate
train.habitat <- sqlFetch(conn,"tblHabitat")
#Temperature- all, i think. 
train.temps <- sqlFetch(conn,"tblTemperature")
#Chemical data
train.chem <- sqlFetch(conn,"tblChemical")

#Change train.stream lat/long to State Plane

#Add to the train.streams table the average temps, etc.

#extract raster values for the points
ras <- raster::extract(predictors, c(train.streams$Latitude, train.streams$Longitude))
points@data = data.frame(points@data, ras)
points@data <- na.omit(points@data)
#points <- subset(points, tpi2k != NA)
points@data <- droplevels(points@data)

# Random Forest Training ----

# set the seed
set.seed(23461)
train.rf <- randomForest(com ~ ., data=points@data, importance=T, ntree=1500, do.trace=100, proximity=T, na.action=na.exclude) # apply the proper mtry and ntree

print(train.rf)
# Variable Importance
par(mfrow=c(4,4))
for (i in 1:14) {
  barplot(sort(train.rf$importance[,i], dec=T),
          main=attributes(train.rf$importance)$dimnames[[2]][i], cex.names=0.6)
}

#Look at just Mean Decrease in Accuracy:
par(mfrow=c(1,1))
barplot(sort(train.rf$importance[,13], dec=T),main="Mean Decrease in Accuracy", cex.names=0.6)

#Outliers
outlier <- outlier(train.rf)
par(mfcol=c(1,1))
plot(outlier, type="h", main="Outlier data points in the RF")


# Predict classification and write it to a raster ----

rpath=paste('~/Documents/GitHub/randomForest', "tifs/repr", sep="/")
xvars <- stack(paste(rpath, paste(rownames(train.rf$importance), "tif", sep="."), sep="/"))
# #not working right now ----
# 
#  tr <-  blockSize(predictors, n=15, minrows=127)
# s <- raster(predictors[[1]])
# s <- writeStart(s, filename=paste('~/GitHub/randomForest', "prob_landcover.tif", sep="/"), overwrite=TRUE)
# # 
#  for (i in 1:tr$n) {
#   v <- getValuesBlock(predictors, row=tr$row[i], nrows=tr$nrows[i])
#   v <- as.data.frame(v)
#   rf.pred <- predict(train.rf,v, type='response')
# #  rf.pred1 <- predict(train.rf,v, type='prob')
#   writeValues(s, as.numeric(rf.pred), tr$row[i])
#   cat(" Finished Block", i, ". . .", sep=" ")
#  }
# s <- writeStop(s)

# # try on a subset
# e<- extent(2209444,2222142,596814,606393)
# cropped<- crop(xvars, e)

beginCluster()
rf.pred <- clusterR(xvars, predict, args=list(model=train.rf), progress='text')
writeRaster(rf.pred, filename = "rasterout1.tif", datatype='GTiff')
endCluster()
