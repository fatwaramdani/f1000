#xgBoost is a Boosting algorithm

setwd("D:/MLinRStudio")

install.packages("ggsn")
install.packages("rtools")
install.packages("devtools")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
install.packages("xgboost")
install.packages("mapmisc")

library(raster)
library(xgboost)
library(rtools)
library(devtools)
library(sp)
library(maptools)
library(mapmisc)



#import data
ras <- stack("PS.tif")
ras
shp <- shapefile("samplingPS15.csv")

#Visualisasi GIS dengan komponen kartografi
#PlotRGB
plotRGB(ras,r=3,g=2,b=1, stretch = "lin")
scaleBar(ras,pos="bottomright",bg="white")



# Load the sample data
# Alternatively, you can use the supplied orthophotos to generate a new set of training and validation data 
# Your samples layer must have a column for each image in the raster stack, a column for the land cover class that point represents, an X and Y column
# You can create such a sample file using QGIS or another GIS software
samplesUBPS = read.csv("samplingPS15.csv")

#The first thing that we will do is to extract the values for each sample point from the raster:

vals <- extract(ras,shp)

#we convert the dataframe into a matrix ??? this is needed for xgBoost:

train <- data.matrix(vals)


#We must convert factors to numeric
# They must be starting from number 0 to use multiclass
# For instance: 0, 1, 2, 3, 4...
classes <- as.numeric(as.factor(shp@data$class)) - 1

#Train the model
xgb <- xgboost(data = train, 
               label = classes, 
               eta = 0.1,
               max_depth = 6, 
               nround=100, 
               objective = "multi:softmax",
               num_class = length(unique(classes)),
               nthread = 3)

#Classify
# prediction on the raw raster yielded an error. That's why I extract the values from the raster and predict on them.
# Note: works only with small raster or huge RAM. Let me know if it works for you.
result   <- predict(xgb, ras[1:(nrow(ras)*ncol(ras))])

#create dummy result raster
resxgBoost      <- raster(ras)

#fill in results and add a "1" to them (to get back to inital class numbering! - see above "Prepate data" for more information)
resxgBoost      <- setValues(resxgBoost,result+1)
plot(resxgBoost)
plot(resxgBoost, col=c("green", "darkgreen","red", "yellow", "orange"))
legend("topright",
       legend = c("grass", "trees", "building", "roads", "residential"),
       fill = c("green", "darkgreen", "red", "yellow", "orange"),
       border = FALSE,
       bty = "n") # turn off legend border


# Convert the evaluation data into a spatial object using the X and Y coordinates and extract predicted values
eva.sp = SpatialPointsDataFrame(coords = cbind(evaUBPS$xcoor, evaUBPS$ycoor), data = evaUBPS, 
                                proj4string = crs("+proj=utm +zone=49 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Superimpose evaluation points on the predicted classification and extract the values
# neural network
xgBoost_Eval = extract(resxgBoost, eva.sp)


# Create an error matrix for each of the classifiers
xgBoost_errorM = confusionMatrix(as.factor(xgBoost_Eval),as.factor(evaUBPS$class))
xgBoost_errorM

freq(resxgBoost)

writeRaster(resxgBoost, filename = "resxgboost.grd")
