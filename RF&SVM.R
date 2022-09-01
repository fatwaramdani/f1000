# set working directory. This will be the directory where all the unzipped files are located
setwd("D:/MLinRStudio")

install.packages("e1071")

# load required libraries
library(raster) # for the manipulation of raster data
library(caret) # for the machine learning algorithms
library(sp) # for the manipulation of spatial objects
library(nnet) # Artificial Neural Network
library(randomForest) # Random Forest 
library(kernlab) # Support Vector Machines
library(rgdal)
library(e1071)


# Load the PlanetScope stack of the study area
psub = stack("PS.tif")

#plot untuk melihat nama layers
plot(psub)

# Name the layers of the stack based on previously saved information
names(psub) = as.character(read.csv("ub_ps_names.csv")[,1])

# Load the sample data
# Alternatively, you can use the supplied orthophotos to generate a new set of training and validation data 
# Your samples layer must have a column for each image in the raster stack, a column for the land cover class that point represents, an X and Y column
# You can create such a sample file using QGIS or another GIS software
samplesUBPS = read.csv("samplingPS15.csv")

# Split the data frame into 60-40 by class >> tidak dilakukan, semua jadi data training
train = list(0)
eval = list(0)
for (i in 1:5){ # loop through all five classes
  cls = samplesUBPS[samplesUBPS$class == i,]
  smpl <- floor(0.60 * nrow(cls))
  tt <- sample(seq_len(nrow(cls)), size = smpl)
  train[[i]] <- cls[tt,]
  eval[[i]] <- cls[-tt,]
}

# combine them all into training and evaluation data frames >> tidak dilakukan
trnUBPS = do.call(rbind, train) #data latih
evaUBPS = do.call(rbind, eval) #data uji

# Set up a resampling method in the model training process
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number of folds
                   repeats = 5) # number of repeats

# Generate a grid search of candidate hyper-parameter values for inclusion into the models training
# These hyper-parameter values are examples. You will need a more complex tuning process to achieve high accuracies

rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node

svm.grid <- expand.grid(sigma=seq(from = 0.01, to = 0.10, by = 0.02), # controls for non-linearity in the hyperplane
                        C=seq(from = 1, to = 5, by = 1)) # controls the influence of each support vector

# Run the random forest model
rf_model <- caret::train(x = trnUBPS[,(1:3)], y = as.factor(as.integer(as.factor(trnUBPS$class))),
                                method = "rf", metric="Accuracy", trainControl = tc, tuneGrid = rf.grid)

# Run the support vector machines model
svm_model <- caret::train(x = trnUBPS[,(1:3)], y = as.factor(as.integer(as.factor(trnUBPS$class))),
                                  method = "svmRadialSigma", metric="Accuracy", trainControl = tc, tuneGrid = svm.grid)

## Apply the models to data
# Apply the random forest model
rf_prediction = raster::predict(psub, model=rf_model)

# Apply the support vector machines model
svm_prediction = raster::predict(psub, model=svm_model)

plot(rf_prediction)
plot(svm_prediction)

# Convert the evaluation data into a spatial object using the X and Y coordinates and extract predicted values
eva.sp = SpatialPointsDataFrame(coords = cbind(evaUBPS$xcoor, evaUBPS$ycoor), data = evaUBPS, 
                                proj4string = crs("+proj=utm +zone=49 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Superimpose evaluation points on the predicted classification and extract the values
# random forest
rf_Eval = extract(rf_prediction, eva.sp)
# support vector machines
svm_Eval = extract(svm_prediction, eva.sp)

# Create an error matrix for each of the classifiers
rf_errorM = confusionMatrix(as.factor(rf_Eval),as.factor(evaUBPS$class))
svm_errorM = confusionMatrix(as.factor(svm_Eval),as.factor(evaUBPS$class))

rf_errorM
svm_errorM

# Plot the results next to one another for comparison
rstack = stack(resxgBoost, nnet_prediction_PS, rf_prediction, svm_prediction) # combine the layers into one stack
names(rstack) = c("XGBoost", "Neural Network", "Random Forest", "SVM") # name the stack
plot(rstack, col=c("green", "darkgreen","red","yellow","orange"))
legend("bottomright",
       legend = c("grass", "trees", "building", "roads", "residential"),
       fill = c("green", "darkgreen", "red", "yellow", "orange"),
       border = TRUE,
       bty = "n") # turn off legend border

freq(rf_prediction)
freq(svm_prediction)

writeRaster(rf_prediction, filename = "rf_prediction.grd")
writeRaster(svm_prediction, filename = "svm_prediction.grd")

#######################################################################################################
install.packages("reshape")
library(reshape)
library(ggspatial)

ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, quiet = TRUE) +
  layer_spatial(svm_result) +
  annotation_scale(location = "br", width_hint = 0.5, style = "ticks") +
  annotation_north_arrow(location = "tl", 
                       style = north_arrow_nautical, 
                       height = unit(1, "cm"),
                       width = unit(1, "cm")) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Classification Result",
       subtitle = "Study Area: Universitas Brawijaya")



# The class names and colors for plotting
class <- c("grass", "trees", "building", "roads", "residential")
classdf <- data.frame(classvalue1 = c(1,2,3,4,5), classnames1 = class)
classcolor <- c("green", "darkgreen","red","yellow","orange")

xgb_result <- ratify(resxgBoost)
rat <- levels(xgb_result)[[1]]

rat$landcover <- class
levels(xgb_result) <- rat

library(rasterVis)
plt <- levelplot(xgb_result, col.regions = classcolor, main = 'XGBoost Classification Result')
print(plt)

levelplot(xgb_result, maxpixels = 1e6,
          col.regions = classcolor,
          scales=list(draw=TRUE),
          main = "XGBoost Classification Result")

#########################################################################################################

install.packages("tmap")
install.packages("tmaptools")
library(tmap)
library(tmaptools)
tm_shape(xgb_result)+tm_raster(palette="-Greys", style="cont", legend.show=FALSE)+
  tm_shape(xgb_result)+
  tm_compass(position = c("left", "top"))+
  tm_layout(title = "XGBoost Result", title.size = 2.5, title.position = c("right", "top"))+
  tm_credits("Dr. Fatwa Ramdani, 2020", position= c("right", "bottom"))+
  tm_raster(style= "cat",
          labels = c("grass", "trees", "building", "roads", "residential"),
          palette = c("green", "darkgreen","red","yellow","orange"),
          title="Land Cover")+
  tm_layout(legend.position= c("left", "bottom"), legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

            