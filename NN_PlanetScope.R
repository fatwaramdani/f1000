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

# Name the layers of the Sentinel-2 stack based on previously saved information
names(psub) = as.character(read.csv("ub_ps_names.csv")[,1])

# Load the sample data
# Alternatively, you can use the supplied orthophotos to generate a new set of training and validation data 
# Your samples layer must have a column for each image in the raster stack, a column for the land cover class that point represents, an X and Y column
# You can create such a sample file using QGIS or another GIS software
samplesUBPS = read.csv("samplingPS15.csv")

# Split the data frame into 60-40 by class >> tidak dilakukan, semua jadi data training
trainPS = list(0)
evalPS = list(0)
for (i in 1:5){ # loop through all five classes
  cls = samplesUBPS[samplesUBPS$class == i,]
  smpl <- floor(0.60 * nrow(cls))
  tt <- sample(seq_len(nrow(cls)), size = smpl)
  train[[i]] <- cls[tt,]
  eval[[i]] <- cls[-tt,]
}

# combine them all into training and evaluation data frames >> tidak dilakukan
trn = do.call(rbind, train) 
eva = do.call(rbind, eval)

# Set up a resampling method in the model training process
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number of folds
                   repeats = 5) # number of repeats

# Generate a grid search of candidate hyper-parameter values for inclusion into the models training
# These hyper-parameter values are examples. You will need a more complex tuning process to achieve high accuracies

nnet.grid = expand.grid(size = seq(from = 1, to = 15, by = 1), # number of neurons units in the hidden layer 
                        decay = seq(from = 0.1, to = 0.5, by = 0.1)) # regularization parameter to avoid over-fitting 

#nnet.grid = expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), .size = c(3, 5, 7, 9))

## Begin training the models. This will take about 6 minutes
# Run the neural network model
nnet_model_PS <- caret::train(x = samplesUBPS[,(1:3)], y = as.factor(as.integer(as.factor(samplesUBPS$class))),
                                  method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid)

## Apply the models to data
# Apply the neural network model to the data
nnet_prediction_PS = raster::predict(psub, model=nnet_model_PS)


# Convert the evaluation data into a spatial object using the X and Y coordinates and extract predicted values
eva.sp = SpatialPointsDataFrame(coords = cbind(evaUBPS$xcoor, evaUBPS$ycoor), data = evaUBPS, 
                                proj4string = crs("+proj=utm +zone=49 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Superimpose evaluation points on the predicted classification and extract the values
# neural network
nnet_Eval = extract(nnet_prediction_PS, eva.sp)


# Create an error matrix for each of the classifiers
nnet_errorM = confusionMatrix(as.factor(nnet_Eval),as.factor(evaUBPS$class))

nnet_errorM


# Plot the results next to one another along with the 2018 NMD dataset for comparison
names(nnet_prediction_PS) = c("Neural Network") # name the stack
plot(nnet_prediction_PS, col=c("green", "darkgreen","red","yellow","orange"))
legend("topright",
       legend = c("grass", "trees", "building", "roads", "residential"),
       fill = c("green", "darkgreen", "red", "yellow", "orange"),
       border = FALSE,
       bty = "n") # turn off legend border
freq(nnet_prediction_PS)
hist(nnet_prediction_PS)

writeRaster(nnet_prediction_PS, filename = "nnet_prediction.grd")
