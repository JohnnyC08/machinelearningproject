library(caret)
library(doParallel)

set.seed(4567)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

crossValidateData <- function() {
  training <- cleanData(getTrainingData())
  inTraining <- createDataPartition(training$classe, p=.7, list=F)
  
  trainingSubset <- training[inTraining,]
  trainingSubsetForValidation <- training[-inTraining,]
  
  loadModelFromFile()
  validationPrediction <- predict(modelFit, trainingSubsetForValidation)
  confusionMatrix(validationPrediction, trainingSubsetForValidation$classe)
}

createModel <- function(dataToFit) {
  modelFit <- train(classe ~. , model="rf", data=dataToFit, prox=T)
}

loadModelFromFile <- function() {
  load("machinelearningproject/modelfit.RData")
}

cleanData <- function(dataToBeCleaned) {
  hasNoNa <- apply(dataToBeCleaned, 2, function(column) { sum(is.na(column)) == 0 })
  dataToBeCleaned <- dataToBeCleaned[, hasNoNa]
  columnsToGetRidOf <- c("X", "user_name", "raw_timestamp_part_1","raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
  dataToBeCleaned <- dataToBeCleaned[,!(names(dataToBeCleaned) %in% columnsToGetRidOf)]
}

getTrainingData <- function() {
  training <- read.csv("machinelearningproject/training.csv", header = T, na.strings = c("#DIV/0!", "", "NA"))
}

getTestingData <- function() {
  training <- read.csv("machinelearningproject/testing.csv", header = T, na.strings = c("#DIV/0!", "", "NA"))
}