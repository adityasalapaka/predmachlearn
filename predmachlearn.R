#libraies
library(caret)


#loading

testing <- read.csv("pml-testing.csv",  na.strings = c("NA", "#DIV/0!", ""))
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))

dim(testing)
dim(training)

#cleanup

training <- training[-(1:7)]
testing <- testing[-(1:7)]

training <- training[,colSums(is.na(training)) != 0]
testing <- testing[,colSums(is.na(testing)) != 0]

dim(testing)
dim(training)

#subsampling

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)