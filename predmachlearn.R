#libraies
library(caret)
library(rpart)
library(rattle)
library(randomForest)

#loading

testing <- read.csv("pml-testing.csv",  na.strings = c("NA", "#DIV/0!", ""))
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))

dim(testing)
dim(training)

#cleanup

training <- training[-(1:7)]
testing <- testing[-(1:7)]

training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

dim(testing)
dim(training)

#subsampling
set.seed(4200)
inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
subtraining <- training[inTrain,]
subtesting <- training[-inTrain,]

#tree model
treeFit <- rpart(classe ~ ., method = "class", data = subtraining)
fancyRpartPlot(treeFit, main = "Decision Tree", 
               sub = "Rpart Decision Tree To Predict Classe")
treePredict <- predict(treeFit, subtesting, type = "class")
confusionMatrix(treePredict, subtesting$classe) #73.81

#forest model
forestFit <- randomForest(classe ~ ., data = subtraining, method = "class")
forestPredict <- predict(forestFit, subtesting, type = "class")
confusionMatrix(forestPredict, subtesting$classe) #99.46