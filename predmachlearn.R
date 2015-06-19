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

#final prediction
prediction <- predict(forestFit, testing, type = "class")
prediction

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(prediction)
