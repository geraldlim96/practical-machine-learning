#Load the required R libraries for the analysis portion  

library(knitr) 
library(caret) 
library(rpart) 
library(rpart.plot) 
library(rattle) 
library(randomForest) 
library(corrplot)

#Open downloaded data 

training <- read.csv(Train) 
testing <- read.csv(Test)

#Subsequently is to partition the data given into training and test sets, with approximately 70% going to training and the remaining 30% to test. 

inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE) 
TrainSet <- training[inTrain, ] 
TestSet <- training[-inTrain, ]

#Both datasets have 160 variables but many of those variables have NA that can be removed. To do so, first remove those with nearly 0 variance. 

NZV <- nearZeroVar(TrainSet) 
TrainSet <- TrainSet[, -NZV] 
TestSet <- TestSet[, -NZV]

#Next, to remove variables that are mostly NA.

NA <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95 
TrainSet <- TrainSet[, NA==FALSE] 
TestSet <- TestSet[, NA==FALSE] 

#Lastly, remove the variables used for identification.

TrainSet <- TrainSet[, -(1:5)] 
TestSet <- TestSet[, -(1:5)]

#The 3 chosen methods used to model this regression are Decision Tree, Generalized Boosted Model and Random Forest. The method with the highest accuracy would be used to predict the 20 quiz results

#Firstly, fit the Decision Tree Model

set.seed(12345)
modFitDecTree <- rpart(classe ~ ., data=TrainSet, method="class") 
modFitRpartPlot(modFitDecTree). 

#To do the prediction

predictDecTree <- predict(modFitDecTree, newdata=TestSet, type="class") 
confMatDecTree <- confusionMatrix(predictDecTree, TestSet$classe) 
confMatDecTree

Confusion Matrix and Statistics 

          Reference 

Prediction  A    B    C   D   E 
         A 1530 269  51  79  16 
         B 35   575  31  25  68 
         C 17    73 743  68  84 
         D 39   146 130 702 128 
         E 53    76  71  90 786 

Overall Statistics Accuracy : 0.7368 
95% CI : (0.7253, 0.748) 
No Information Rate : 0.2845 
P-Value [Acc > NIR] : < 2.2e-16 
Kappa : 0.6656 
Mcnemar's Test P-Value : < 2.2e-16 

Statistics by Class: 
                            Class: A Class: B Class: C Class: D Class: E 
        Sensitivity          0.9140   0.50483   0.7242   0.7282   0.7264 
        Specificity          0.9014   0.96650   0.9502   0.9100   0.9396 
        Pos Pred Value       0.7866   0.78338   0.7543   0.6131   0.7305 
        Neg Pred Value       0.9635   0.89051   0.9422   0.9447   0.9384 
        Prevalence           0.2845   0.19354   0.1743   0.1638   0.1839 
        Detection Rate       0.2600   0.09771   0.1263   0.1193   0.1336 
        Detection Prevalence 0.3305   0.12472   0.1674   0.1946   0.1828 
        Balanced Accuracy    0.9077   0.73566   0.8372   0.8191   0.8330 

#This method gives an accuracy of 0.7368.

#Secondly, fit the Generalized Boosted Model

set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1) 
modFitGBM <- train(classe ~ ., data=TrainSet, method = "gbm", trControl = controlGBM, verbose = FALSE) 
modFitGBM$Model. 

#To do the prediction

predictGBM <- predict(modFitGBM, newdata=TestSet) 
confMatGBM <- confusionMatrix(predictGBM, TestSet$classe) 
confMatGBM

Confusion Matrix and Statistics 

           Reference 
           
Prediction   A   B   C  D  E 
         A 1669 13  0  2   0 
         B   4 1113 23 5   3 
         C   0  13 998 16  2 
         D   1   0  5 941  8 
         E   0   0  0  0 1069 
         
Overall Statistics Accuracy : 0.9839 
95% CI : (0.9803, 0.9869) 
No Information Rate : 0.2845 
P-Value [Acc > NIR] : < 2.2e-16 
Kappa : 0.9796 
Mcnemar's Test P-Value : NA 

Statistics by Class: 
                            Class: A Class: B Class: C Class: D Class: E 
        Sensitivity          0.9970   0.9772   0.9727   0.9761   0.9880 
        Specificity          0.9964   0.9926   0.9936   0.9972   1.0000 
        Pos Pred Value       0.9911   0.9695   0.9699   0.9853   1.0000 
        Neg Pred Value       0.9988   0.9945   0.9942   0.9953   0.9973 
        Prevalence           0.2845   0.1935   0.1743   0.1638   0.1839 
        Detection Rate       0.2836   0.1891   0.1696   0.1599   0.1816 
        Detection Prevalence 0.2862   0.1951   0.1749   0.1623   0.1816 
        Balanced Accuracy    0.9967   0.9849   0.9832   0.9866   0.9940 

#This method gives an accuracy of 0.9839.

#Lastly, fit the Random Forest Model 

set.seed(12345) 
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE) 
modFitRandForest <- train(classe ~ ., data=TrainSet, method="rf", trControl=controlRF) 
modFitRandForest$Model

#To do the prediction

predictRandForest <- predict(modFitRandForest, newdata=TestSet) 
confMatRandForest <- confusionMatrix(predictRandForest, TestSet$classe) 
print(confMatRandForest)

Confusion Matrix and Statistics 

        Reference 
        
Prediction   A   B   C   D   E 
         A 1674  5   0  0   0 
         B   0 1133  3  0   0 
         C   0   1 1023 9   0 
         D   0   0   0 955  4 
         E   0   0   0  0 1078 

Overall Statistics Accuracy : 0.9963 
95% CI : (0.9943, 0.9977) 
No Information Rate : 0.2845 
P-Value [Acc > NIR] : < 2.2e-16 
Kappa : 0.9953 
Mcnemar's Test P-Value : NA 

Statistics by Class: 
                            Class: A Class: B Class: C Class: D Class: E 
        Sensitivity          1.0000   0.9947   0.9971   0.9907   0.9963 
        Specificity          0.9988   0.9994   0.9979   0.9992   1.0000 
        Pos Pred Value       0.9970   0.9974   0.9903   0.9958   1.0000 
        Neg Pred Value       1.0000   0.9987   0.9994   0.9982   0.9992 
        Prevalence           0.2845   0.1935   0.1743   0.1638   0.1839 
        Detection Rate       0.2845   0.1925   0.1738   0.1623   0.1832 
        Detection Prevalence 0.2853   0.1930   0.1755   0.1630   0.1832 
        Balanced Accuracy    0.9994   0.9971   0.9975   0.9949   0.9982 
        
#This method gives an accuracy of 0.9963. Hence conclude that the Random Forest method is the best to predict the 20 quiz results. 

predictTEST <- predict(modFitRandForest, newdata=testing) 
predictTEST

[1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
