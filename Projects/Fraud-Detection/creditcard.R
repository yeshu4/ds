library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)
library(Hmisc)
library(party)
library(e1071)

set.seed(1234)
creditData <- read_csv("C:/Users/Intellipaat-Ashwin/Desktop/October/creditcard/creditcard.csv")

glimpse(creditData)
View(creditData)


table(creditData$Class)

# make Class a factor
creditData$Class <- as.factor(creditData$Class)

#Dividing the data into train and test sets

train <- creditData[1:15000, ]
test <- creditData[15001:20000, ]
#count of class in train set
train %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse

#count of class in train set
test %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse

# build random forest model using every variable
rfModel <- randomForest(Class ~ . , data = train)

test$predicted <- predict(rfModel, test)

#confusion-matrix
library(caret)
confusionMatrix(test$Class, test$predicted)


library(MLmetrics)

F1_all <- F1_Score(test$Class, test$predicted)

F1_all

#Importance of predictors
options(repr.plot.width=5, repr.plot.height=4)
varImpPlot(rfModel,
           sort = T,
           n.var=10,
           main="Top 10 Most Important Variables")



#Using only V17 as predictor

rfModelTrim1 <- randomForest(Class ~  V17, 
                             data = train)

test$predictedTrim1 <- predict(rfModelTrim1, test)

F1_1 <- F1_Score(test$Class, test$predictedTrim1)
F1_1

#Using V17 & V14 as predictors

rfModelTrim2 <- randomForest(Class ~  V17 + V14, 
                             data = train)

test$predictedTrim2 <- predict(rfModelTrim2, test)

F1_2 <- F1_Score(test$Class, test$predictedTrim2)
F1_2


#Using V17, V14  & Time as predictors

rfModelTrim3 <- randomForest(Class ~  V17 + V14 + Time, 
                             data = train)

test$predictedTrim3 <- predict(rfModelTrim3, test)

F1_3 <- F1_Score(test$Class, test$predictedTrim3)
F1_3

---------------------------------------------------------------
  
  # build dataframe of number of variables and scores
numVariables <- c(1,2,3,30)
F1_Score <- c(F1_1, F1_2, F1_3, F1_all)
variablePerf <- data.frame(numVariables, F1_Score)

# plot score performance against number of variables

ggplot(variablePerf, aes(numVariables, F1_Score)) + geom_point() +geom_line() + labs(x = "Number of Variables", y = "F1 Score", title = "F1 Score Performance")