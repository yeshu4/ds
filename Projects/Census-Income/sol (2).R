
#Read csv file
census <- read.csv("C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\BLOG\\files\\project\\census-income.csv")

View(census)

str(census)



#------------------------------------------------------------------------------------------------------------------------
#Data=-Preprocessing

census$workclass[28]


#Convert factor columns to character

census$workclass <- as.character(census$workclass)
census$occupation <- as.character(census$occupation)
census$native.country <- as.character(census$native.country)
census$education <- as.character(census$education)
census$marital.status <- as.character(census$marital.status)
census$relationship <- as.character(census$relationship)
census$race <- as.character(census$race)
census$sex <- as.character(census$sex)
census$X <- as.character(census$X)


census$workclass[28]  

#Replace " ?" with NA
census[census == " ?"] <- NA



#Remove NA
census <- na.omit(census)


sum(is.na(census))

#Remove Whitespaces
library("dplyr")
library(stringr)

census %>%
  mutate_if(is.character, str_trim) -> census


View(census)


#Convert character columns back to factors
census$workclass <- as.factor(census$workclass)
census$occupation <- as.factor(census$occupation)
census$native.country <- as.factor(census$native.country)
census$education <- as.factor(census$education)
census$marital.status <- as.factor(census$marital.status)
census$relationship <- as.factor(census$relationship)
census$race <- as.factor(census$race)
census$sex <- as.factor(census$sex)
census$X <- as.factor(census$X)

str(census)

#-------------------------------------------------------------------------------------------------------------------------------
#Data-Manipulation

library("dplyr")


census %>% select(education) -> census_ed

census %>% select(age:relationship) -> census_seq
View(census_seq)

census %>% select(c(5,8,11)) -> census_col

View(census_col)

census %>% select(sex,workclass) %>% filter(sex == "Male" & workclass == "State-gov") -> male_gov
View(male_gov)


census %>% select(age,education,native.country) %>% filter(age == 39 & (education == "Bachelors" | native.country == "United-States")) -> census_us
View(census_us)


census %>% sample_n(200) -> census_200
View(census_200)


census %>% count(workclass)


census %>% group_by(workclass) %>% summarise(mean(capital.gain))

#---------------------------------------------------------------------------------------------------------
#Data-Visualization

library("ggplot2")

#Box-plot

ggplot(data = census, aes(x = relationship, fill = race)) +
  geom_bar()


ggplot(data = census, aes(x = relationship, fill = race)) +
  geom_bar()+
  labs(x =  "Categories of Relationships", y = "Count of Categories")


ggplot(data = census, aes(x = relationship, fill = sex)) +
  geom_bar()+
  labs(x =  "Categories of Relationships", y = "Count of Categories")



ggplot(data = census, aes(x = relationship, fill = sex)) +
  geom_bar(position = "dodge")+
  labs(x =  "Categories of Relationships", y = "Count of Categories")


ggplot(data = census, aes(x = relationship, fill = sex)) +
  geom_bar(position = "dodge")+
  labs(x =  "Categories of Relationships", y = "Count of Categories", title = "Distribution of Relationships by Sex" )
#-----------------------------------------------------------------------------------------------------------------------------------------
#Histogram

ggplot(data = census, aes(x = age)) + geom_histogram(bins = 50)

ggplot(data = census, aes(x = age, fill = X)) + geom_histogram(bins = 50)

ggplot(data = census, aes(x = age, fill = X)) + geom_histogram(bins = 50) +
  labs(title = "Distribution of Age")


ggplot(data = census, aes(x = age, fill = X)) + geom_histogram(bins = 50) +
  labs(title = "Distribution of age", fill = "Yearly income")

ggplot(data = census, aes(x = age, fill = X)) + geom_histogram(bins = 50) +
  labs(title = "Distribution of age", fill = "Yearly income")+
  theme_bw()
#----------------------------------------------------------------------------------------------------------------------------------------
 #Scatter-plot

ggplot(data = census,aes(x = capital.gain, y = hours.per.week)) + geom_point()


ggplot(data = census,aes(x = capital.gain, y = hours.per.week)) + geom_point(alpha = 0.6,size = 2)


ggplot(data = census,aes(x = capital.gain, y = hours.per.week, col = X)) + geom_point(alpha = 0.6,size = 2)

ggplot(data = census,aes(x = capital.gain, y = hours.per.week, col = X)) + 
  geom_point(alpha = 0.6,size = 2)+
  labs(x = "Capital Gain", y= " Hours per Week",title = "Capital Gain vs Hours per Week ", col = "Yearly income")
#-----------------------------------------------------------------------------------------------------------------------------------------

#Box-plot

ggplot(data = census, aes(x = education,y = age)) + geom_boxplot()

ggplot(data = census, aes(x = education,y = age, fill = sex)) + geom_boxplot()

ggplot(data = census, aes(x = education,y = age, fill = sex)) + geom_boxplot()+
  labs(title ="Box-Plot of age by Education and Sex")

#-----------------------------------------------------------------------------------------------------------------------------------

#Linear-Regression

library(caTools)

set.seed(111)

sample.split(census$education.num, SplitRatio = 0.7) -> split_tag

subset(census, split_tag == TRUE) ->train

subset(census, split_tag == FALSE) -> test



l_model <- lm(hours.per.week ~ education.num, data = train)

summary(l_model)

options(scipen = 999)

pred_val <- predict(l_model, newdata = test)

head(pred_val)


cbind(Actual = test$hours.per.week,Predicted =  pred_val) -> final_data

final_data <- as.data.frame(final_data)

final_data$Actual - final_data$Predicted -> error

cbind(final_data,error) -> final_data

head(final_data)

sqrt(mean((final_data$error)^2))

plot(census$education.num,census$hours.per.week)
abline(l_model)

#--------------------------------------------------------------------------------------------------------
#Logistic Regression

library(caTools)

set.seed(123)

sample.split(census$X, SplitRatio = 0.65) -> split_tag

subset(census, split_tag == TRUE) ->train

subset(census, split_tag == FALSE) -> test



log_mod <- glm(X ~ occupation, data = train, family = "binomial")

summary(log_model)

pred_val <- predict(log_mod, newdata = test, type = "response")

head(pred_val)

range(pred_val)


library(ROCR)

prediction(pred_val,test$X) -> predict_log_roc

performance(predict_log_roc, "acc") -> acc

plot(acc)


range(pred_val)
table(census$X)

lm.pred <- ifelse(pred_val > 0.47, ">50K", "<=50K")

table(lm.pred,test$X) -> tab

sum(diag(tab))/ sum(tab)

library("caret")

confusionMatrix(factor(lm.pred), test$X)




performance(predict_log_roc, "tpr", "fpr") -> roc

plot(roc)

performance(predict_log_roc, "auc") -> auc
auc <- auc@y.values[[1]]
auc

#-----------------------------------------------------------------------------------------------------
#Multiple-Logistic-Regression

library(caTools)

set.seed(222)

sample.split(census$X, SplitRatio = 0.8) -> split_tag

subset(census, split_tag == TRUE) ->train

subset(census, split_tag == FALSE) -> test



log_mod <- glm(X ~ age + workclass + education, data = train, family = "binomial")

summary(log_mod)

pred_val <- predict(log_mod, newdata = test, type = "response")

head(pred_val)

range(pred_val)



library(ROCR)

prediction(pred_val,test$X) -> predict_log_roc

performance(predict_log_roc, "acc") -> acc

plot(acc)


range(pred_val)
table(census$X)

lm.pred <- ifelse(pred_val > 0.45, ">50K", "<=50K")

table(lm.pred,test$X) -> tab

sum(diag(tab))/ sum(tab)

confusionMatrix(factor(lm.pred), test$X)




performance(predict_log_roc, "tpr", "fpr") -> auc

plot(auc)

abline(a = 0, b = 1)


auc_ROCR <- performance(predict_log_roc,"auc")

auc_ROCR <- auc_ROCR@y.values[[1]]

auc_ROCR


predict(log_mod, newdata = data.frame(age = 52, workclass = "State-gov", education = "HS-grad"), type = "response")

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Decision-Tree

library(caTools)

set.seed(333)

sample.split(census$X, SplitRatio = 0.7) -> split_tag

subset(census, split_tag == TRUE) ->train

subset(census, split_tag == FALSE) -> test

library(rpart)
library(rpart.plot)

census_model <- rpart(formula = X ~ ., 
                      data = train, 
                      method = "class")

rpart.plot(x = census_model,type = 5, extra = 0,tweak = 1.2)


class_prediction <- predict(object = census_model,  
                            newdata = test,   
                            type = "class")  

tab <- table(Prediction = class_prediction,Actual = test$X)

sum(diag(tab))/ sum(tab)

confusionMatrix(class_prediction, test$X)




#--------------------------------------------------------------------------------------------------------------------------------
#Random-Forest

library(caTools)

set.seed(1)

sample.split(census$X, SplitRatio = 0.8) -> split_tag

subset(census, split_tag == TRUE) ->train

subset(census, split_tag == FALSE) -> test

library(randomForest)

set.seed(2)  

census_model <- randomForest(formula = X ~ ., 
                             data = train,
                             ntree = 300)

print(census_model)


plot(census_model)


class_prediction <- predict(object = census_model,  
                            newdata = test,   
                            type = "class")  






tab <- table(Prediction = class_prediction,Actual = test$X)

sum(diag(tab))/ sum(tab)


confusionMatrix(class_prediction,test$X)





