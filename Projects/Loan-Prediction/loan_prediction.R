
customer_loan_details <- read.csv("C:/Users/Intell-shambhavi/Desktop/October/loan_prediction/cutomer_loan.csv", sep = ",")

# Print the structure of the dataframe
str(customer_loan_details)
head(customer_loan_details)

# Check for the NA values
any(is.na(customer_loan_details))

# Calculating Debt-to-Income Ratio
customer_loan_details$dti <- (customer_loan_details$debts/customer_loan_details$income)*100

# Create loan_decision_status variable which is our target variable to use for loan prediction
customer_loan_details$loan_decision_status <- ifelse(customer_loan_details$loan_decision_type == 'Denied', 0, 1)

# Encoding the target variable as factor
customer_loan_details$loan_decision_status <- factor(customer_loan_details$loan_decision_status, levels = c(0, 1))

#Selecting the required fields for prediction
customer_loan_refined <- customer_loan_details[,c(3,4,6:8,11,13:14)]
head(customer_loan_refined)

# Encoding the categorical variable as factors
customer_loan_refined$gender <- as.numeric(factor(customer_loan_refined$gender,
                                                  levels = c('Male','Female'),
                                                  labels = c(1,2)))

customer_loan_refined$marital_status <- as.numeric(factor(customer_loan_refined$marital_status,
                                                          levels = c('Divorced','Married','Single'),
                                                          labels = c(1,2,3)))

customer_loan_refined$occupation <- as.numeric(factor(customer_loan_refined$occupation,
                                                      levels = c('Accout','Business','IT','Manager','NYPD'),
                                                      labels = c(1,2,3,4,5)))

customer_loan_refined$loan_type <- as.numeric(factor(customer_loan_refined$loan_type,
                                                     levels = c('Auto','Credit','Home','Personal'),
                                                     labels = c(1,2,3,4)))

head(customer_loan_refined)
# Splitting the customer_loan_refined dataset into training and test sets
library(caTools)
set.seed(123)
split = sample.split(customer_loan_refined$loan_decision_status, SplitRatio = 0.70)
training_set = subset(customer_loan_refined, split == TRUE)
test_set = subset(customer_loan_refined, split == FALSE)

#Applying Feature Scaling
training_set[-8] = scale(training_set[-8])
test_set[-8] = scale(test_set[-8])

head(training_set)

# Applying Dimensionality reduction using PCA to training and test sets
# install.packages("caret")
library(caret)
pca = preProcess(x = training_set[-8], method = 'pca', pcaComp = 2)
training_set_pca = predict(pca, training_set)
training_set_pca = training_set_pca[c(2, 3, 1)]
test_set_pca = predict(pca, test_set)
test_set_pca = test_set_pca[c(2, 3, 1)]
head(test_set_pca)

# Appling Naive Bayes classification model to predict the loan
# install.packages("e1071")
library(e1071)
classifier = naiveBayes(x = training_set_pca[-3], y = training_set_pca$loan_decision_status)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set_pca[-3])

# confusionMatrix to calculate accuracy
confusionMatrix(table(test_set_pca[, 3], y_pred))

