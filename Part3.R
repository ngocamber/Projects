# 1. IMPORTING LIBRARIES
library(ggplot2)
library(dplyr)
library(MASS)
library(caret)
library(caTools)
library(e1071)
library(partykit)
library(randomForest)
library(ROSE)

1# 2. IMPORTING DATA ---------------------------------------------------------
setwd('C:/Users/LENOVO/Documents/year3/ML Projects')
data <- read.csv('bank.csv', sep=';',stringsAsFactors = TRUE)
summary(data)
dim(data)
#  4521   17

# 3. DATA PREPARATION --------------------------------------------------------
# Remove variable 'duration'
drops <- c("duration")
data <- data[ , !(names(data) %in% drops)]

# Check null values
data %>% 
  is.na() %>% 
  colSums()
## No missing values found

# Check proportion of categorical data
prop.table(table(data$y))
##      no     yes 
## 0.88476 0.11524 

# Train-test split
set.seed(2020)

train <- sample.split(Y=data$y, SplitRatio = 0.7)
trainset <- subset(data, train==T)
testset <- subset(data, train==F) 

# Sample the trainset
trainset <- ovun.sample(y ~ ., data = trainset, 
                                  method = "both", p=0.5,
                                  N=3165, seed = 1)$data
prop.table(table(trainset$y))
##        no       yes 
##0.5191153 0.4808847

# Scale data
num_col<- sapply(trainset, is.numeric)
trainset[num_col] <- lapply(trainset[num_col], scale)


# 4. MODELING ----------------------------------------------------------------
# 4.1 NAIVE BAYES
naive <- naiveBayes(x = trainset %>% dplyr::select(.,-y),
                         y = trainset$y, probability = TRUE)
naive

# Predicting
naive_pred <- predict(object = naive,
                           newdata = testset,
                           type = 'class')
naive_pred
# Confusion matrix
naive_cm <- confusionMatrix(data = naive_pred,
                reference = testset$y)
accuracy <- naive_cm$overall["Accuracy"]

# 4.2 DECISION TREE
dt <- ctree(formula = y ~.,
                 data = trainset)
dt

# Prediction
dt_pred <- predict(object = dt,
                        newdata = testset,
                        type = "response")
dt_pred
# ConfusionMatrix
dt_cm <- confusionMatrix(data = dt_pred,
                reference = testset$y)
accuracy <- c(accuracy, dt_cm$overall['Accuracy'])


# 4.3 RANDOM FOREST 
# Remove low variance predictors
n0_var <- nearZeroVar(data)
data_new <- data[,-n0_var]

# Train-test split
set.seed(2020)
train_rf <- sample.split(Y=data_new$y, SplitRatio = 0.7)
trainset_rf <- subset(data_new, train==T)
testset_rf <- subset(data_new, train==F) 

# Sample the trainset
trainset_rf <- ovun.sample(y ~ ., data = trainset_rf, 
                        method = "both", p=0.5,
                        N=3165, seed = 1)$data

# Scale data
num_col<- sapply(trainset_rf, is.numeric)
trainset_rf[num_col] <- lapply(trainset_rf[num_col], scale)

# Model
rf <- randomForest(y ~., data=trainset_rf)

graphics.off()
plot(rf)

# Prediction
rf_pred <- predict(object = rf,
                        newdata = testset_rf,
                        type = "response")
rf_pred

# Confusion Matrix
rf_cm <- confusionMatrix(data = rf_pred,
                reference = testset_rf$y)
rf_cm
accuracy <- c(accuracy, rf_cm$overall['Accuracy'])


# 5. RESULT TABLE ------------------------------------------------------------
models <- c('Naive Bayes', 'Decision Tree', 'Random Forest')
results <- data.frame(models, accuracy)
View(results)



