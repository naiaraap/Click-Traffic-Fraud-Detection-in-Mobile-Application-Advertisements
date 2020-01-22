# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Click Traffic Fraud Detection in Mobile Application Advertisements      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Fraud Detection project: Model creation
# In this script, the prediction model is created. 
# Then, the prediction for the test data set is made and the model is evaluated.
# Due to my machine's processing and memory limitations, I will work with random samples
# from the training and test datasets on proportion 70% training to 30% test
# to build and evaluate the model

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Fraud_Detection")

# Set seed
set.seed(123)

# Creating a model with random Forest
# load the library
library(randomForest)

# Getting the correct classification for test dataset
test_result <- read.csv("sample_submission.csv")

# Getting smaller samples of trein and test datasets
# on proportion 70% training to 30% test
index <- sample(1:nrow(train), 100000)
train_data <- train[index, ]

str(train_data)

index <- sample(1:nrow(test), 42857)
test_data <- test[index,]
test_data <- merge(test_data, test_result[index,], by.x="click_id", by.y="click_id")
test_data$click_id <- NULL


# Converting dependent variables to integer before creating model
# using random forest algorithm
cnames <- c('ip', 'app', 'device', 'os', 'channel')
for (var in cnames) {
  train_data[,var] <- as.integer(train_data[,var])
  test_data[,var] <- as.integer(test_data[,var])
}


# Emphasize the levels of the target variable
# since I used a random sample to build the model
levels(train_data$is_attributed) <- c("0", "1")

# Creating model
model <- randomForest( is_attributed ~ ip + app + device + os + channel + click_time,
                       data = train_data,
                       ntree = 100,
                       nodesize = 10)

# Print model
print(model)

# Generating predictions in test data
pred <- data.frame(observed = test_data$is_attributed,
                        predicted = predict(model, newdata = test_data[,1:6]))

pred$observed <- as.factor(pred$observed)
levels(pred$observed) = c("0", "1")
levels(pred$predicted) = c("0", "1")

# Visualizing the results
View(pred)

# Evaluating the model
# Generating a confusion matrix
caret::confusionMatrix(pred$observed, pred$predicted)


# The model created has the model created has an accuracy of 0.9975.
# However as the tests were done with a small sample 
# I cannot say that it is an efficient model.


