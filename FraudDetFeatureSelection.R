# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Click Traffic Fraud Detection in Mobile Application Advertisements      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Fraud Detection project: Feature selection
# In this script the selection of characteristics
# is performed for the creation of the model

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Fraud_Detection")

# Set seed
set.seed(123)

# Feature selection using randomForest package
# load the library
library(randomForest)

# Converting target variable to factor
train$is_attributed <- as.factor(train$is_attributed)

# Converting dependents variables to integer to run random forest algorithm
cnames <- c('ip', 'app', 'device', 'os', 'channel')
for (var in cnames) {
  train[,var] <- as.integer(train[,var])
  test[,var] <- as.integer(test[,var])
}


# Due to my machine's processing and memory limitations, I will work with random samples 
# from the training and test datasets to build and evaluate the model
index <- sample(1:nrow(train), 100000)
train_data <- train[index,]

# Creating random forest model measuring importance
model <- randomForest( is_attributed ~ ip + app + device + os + channel + click_time,
                       data = train_data,
                       ntree = 100,
                       nodesize = 10,
                       importance = TRUE)


# Plotting estimate variable importance
varImpPlot(model)

# The dataset has few variables so I will make the 
# first version using all but (attribute_time)



