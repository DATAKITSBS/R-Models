library(tidyverse)
library(caret)
library(xgboost)

training_data <- read_csv("../input/train.csv", )

set.seed(1234)
# index for splitting data in testing & training
trainIndex <- createDataPartition(training_data$SalePrice, p = .8, 
                                  list = FALSE, 
                                  times = 1)

# convert training data to matrix
training_data_matrix <- training_data %>%
    select(-SalePrice) %>%
    mutate_if(is.character, as.factor) %>% # convert strings to factors
    mutate_if(is.factor, as.numeric) %>% # label encode categorical factors
    select_if(is.numeric) %>%
    as.matrix()

# train XGBoost model with training data
xgboost_model <- xgboost(data = training_data_matrix[trainIndex,],
        label = training_data$SalePrice[trainIndex],
        nrounds = 40, eval_metric="mae") # using same metric as comp.



# generate predictions for our held-out testing data
pred <- predict(xgboost_model, training_data_matrix[-trainIndex, ])

# get & print the classification error
err <- mean(pred - training_data$SalePrice[-trainIndex])
print(paste("test-error=", err))
# read in test data (won't have labels)
test_data <- read_csv("../input/test.csv")

# convert testing data to matrix (using same techniques
# we used for our training data)
test_data_matrix <- test_data %>%
    # convert strings to factors
    mutate_if(is.character, as.factor) %>% 
    # label encode categorical factors
    mutate_if(is.factor, as.numeric) %>% 
    select_if(is.numeric) %>%
    as.matrix()

# predict prices
submission_predictions <- predict(xgboost_model, 
                                  test_data_matrix)

# format csv as specificed in competition discription
prediction_file <- tibble(Id = test_data$Id, 
                         SalePrice = submission_predictions)

# save file
write_csv(prediction_file, "submission.csv")