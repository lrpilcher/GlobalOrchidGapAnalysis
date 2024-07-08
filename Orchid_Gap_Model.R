#Running Models to predict conservation status
#1 - Load Packages - Tidymodels and dplyr packages
library(dplyr)
library(tidymodels)

#2 - Load Data 
orchiddata <- read.csv2()

#3 - Split Data into Training and Testing Data
# Create data split object
orchid_split <- initial_split(orchiddata, prop = 0.75,
                               strata = tbc)

# Create the training data
orchid_training <- orchid_split %>% 
  training()

# Create the test data
orchid_test <- orchid_split %>% 
  testing()

# Check the number of rows
nrow(orchid_training)
nrow(orchid_test)

#4 - Fitting logistic regression model

# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
  # Set the engine
  set_engine('glm') %>% 
  # Set the mode
  set_mode('classification')

# Fit to training data
logistic_fit <- logistic_model %>% 
  fit(conservation_status ~ predictor1 + predictor2 + predictor3,
      data = orchid_training)

# Print the model specification
logistic_model

# Predict outcome categories
class_preds <- predict(logistic_fit, new_data = orchid_test,
                       type = 'class')

# Obtain estimated probabilities for each outcome value
prob_preds <- predict(logistic_fit, new_data = orchid_test_test, 
                      type = 'prob')

# Combine test set results
orchid_results <- orchid_test %>% 
  select(tbc) %>% 
  bind_cols(prob_preds,class_preds)

# View results tibble
orchid_results
#Above can be put into a confusion matrix 


# Create a custom metric function
orchid_metrics <- metric_set(accuracy, sens, spec)

# Calculate metrics using model results tibble
orchid_metrics(orchid_results, truth = tbc,
                estimate = .class_preds)

# Create a confusion matrix
conf_mat(orchid_results,
         truth = tbc,
         estimate = .class.preds) %>% 
  # Pass to the summary() function
  summary()