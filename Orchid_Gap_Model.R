#Load Packages - Tidymodels and dplyr packages
library(dplyr)
library(tidymodels)

#Load Data 
orchiddata <- read.csv("Orchid_Model_Data.csv")

#converting outcome variable to a factor
orchiddata$seedbank_conserved <- as.factor(orchiddata$seedbank_conserved)


#Data Re sampling - creating training and test datasets
orchid_split <- initial_split(orchiddata, prop = 0.75,
                               strata = seedbank_conserved)

#Create the training data
orchid_training <- orchid_split %>% 
  training()

#Create the test data
orchid_test <- orchid_split %>% 
  testing()


#Check the number of rows
nrow(orchid_training)
nrow(orchid_test)


#Fitting logistic regression model
#Specify a logistic regression model
logistic_model <- logistic_red() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

#Building the Feature Engineering Pipeline
#Specify Feature Engineering Steps with 'Recipes'.
orchids_recipe <- recipe(seedbank_conserved ~ region + lifeform_category + climate_description +
                           threat_pred + redlist_category,
                         data = orchid_training) %>%
  step_corr(all_numeric(), threshold = 0.9) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes())


#Recipe training
orchid_recipe_prep <- orchids_recipe %>%
  prep(training = orchid_training)

#Preprocess training data
#Apply our trained recipe to the training data and store results in orchid_training_prep
orchids_training_prep <- orchid_recipe_prep %>%
  bake(new_data = NULL)
orchids_training_prep

#Preprocess test data
orchids_test_prep <- orchid_recipe_prep %>%
  bake(new_data = orchid_test)
orchids_test_prep


#Model fitting and predictions
# Fit to training data using preprocessed training data
logistic_fit <- logistic_model %>% 
  fit(seedbank_conserved ~ region + lifeform_category + climate_description +
        threat_pred + redlist_category,
      data = orchids_training_prep)

# Obtain model predictions
class_preds <- predict(logistic_fit, new_data = orchids_test_prep,
                       type = 'class')

prob_preds <- predict(logistic_fit, 
                      new_data = orchids_test_prep, 
                      type = 'prob')


#combine actual outcome variable from test dataset
orchid_results <- orchid_test %>%
  select(seedbank_conserved) %>%
  bind_cols(class_preds, prob_preds)

#produces model results dataframe
orchid_results 

#model evaluation
#confusion matrix 
orchid_results %>%
  conf_mat(truth = seedbank_conserved, estimate = .pred_class)



