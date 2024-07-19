# Install and load necessary packages
library(tidymodels)
library(dplyr)
library(yardstick)  # For metrics

# Load data
orchiddata <- read.csv("Orchid_Model_Data.csv")

# Convert the outcome variable to a factor
orchiddata$seedbank_conserved <- as.factor(orchiddata$seedbank_conserved)

# Data resampling - create training and test datasets
set.seed(123)
orchid_split <- initial_split(orchiddata, prop = 0.75, strata = seedbank_conserved)

# Create training data
orchid_training <- training(orchid_split)

# Create test data
orchid_test <- testing(orchid_split)

# Count the number of instances per class
class_counts <- table(orchid_training$seedbank_conserved)
print(class_counts)

# Identify minority and majority classes
minority_class <- names(class_counts)[which.min(class_counts)]
majority_class <- names(class_counts)[which.max(class_counts)]

# Calculate number of additional samples needed
num_additional_samples <- class_counts[majority_class] - class_counts[minority_class]

set.seed(123)  # For reproducibility
minority_samples <- orchid_training %>%
  filter(seedbank_conserved == minority_class) %>%
  slice_sample(n = num_additional_samples, replace = TRUE)

balanced_training <- bind_rows(
  orchid_training %>% filter(seedbank_conserved == majority_class),
  minority_samples
)

# Check the class distribution after oversampling
print(table(balanced_training$seedbank_conserved))

# Check the number of rows
print(nrow(orchid_training))
print(nrow(orchid_test))

# Identify nominal predictors
nominal_predictors <- c("region", "lifeform_category", "climate_description", "iucn_assessed", "threat_pred")

# Define the recipe
rec <- recipe(seedbank_conserved ~ region + lifeform_category + climate_description
              + iucn_assessed + threat_pred, data = balanced_training) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_of(nominal_predictors), one_hot = TRUE)

prep_rec <- prep(rec)
bake(prep_rec, new_data = balanced_training) %>% names() 

preprocessed_training <- bake(prep_rec, new_data = balanced_training)
print(names(preprocessed_training))# Define the Random Forest model
rf_model <- rand_forest(trees = 50) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Create workflows for each model
rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rec)

# Fit the model
trained_model <- rf_wf %>%
  fit(data = balanced_training)

# Make predictions on the test set
rf_predictions <- trained_model %>%
  predict(new_data = orchid_test) %>%
  bind_cols(orchid_test)

# Evaluate Predictions
# Use the `yardstick` functions correctly
conf_matrix <- conf_mat(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)
accuracy_score <- accuracy(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)
roc_auc_score <- roc_auc(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)

print(conf_matrix)
print(accuracy_score)
print(roc_auc_score)


names(balanced_training)
names(orchid_test)
