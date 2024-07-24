
# Install and load necessary packages
library(tidymodels)
library(dplyr)
library(yardstick)
library(vip)
library(pdp)

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
nominal_predictors <- c("region", "lifeform_category", "climate_description", "iucn_assessed", "redlist_category")

# Define the recipe
rec <- recipe(seedbank_conserved ~ region + lifeform_category + climate_description
              + iucn_assessed + threat_prediction_prob + redlist_category + number_of_regions, data = balanced_training) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_of(nominal_predictors), one_hot = TRUE)

prep_rec <- prep(rec)
bake(prep_rec, new_data = balanced_training) %>% names() 

preprocessed_training <- bake(prep_rec, new_data = balanced_training)

preprocessed_test <- bake(prep_rec, new_data = orchid_test)

# Define the Random Forest model
rf_model <- rand_forest(trees = 100) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Create workflows for each model
rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rec)

# Fit the model
trained_model <- rf_wf %>%
  fit(data = balanced_training)

# Make predictions on the test set
rf_predictions_class <- trained_model %>%
  predict(new_data = orchid_test, type = "class") %>%
  bind_cols(orchid_test)

rf_predictions_prob <- trained_model %>%
  predict(new_data = orchid_test, type = "prob") %>%
  bind_cols(orchid_test)

# Combine class and probability predictions
rf_predictions <- rf_predictions_prob %>%
  select(seedbank_conserved, .pred_Yes, .pred_No) %>%  # Ensure these are the correct names
  bind_cols(rf_predictions_class %>% select(.pred_class))


# Evaluate Predictions
# Use the yardstick functions correctly
conf_matrix <- conf_mat(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)
accuracy_score <- accuracy(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)
sensitivity_score <- sens(rf_predictions, truth = seedbank_conserved, estimate = .pred_class)
specificity_score <- spec(data = rf_predictions, truth = seedbank_conserved, estimate = .pred_class)


print(conf_matrix)
print(accuracy_score)
print(sensitivity_score)
print(specificity_score)

#ROC_AUC

# Calculate ROC AUC
roc_auc_score <- roc_auc(
  data = rf_predictions,
  truth = seedbank_conserved,  
  .pred_No                  
)

print(roc_auc_score)

roc_curve_data <- roc_curve(
  rf_predictions,
  truth = seedbank_conserved,
  .pred_No
)
# Plot ROC Curve with ggplot2
ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(roc_auc_score$.estimate, 2), ")"),
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_minimal()


# Extract the trained model
final_rf_model <- extract_fit_parsnip(trained_model)

# Use the vip package to create the variable importance plot
vip::vip(final_rf_model$fit)

vip_plot <- vip(final_rf_model$fit, num_features = 10, geom = "point", 
                aesthetics = list(size = 3, color = "blue"))
print(vip_plot)


#Looking at ggdist
install.packages("ggdist")
library(ggdist)
library(ggplot2)

is.data.frame(rf_predictions_prob)

climate_plot <- ggplot(rf_predictions_prob) +
  aes(y = climate_description, x = .pred_Yes) +
  stat_halfeye()
print(climate_plot)


threat_plot <- ggplot(rf_predictions_prob) +
  aes(y = threat_pred, x = .pred_Yes) +
  stat_halfeye()
print(threat_plot)

threat_plot2 <- ggplot(rf_predictions_prob, aes(x = threat_prediction_prob, y = number_of_regions)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot with Regression Line", x = "Threat Prediction Probability", y = "Number of regions covered") +
  theme_minimal()  # Set y-axis limits

print(threat_plot2)
