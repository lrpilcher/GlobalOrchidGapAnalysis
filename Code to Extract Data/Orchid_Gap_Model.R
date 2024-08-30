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
print(nrow(balanced_training))
print(nrow(orchid_test))

# Identify nominal predictors
nominal_predictors <- c("lifeform_category", "climate_description", "iucn_assessed", "redlist_category", "region")

# Define the recipe
rec <- recipe(seedbank_conserved ~ region + lifeform_category + climate_description   + iucn_assessed + threat_prediction_prob + redlist_category + number_of_regions, data = balanced_training) %>%
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
specificity_score <- specificity(data = rf_predictions, truth = seedbank_conserved, estimate = .pred_class)


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

# Use the vip package to create the variable importance plot
# Extract the trained model
final_rf_model <- extract_fit_parsnip(trained_model)

vip::vip(final_rf_model$fit)

vip_plot <- vip(final_rf_model$fit, num_features = 10, geom = "point", 
                aesthetics = list(size = 3, shape = 17, color = "darkgreen")) +
  theme_light() 
print(vip_plot)

install.packages("iml")

#Applying the model to the whole dataset to gain predictions

preprocessed_entire_data <- bake(prep_rec, new_data = orchiddata)

rf_predictions_entire_class <- trained_model %>%
  predict(new_data = orchiddata, type = "class") %>%
  bind_cols(orchiddata)

rf_predictions_entire_prob <- trained_model %>%
  predict(new_data = orchiddata, type = "prob") %>%
  bind_cols(orchiddata)

rf_predictions_entire <- rf_predictions_entire_prob %>%
  select(seedbank_conserved, .pred_Yes, .pred_No) %>%
  bind_cols(rf_predictions_entire_class %>% select(.pred_class))


print(rf_predictions_entire)


write.csv(rf_predictions_entire_prob, "Orchid_Model_Predictions.csv", row.names = FALSE)


#Creating confusion matrix for entire
conf_matrix_entire <- conf_mat(rf_predictions_entire, truth = seedbank_conserved, estimate = .pred_class)
print(conf_matrix_entire)
#Looking at ggdist
install.packages("ggdist")
install.packages("hexbin")
library(ggdist)
library(ggplot2)
library(hexbin)

is.data.frame(rf_predictions_entire)
#Climate Figure
climate_plot <- ggplot(rf_predictions_entire_prob) +
  aes(y = climate_description, x = .pred_Yes) +
  stat_halfeye() +
  xlab("Probability that Species can be Banked") +
  ylab("Climate Description") +
  theme_light()
  
print(climate_plot)
ggsave("climate_plot.png", width = 6, height = 4, dpi = 300)


#Important Regions
important_regions <- subset(rf_predictions_entire_prob, region %in% 
                              c('Australia', 'Western Indian Ocean',  'Western South America', 'Northern Europe'))

region_plot <- ggplot(important_regions) +
  aes(y = .pred_Yes, x = region) +
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", size=2, color = "red") +
  xlab("Region") +
  ylab("Probability that Species can be Banked") +
  theme_light()
print(region_plot) 
ggsave("regions_plot.png", width = 6, height = 4, dpi = 300)

#Threat Plot
threat_plot3 <- ggplot(rf_predictions_entire_prob, aes(x = .pred_Yes, y = threat_prediction_prob)) +
  geom_hex(bins = 25) +
  xlab("Probability that Species can be Banked") +
  ylab("Threat Probability") +
  theme_light()

print(threat_plot3)
ggsave("threat_plot.png", width = 6, height = 4, dpi = 300)

#Lifeform plot
#transforming woody perreials to terrestrial as that is what they are:
rf_predictions_entire_prob$lifeform_category[rf_predictions_entire_prob$lifeform_category == "woody perennial"] <- "herbaceous perennial"
rf_predictions_entire_prob$lifeform_category[rf_predictions_entire_prob$lifeform_category == "herbaceous perennial"] <- "terrestrial"

lifeform_plot <- ggplot(rf_predictions_entire_prob) +
  aes(y = .pred_Yes, x = lifeform_category) +
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", size=2, color = "red") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(..y.., 2)),
    vjust = -0.5, 
    hjust = -0.4,
    color = "black") +
  xlab("Lifeform Category") +
  ylab("Probability that Species can be Banked") +
  theme_light()

print(lifeform_plot)
ggsave("lifeform_plot.png", width = 6, height = 4, dpi = 300)

citation()
R.Version()
