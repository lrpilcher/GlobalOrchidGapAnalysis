#Creating Threat Probabilities
#library
install.packages("boot")
library(dplyr)
library(boot)

# Example dataframe with NA values
set.seed(123)  # for reproducibility

#read in main database
main_database <- read.csv("Main_Database.csv")

# Ensure Min_Prob and Max_Prob are numeric (convert if necessary)
main_database$min_probability <- as.numeric(main_database$min_probability)
main_database$max_probability <- as.numeric(main_database$max_probability)

# Function to sample from the range, handling NA values
sample_from_range <- function(min_probability, max_probability) {
  if (is.na(min_probability) || is.na(max_probability)) {
    return(NA)
  } else {
    return(runif(1, min_probability, max_probability))
  }
}

# Placeholder dataset for boot function
placeholder_data <- data.frame(ID = 1)

# Function to bootstrap the sampling process for each species and return the mean
bootstrap_sampling_mean <- function(min_probability, max_probability, num_bootstraps = 100) {
  if (is.na(min_probability) || is.na(max_probability)) {
    return(NA)
  } else {
    # Define a function to sample once from the range
    single_sample <- function(data, indices) {
      return(sample_from_range(min_probability, max_probability))
    }
    # Perform bootstrapping
    boot_results <- boot(data = placeholder_data, statistic = single_sample, R = num_bootstraps)
    # Return the mean of the bootstrapped samples
    return(mean(boot_results$t, na.rm = TRUE))
  }
}

# Apply bootstrapping process to each row and create a new column for the bootstrapped mean
main_database$Bootstrapped_Prob <- mapply(bootstrap_sampling_mean, main_database$min_probability, main_database$max_probability)


# Print the dataframe to verify
print(head(main_database))

write.csv(main_database, "main_database_v2.csv")

