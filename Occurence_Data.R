#rWCVP Package(s)
library(rWCVP)
library(rWCVPdata)
library(dplyr)
library(tidyverse)
library(gt)

#Creating a global Occurence matrix for Orchidaceae 
occurence_matrix <- wcvp_occ_mat(taxon="Orchidaceae",
                  
                  taxon_rank = "family",
                  
                  introduced=FALSE,
                  
                  extinct=FALSE,
                  
                  location_doubtful=FALSE)

View(occurence_matrix)

#creating a new dataframe with removed plant ID and Taxon Name
country_counts_matrix <- select(occurence_matrix, -plant_name_id, -taxon_name)
#Creating a loop to count number of species in each area code:

# Create an empty dataframe to store the sums
number_of_species_per_area <- data.frame(column = character(), sum = numeric())

# Loop through each column of the dataframe
for (col in names(country_counts_matrix)) {
  # Calculate the sum of the current column
  col_sum <- sum(country_counts_matrix[[col]])
  
  # Create a new row for the sum dataframe
  new_row <- data.frame(column = col, sum = col_sum)
  
  # Add the new row to the sum dataframe
  number_of_species_per_area <- rbind(number_of_species_per_area, new_row)
}

# Print the resulting dataframe
print(number_of_species_per_area)
view(number_of_species_per_area)


write.csv (x = as.data.frame(number_of_species_per_area), file = "Species_Per_Country.csv", sep=";")
