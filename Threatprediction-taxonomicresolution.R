#WVCP Data Package
if (!require(rWCVPdata)) {
  install.packages("rWCVPdata",
                   repos = c(
                     "https://matildabrown.github.io/drat",
                     "https://cloud.r-project.org"
                   )
  )
}

install.packages("rWCVP")

# taxonomy data
names <- rWCVPdata::wcvp_names

# distribution data
distributions <- rWCVPdata::wcvp_distributions

#Loading Libraries
library(tidyverse)
library(rWCVP)
library(gt)
library(ggalluvial)

#Importing Threat Prediction Data
orchidaceae_threatpred <- read.csv("AngiospermThreat_ForR.csv")
view(orchidaceae_threatpred)

#Matching names
matches <- wcvp_match_names(orchidaceae_threatpred,
                            name_col="scientificName",
                            author_col="authority",
                            fuzzy=TRUE,
                            progress_bar=FALSE)
view(matches)

#Resolving matched names to accepted species
#Manually verify anything that is <90% similar
#>90% similar, we keep it
#If it is only one letter out and is over >90% similar we will keep it

fuzzy_matches <- matches %>%
  filter(str_detect(match_type, "Fuzzy")) %>%
  mutate(
    keep = case_when( #set up a keep column
      match_similarity < 0.9 ~ NA_real_, # fill with blank for dissimilar names
      wcvp_author_edit_distance == 0 ~ 1, # fill with 1 if authors identical
      match_edit_distance == 1 ~ 1, # fill with 1 if only one letter different
    )
  )

#how many did this resolve?
table(fuzzy_matches$keep, useNA = "always")

#Manually Checking
write_csv(fuzzy_matches, "threatpred-fuzzy-tocheck.csv")
