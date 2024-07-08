#WVCP Data Package
install.packages("rWCVPdata", repos=c("https://matildabrown.github.io/drat", "https://cloud.r-project.org"))

#Checking the version of WCVP
rWCVPdata::wcvp_check_version()
#Citation
citation("rWCVPdata")


#WCVP PAckage
install.packages("rWCVP")

# taxonomy data
names <- rWCVPdata::wcvp_names

# distribution data
distributions <- rWCVPdata::wcvp_distributions

#Loading Libraries
library(tidyverse)
library(rWCVPdata)
library(rWCVP)
library(gt)
library(ggalluvial)


#Importing MSBP Orchidaceae Data
#Data was downloaded on Monday 29th April 
msbp_orchidaceae <- read.csv("MSBP_Orchidaceae_Raw_Updated.csv")
view(msbp_orchidaceae)

#Matching names
matches <- wcvp_match_names(msbp_orchidaceae,
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
write_csv(fuzzy_matches, "msbp-fuzzy-tocheck_updated.csv")

#Reading the manually checked file back in
fuzzy_checked <-
  read_csv("msbp-fuzzy-checked_updated.csv",
           show_col_types=FALSE) %>%
  select(-keep) %>%
  mutate(resolved_match_type=ifelse(! is.na(resolved_match_type),
                                    resolved_match_type,
                                    match_type))

checked_matches <- matches %>%
  filter(! str_detect(match_type, "Fuzzy")) %>%
  bind_rows(fuzzy_checked)



#From inspecting the data, there are only 5 which have multiple matches, which are
#solved by filtering out illegitimate matches anyway. 

manually_resolved <- filter(checked_matches, 
                            wcvp_status != "Illegitimate", 
                            wcvp_status != "Invalid")


resolved_matches <- manually_resolved


#Linking assessments to accepted names, e.g. dealing with synonyms
# a for accepted
accepted_matches <- resolved_matches %>%
  left_join(rWCVPdata::wcvp_names, by=c("wcvp_accepted_id"="plant_name_id")) %>%
  mutate(keep=case_when(
    taxon_status == "Accepted" & (wcvp_status != "Synonym" | wcvp_homotypic) ~
      "Matched to an accepted name",
    TRUE ~ "Not matched to an accepted name"
  ))

count(accepted_matches, keep)

#Final Dataset
final_matches <-
  accepted_matches %>%
  filter(keep == "Matched to an accepted name") %>%
  select(scientificName, authority,
         match_name=wcvp_name, match_status=wcvp_status,
         accepted_plant_name_id=wcvp_accepted_id, ipni_id,
         accepted_taxon_name=taxon_name, accepted_taxon_authors=taxon_authors,
         geographic_area, lifeform_description, climate_description, 
         AccessionNumber, Seedbank, CountryName, LocationSummary)

is.data.frame(final_matches)
write.csv (x = as.data.frame(final_matches), file = "cleaned_msbp_orchidaceae.csv", sep=",")

