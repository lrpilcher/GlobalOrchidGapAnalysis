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


#Importing Red List Data - Taxonomy
#Red List Data was downloaded on the 28th March 2024
redlistdata_taxonomy <- read.csv("IUCNredlisttaxonomy.csv")
view(redlistdata_taxonomy)

matches <- wcvp_match_names(redlistdata_taxonomy,
                            name_col="scientificName",
                            author_col="authority",
                            fuzzy=TRUE,
                            progress_bar=FALSE)

view(matches)

#saving 
write.csv(matches, "RedList_TaxonomicResolution.csv")

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

#Manually checking the 'fuzzy matches'
write_csv(fuzzy_matches, "redlist-fuzzy-tocheck.csv")

#Reading the manually checked file back in
fuzzy_checked <-
  read_csv("redlist-fuzzy-checked.csv",
           show_col_types=FALSE) %>%
  select(-keep) %>%
  mutate(resolved_match_type=ifelse(! is.na(resolved_match_type),
                                    resolved_match_type,
                                    match_type))

checked_matches <- matches %>%
  filter(! str_detect(match_type, "Fuzzy")) %>%
  bind_rows(fuzzy_checked)

#Now time to deal with multiple matches
#Resolve multiple matches function 
# resolve multiple matches function ####
resolve_multi <- function(df) {
  if (nrow(df) == 1) {
    return(df)
  }
  
  # some fuzzy matches are rejected from the previous section
  valid_matches <- filter(df, !is.na(match_similarity))
  
  if (nrow(valid_matches) == 0) {
    return(head(df, 1))
  }
  
  matching_authors <-
    valid_matches %>%
    filter(wcvp_author_edit_distance == 0 | ! sum(wcvp_author_edit_distance == 0,
                                                  na.rm=TRUE))
  
  if (nrow(matching_authors) == 1) {
    return(matching_authors)
  }
  
  accepted_names <-
    matching_authors %>%
    filter(wcvp_status == "Accepted" | ! sum(wcvp_status == "Accepted"))
  
  if (nrow(accepted_names) == 1) {
    return(accepted_names)
  }
  
  synonym_codes <- c("Synonym", "Orthographic", "Artificial Hybrid", "Unplaced")
  synonyms <-
    accepted_names %>%
    filter(wcvp_status %in% synonym_codes | ! sum(wcvp_status %in% synonym_codes))
  
  if (nrow(synonyms) == 1)  {
    return(synonyms)
  }
  
  n_matches <- length(unique(synonyms$wcvp_accepted_id)) / nrow(synonyms)
  final <- head(synonyms, 1)
  
  if (n_matches != 1) {
    final <-
      final %>%
      mutate(
        across(wcvp_id:resolved_match_type & where(is.numeric), ~NA_real_),
        across(wcvp_id:resolved_match_type & where(is.character), ~NA_character_),
        resolved_match_type="Could not resolve multiple matches"
      )
  }
  
  final
}

#now going through each name with multiple matches
auto_resolved <-
  checked_matches %>%
  nest_by(scientificName) %>%
  mutate(data=list(resolve_multi(data))) %>%
  unnest(col=data) %>%
  ungroup()

auto_resolved <-
  auto_resolved %>%
  mutate(resolved_match_type=case_when(
    is.na(resolved_match_type) & is.na(match_type) ~ "No match found",
    is.na(resolved_match_type) ~ match_type,
    TRUE ~ resolved_match_type
  ))

#How did the automatic resolution do?
count(auto_resolved, resolved_match_type)

#5 names we couldn't find a match for

#Going through these 5:
auto_resolved %>%
  filter(resolved_match_type %in% c("No match found","Fuzzy match rejected")) %>%
  write_csv("redlist_tomanuallymatch.csv")

#Couldn't find any matches for these 5
manually_resolved <- read_csv("redlist_manually_matched.csv",
                              show_col_types=FALSE)
count(manually_resolved, resolved_match_type)

#add back into results and remove anything which remains unmatched
resolved_matches <-
  manually_resolved %>%
  select(-c(match_type, multiple_matches,
            match_similarity, match_edit_distance)) %>%
  bind_rows(
    auto_resolved %>%
      filter(! scientificName %in% manually_resolved$scientificName) 
  )

#We want to remove resolved_match_type where no valid match is found

resolved_matches_unpaired <- resolved_matches[!is.na(resolved_matches$match_type), ]
view(resolved_matches_unpaired)

#Linking assessments to accepted names, e.g. dealing with synonyms

# a for accepted
accepted_matches <- resolved_matches_unpaired %>%
  left_join(rWCVPdata::wcvp_names, by=c("wcvp_accepted_id"="plant_name_id")) %>%
  mutate(keep=case_when(
    taxon_status == "Accepted" & (wcvp_status != "Synonym" | wcvp_homotypic) ~
      "Matched to an accepted name",
    TRUE ~ "Not matched to an accepted name"
  ))

count(accepted_matches, keep)

#Homotypic Synonyms
# a for accepted
accepted_matches <- resolved_matches_unpaired %>%
  left_join(rWCVPdata::wcvp_names, by=c("wcvp_accepted_id"="plant_name_id")) %>%
  mutate(keep=case_when(
    taxon_status == "Accepted" & (wcvp_status != "Synonym" | wcvp_homotypic) ~
      "Matched to an accepted name",
    TRUE ~ "Not matched to an accepted name"
  ))

#Final Dataset
final_matches <-
  accepted_matches %>%
  filter(keep == "Matched to an accepted name") %>%
  select(scientificName, authority,
         match_name=wcvp_name, match_status=wcvp_status,
         accepted_plant_name_id=wcvp_accepted_id, ipni_id,
         accepted_taxon_name=taxon_name, accepted_taxon_authors=taxon_authors)

glimpse(final_matches)

#Yay final Data!!!
view(final_matches) 

#checking its a dataframe
is.data.frame(final_matches)
write.csv (x = as.data.frame(final_matches), file = "cleanedredlisttaxonomy.csv", sep=",")

