# rWCVP - orchidaceae checklist
library(rWCVP)
library(dplyr)

# use WCVP to get the accepted list of orchids with geography
# gives 72,373 rows, but many species occure in multiple regions
orchid_fam <- wcvp_checklist(
  taxon = "Orchidaceae",
  taxon_rank = "family",
  area_codes = NULL,
  synonyms = FALSE,
  render_report = FALSE,
  native = TRUE,
  infraspecies = FALSE,
  introduced = FALSE,
  extinct = TRUE,
  location_doubtful = FALSE,
  hybrids = FALSE,
)

# filter out the duplicates to get the list of accepted species
unique_species <- orchid_fam %>% distinct(accepted_name, .keep_all = TRUE)

#Removing the columns we don't need by subsetting the data

orchid_baselist <- subset.data.frame(unique_species, c('plant_name_ID', 
                                            'ipni_id', 
                                            'accepted_name',
                                            'taxon_status', 
                                            'primary_author',
                                            'lifeform_description',
                                            'climate_description',
                                            'geographic_area'
                                            ))
orchid_baselist <- unique_species %>% select(-family, -genus_hybrid,-species_hybrid, -species,
                          -infraspecific_rank, -infraspecies, -parenthetical_author, 
                          -primary_author, -publication_author, -place_of_publication,
                          -volume_and_page, -place_of_publication, -first_published,
                          -nomenclatural_remarks, -replaced_synonym_author, -homotypic_synonym, 
                          -hybrid_formula, -reviewed, -plant_locality_id, -continent_code_l1, -continent,
                          -region_code_l2, -area_code_l3, -area, -occurrence_type, -in_geography, 
                          -endemic, -area_endemic, -parent_plant_name_id, -basionym_plant_name_id)


# report the WCVP version using this:
rWCVPdata::wcvp_version()

#Exporting the Baselist 
write.csv(orchid_baselist, "Orchid_Baselist.csv")
