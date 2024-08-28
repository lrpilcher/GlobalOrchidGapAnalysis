library(rWCVP)
library(dplyr)


# use WCVP to get the accepted list of orchids with geography
# gives 72,373 rows, but many species occur in multiple regions
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

write.csv(orchid_fam, file = "orchid_regions.csv")

