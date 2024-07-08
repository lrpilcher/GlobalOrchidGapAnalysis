library(dplyr)

#Loading WCVP Data
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
wcvp_names <- rWCVPdata::wcvp_names

# distribution data
wcvp_distributions <- rWCVPdata::wcvp_distributions

#filtering WCVP NAmes for Orchidaceae

orchid_names_wcvp <-
  dplyr::filter(wcvp_names, family=="Orchidaceae")

#exporting this data

write.csv (x = as.data.frame(orchid_names_wcvp), file = "orchid_names_wcvp.csv", sep=";")

#now lets do the distributions 

