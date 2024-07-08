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

#Working out how many species there are per genera 
#First we filter by Orchidaceae
orchidaceae_wcvp <- 
  dplyr::filter(wcvp_names, family=="Orchidaceae")

#Next we count how many in each Genera 
wcvp_genera_count <- orchidaceae_wcvp %>% group_by(genus) %>% summarise(count = n())
print(wcvp_genera_count)


write_csv2(wcvp_genera_count, file = "WCVP Orchid Genera Count.csv")

?write_csv

redlist_category_count <- cleaned_IUCN_summary %>% group_by(redlistCategory) %>% summarise(count = n())
print(redlist_category_count)