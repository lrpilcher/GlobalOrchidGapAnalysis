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

#Importing Red List Data - Assessments
#Red List Data was downloaded on the 28th March 2024

redlistdata_assessments <- read.csv("RL-assessments-raw-data.csv")
