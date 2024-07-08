#H1 - Are Threatened Orchids Conserved?
#Installing Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(IUCNpalette)

#Reading In Data
#Threat Predictions
angiosperm_threat_predictions <- 
  read.csv("Angiosperm_threat_predictions_v1.csv")

#Cleaned IUCN Red List Summary
cleaned_IUCN_summary <- read.csv("cleanedredlistsummary.csv")

#Cleaned MSBP Data
msbp_orchidaceae <- read.csv("cleaned_msbp_orchidaceae.csv")

#How many Orchidaceae are predicted to be threatened in the Angiosperm Threat
#predictions dataset?

#Filtering for the Orchidaceae family
orchidaceae_threat_predictions <- dplyr::filter(angiosperm_threat_predictions,
                                                family=="Orchidaceae")

#next we filter for where confidence in predictions is 'confident'
orchidaceae_threat_predictions_confident <- 
  dplyr::filter(orchidaceae_threat_predictions, confidence=="confident")


#How many species does this give us?
tp_species_count <- length(orchidaceae_threat_predictions_confident$taxon_name)
print(tp_species_count)
#25,127 species predictions! 

#How many are threatened and how many are non threatened?
tp_category_counts <- table(orchidaceae_threat_predictions_confident$predicted_threat)
print(tp_category_counts)
#7470 of these are predicted to be not threatened while 17657 are predicted to be threatened


#creating a quick dataframe:
tp_category_counts_df <- data.frame(
  category=c("Not Threatened", "Threatened", "Not Assessed"),
  count=c(9227, 19294, 1540))

# Compute percentages
tp_category_counts_df$fraction = tp_category_counts_df$count / sum(tp_category_counts_df$count)

# Compute the cumulative percentages (top of each rectangle)
tp_category_counts_df$ymax = cumsum(tp_category_counts_df$fraction)

# Compute the bottom of each rectangle
tp_category_counts_df$ymin = c(0, head(tp_category_counts_df$ymax, n=-1))

# Compute label position
tp_category_counts_df$labelPosition <- (tp_category_counts_df$ymax + tp_category_counts_df$ymin) / 2

#rounding Fraction column 
tp_category_counts_df$fraction <- tp_category_counts_df$fraction * 100
tp_category_counts_df$fraction <- round(tp_category_counts_df$fraction, 1)

#Computing Labels
tp_category_counts_df$label <- paste0(tp_category_counts_df$fraction, "% ", tp_category_counts_df$category)

#Testing a donut chart
threat_donutplot <- ggplot(tp_category_counts_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Orchidaceae family by Threat Prediction") +
  theme(
    plot.title=element_text(family='', face='bold', colour='Black', size=16)
  )
            
#Adding New Legend Title 
threat_donutplot <- threat_donutplot + guides(fill=guide_legend(title="Threat Category"))
print(threat_donutplot)


#Now we want to understand what is conserved in the MSB
#Adding in Main Data Set with combined data 
main_database <- read.csv("Main_Database.csv")

conserved_msbp <- sum(main_database$seedbank_conserved == "Yes")
conserved_message <- "unique species of Orchid are conserved across the Millenium Seedbank Partnership"
conserved_statement <- paste(conserved_msbp, conserved_message)
print(conserved_statement)

#explanatory variable = Threat Prediction
#Response Vaiable = Conservation Status
threat_table <- table(main_database$threat_pred, main_database$seedbank_conserved)
print(threat_table)

#putting this into a dataframe
threat_table_df <- data.frame(NotConserved=c(1528,8636,18846),Conserved=c(12,591,448), row.names=c("No Threat Prediction","Not Threatened", "Threatened"))

mosaicplot(threat_table_df, 
           color = c("khaki1", "palegreen"), 
           xlab ="Threat Status", 
           ylab = "Conserved in Millenium Seedbank Partnership",
           main = "Threat Prediction and Conservation status of the Orchidaceae family",
           )
           
           
#Contingency Analysis 
#Null Hypothesis - Two Variables are independent of each other. There is no relationship
#between threat status and conservation status. 

#Checking assumptions, that the expected values are greater than 5. 
chisq.test(threat_table)$expected

chisq.test(threat_table, correct = FALSE)
#x squared = 343.95, df = 2, p-value <2.2e-16
#Reject the null hypothesis that there is no association between Threat Status and Conservation Status

prop.table(threat_table)

#Let's look closer at what is being conserved in the seedbank by genus
#first we subset the data
conserved_data <- dplyr::filter(main_database, seedbank_conserved == "Yes")
is.data.frame(conserved_data)

genus_table <- table(main_database$genera, main_database$seedbank_conserved)
genus_table_df <- as.data.frame.matrix(genus_table)

#Lets look at what isn't being conserved GAP ANALYSIS:
not_conserved <- dplyr::filter(main_database, seedbank_conserved == "No")
not_conserved_threatened <- dplyr::filter(not_conserved, threat_pred == "threatened")

lifeform_gap <- table(not_conserved_threatened$lifeform_category)
lifeform_gap <- as.data.frame(lifeform_gap)

ggplot(lifeform_gap, aes(x= Freq, y = Var1)) +
  geom_bar(stat = "identity", width = 0.9, col = "darkgreen", fill = "honeydew3") +
  xlab("Count") +
  ylab("LifeForm") +
  ggtitle("LifeForm of Threatened Non-Conserved Orchids") +
  theme_classic() 


conserved_threatened <- dplyr::filter(conserved_data, threat_pred == "threatened")
conserved_threatened_lifeform <- table(conserved_threatened$lifeform_category)
conserved_threatened_lifeform_df <- as.data.frame(conserved_threatened_lifeform)

ggplot(conserved_threatened_lifeform_df, aes(x= Freq, y = Var1)) +
  geom_bar(stat = "identity", width = 0.9, col = "darkgreen", fill = "honeydew3") +
  xlab("Count") +
  ylab("LifeForm") +
  ggtitle("LifeForm of Threatened Conserved Orchids") +
  theme_classic() 

#Lets look at what is being conserved in the genebank by Geography
geography_table <- table(main_database$region, main_database$seedbank_conserved)
geography_table_df <- as.data.frame.matrix(geography_table)


#Now looking at life form
lifeform_table <- table(main_database$lifeform_category)
lifeform_table_df <- as.data.frame(lifeform_table)
lifeform_table

#Whole Orchidaceae family
ggplot(lifeform_table_df, aes(x= Freq, y = Var1)) +
  geom_bar(stat = "identity", width = 0.9, col = "darkgreen", fill = "honeydew3") +
  xlab("Count") +
  ylab("LifeForm") +
  ggtitle("Variation in life form in the Orchidaceae family") +
  theme_classic() 

#Just conserved 
life_form_conserved <- table(conserved_data$lifeform_category)
life_form_conserved_df <- as.data.frame(life_form_conserved)

#Just conserved Orchids
ggplot(life_form_conserved_df, aes(x= Freq, y = Var1)) +
  geom_bar(stat = "identity", width = 0.9, col = "darkgreen", fill = "honeydew3") +
  xlab("Count") +
  ylab("LifeForm") +
  ggtitle("Variation in life form in Orchids conserved across the MSBP") +
  theme_classic() 

#What is conserved in the seedbank by threat status?

iucn_assessed <- dplyr::filter(main_database, iucn_assessed == "Yes")
iucn_table <- table(iucn_assessed$redlist_category, iucn_assessed$seedbank_conserved)
iucn_assessed_df <- as.data.frame(iucn_table)

#Importing IUCN Colours
names(iucn_palettes)

colour_vector <- c()


ggplot(iucn_assessed_df, aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  xlab("RedList Category") +
  ylab("Count") +
  theme_classic()


#Looking at boxplots - Angiosperm Threat Predictions

threat_pred <- table(main_database$threat_pred, main_database$seedbank_conserved)
threat_pred_df <- as.data.frame(threat_pred)

threat_pred2 <- table(main_database$threat_pred)
threat_pred2_df <- as.data.frame(threat_pred2)

ggplot(data = threat_pred2_df) +
  geom_boxplot(mapping = aes(x = Var1, y = Freq))

ggplot(data = threat_pred_df) +
  geom_boxplot(mapping = aes(x = Var2, y = Freq)) +
  xlab("Conserved in the Millenium Seedbank Partnership") +
  ylab("Frequency") 



#Looking at boxplots - IUCN red list:
ggplot(data = iucn_assessed_df) +
  geom_boxplot(mapping = aes(x = Var2, y = Freq)) +
  xlab("Conserved in Millenium Seedbank Partnership") +
  ylab("Frequency") 

ggplot(data = iucn_assessed_df) +
  geom_boxplot(mapping = aes(x = Var1, y = Freq)) +
  xlab("IUCN Category") +
  ylab("Frequency")

#Trying to visualise chi square test results
#Checking assumptions, that the expected values are greater than 5. 
chisq.test(threat_table)$expected

chisq.test(threat_table, correct = FALSE)
#x squared = 343.95, df = 2, p-value <2.2e-16
#Reject the null hypothesis that there is no association between Threat Status and Conservation Status
prop.table(threat_table)



install.packages("vcd")
library(vcd)

library(vcd)

mosaic(contingency_table)

contingency_table <- table(main_database$threat_pred, main_database$seedbank_conserved)
print(contingency_table)

dimnames(contingency_table) <- list(
  ThreatStatus = c("Not Assessed", "Not Threatened", "Threatened"),
  ConservationStatus = c("Not Conserved", "Conserved")
)

print(contingency_table)
mosaic(~ ThreatStatus + ConservationStatus,
       direction = c("v", "h"),
       data = contingency_table,
       shade = TRUE
)


#Contingency Table for IUCN 

# Step 1: Define the values as a vector
values <- c(877, 47, 917, 74)

# Step 2: Create the matrix
matrix_data <- matrix(values, nrow = 2, ncol = 2, byrow = TRUE)

# Step 3: Add row and column names
dimnames(matrix_data) <- list(
  ThreatStatus = c("Threatened", "Not Threatened"), # Row names
  ConservationStatus = c("Not Conserved", "Conserved")          # Column names
)

# Step 4: Convert the matrix to a table
table_data <- as.table(matrix_data)

# Display the table
print(table_data)


mosaic(~ ThreatStatus + ConservationStatus,
       direction = c("v", "h"),
       data = table_data,
       shade = TRUE)

iucn_chi <- chisq.test(table_data)
print(iucn_chi)

conserved_data <- dplyr::filter(main_database, seedbank_conserved == "Yes")

conserved_threatened <- dplyr::filter(conserved_data, threat_pred == "threatened")
conserved_notthreatened <- dplyr::filter(conserved_data, threat_pred =="not threatened")
conserved_notassessed <- dplyr::filter(conserved_data, threat_pred == "#N/A")

conserved_df <- data.frame(
  category=c("Not Threatened", "Threatened", "Not Assessed"),
  count=c(591, 448, 12))

# Compute percentages
conserved_df$fraction = conserved_df$count / sum(conserved_df$count)

# Compute the cumulative percentages (top of each rectangle)
conserved_df$ymax = cumsum(conserved_df$fraction)

# Compute the bottom of each rectangle
conserved_df$ymin = c(0, head(conserved_df$ymax, n=-1))

# Compute label position
conserved_df$labelPosition <- (conserved_df$ymax + conserved_df$ymin) / 2

#rounding Fraction column 
conserved_df$fraction <- conserved_df$fraction * 100
conserved_df$fraction <- round(conserved_df$fraction, 1)

#Computing Labels
conserved_df$label <- paste0(conserved_df$fraction, "% ", conserved_df$category)


threat_conserved_donutplot <- ggplot(conserved_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Orchidaceae family by Threat Prediction") +
  theme(
    plot.title=element_text(family='', face='bold', colour='Black', size=16)
  )

#Adding New Legend Title 
threat_conserved_donutplot <- threat_conserved_donutplot + guides(fill=guide_legend(title="Threat Category"))
print(threat_conserved_donutplot)



#IUCN Chi squared

iucn_chi <- table(iucn_assessed$redlist_category, iucn_assessed$seedbank_conserved)
print(iucn_chi)
iucn_chisq <- chisq.test(iucn_chi)
print(iucn_chisq)



#Ignore
library(vcd)

mosaic(contingency_table)

iucn_table <- table(iucn_assessed$redlist_category, iucn_assessed$seedbank_conserved)
print(iucn_table)

dimnames(iucn_table) <- list(
  ThreatStatus = c("Not Assessed", "Not Threatened", "Threatened"),
  ConservationStatus = c("Not Conserved", "Conserved")
)

print(contingency_table)
mosaic(~ ThreatStatus + ConservationStatus,
       direction = c("v", "h"),
       data = contingency_table,
       shade = TRUE
)


#Reading in updated dataset including binary data and bootstrappign
main_database2 <- read.csv("main_database_v2.csv")


#LifeForm vs Conservation Status
ggplot(main_database2, aes(x = lifeform_category, y = binary_conserved)) +
  geom_boxplot() +
  labs(title = "Boxplot of Binary Values by Lifeform",
       x = "Lifeform",
       y = "Conservation Status Binary") +
  theme_minimal()

#Jitter plot
jitter <- ggplot(main_database2, aes(lifeform_category, binary_conserved))

jitter + geom_jitter(aes(colour = climate_description))
