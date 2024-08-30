#H1 - Are Threatened Orchids Conserved?
#Installing Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(IUCNpalette)
library(gtsummary)
library(vcd)

#Importing Database
Main_Data <- read.csv("Main_Database.csv")

#Figure 1
threat_category_counts <- table(Main_Data$threat_pred)
print(threat_category_counts)

threat_category_df <- data.frame(
  category = c("No Prediction", "Not Threatened", "Threatened"),
  counts = c(1540, 9227, 19294)
)

threat_category_df$percentage <- threat_category_df$counts / sum(threat_category_df$counts) * 100

bar_colors <- c(rgb(153, 153, 153, maxColorValue = 255),  
                rgb(26, 152, 80, maxColorValue = 255),    
                rgb(215, 48, 39, maxColorValue = 255)) 


figure1 <- ggplot(threat_category_df, aes(x=category, y=counts, fill=category)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = bar_colors) +
  xlab("Threat Category") +
  ylab("Number of Species") +
  labs(fill = "Threat Category") +
  scale_y_continuous(breaks = seq(0, 20000, by = 2500)) +
  geom_text(aes(label=paste(round(percentage), "%")), vjust=-0.3, size=3.0, color="black") + 
  theme_light(base_size = 9) +
  theme(text = element_text(family = "Arial"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"),  
        axis.title.y = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"), 
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"),
        legend.text = element_text(size = 11, colour = "black"),
        legend.title = element_text(size = 12, colour = "black")
        )

print(figure1)
ggsave("figure1.png", width = 8, height = 6)

#Figure 2 
iucn_assessed <- dplyr::filter(Main_Data, iucn_assessed == "Yes")

iucn_categories <- table(iucn_assessed$redlist_category)
print(iucn_categories)

iucn_category_df <- data.frame(
  category = c("Data Deficient", "Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Extinct"),
  counts = c(254, 634, 103, 228, 447, 250, 6),
  color = c("#D1D1c6", "#60c659", "#CCE226", "#F9E814", "#FC7F3F", "#D81E05", "#000000")
)

# Reorder factor levels of category column
iucn_category_df$category <- factor(iucn_category_df$category, levels = c("Data Deficient", "Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Extinct"))

# Create the bar plot with reordered bars and specific colors
figure2 <- ggplot(iucn_category_df, aes(x=category, y=counts, fill=color)) +
  geom_bar(stat="identity") +
  xlab("IUCN Category") +
  ylab("Number of Species") +
  geom_text(aes(label=counts), vjust=-0.5, color="black", size=3.0) +
  scale_fill_identity() +  # Use the color names directly as fill colors
  theme_light(base_size = 9) +
  labs(fill = "Color") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, colour = "black"),
    text = element_text(family = "Arial"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"), 
    axis.text.y = element_text(size = 11, colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"))
            

print(figure2)
ggsave("figure2.png", width = 8, height = 6)

#Figure 3 - Chi-Squared Visualisation

contingency_table <- table(Main_Data$threat_pred, Main_Data$seedbank_conserved)
print(contingency_table)

dimnames(contingency_table) <- list(
  ExtinctionRisk = c("NA", "Not Threatened", "Threatened"),
  ConservationStatus = c("Not Conserved", "Conserved")
)

par(cex = 0.5) 

mosaic(~ ExtinctionRisk + ConservationStatus,
       direction = c("v", "h"),
       data = contingency_table,
       shade = TRUE
)

ggsave("figure3.png", width = 6, height = 4, dpi = 300)


#Figrue 4 - Horizontal Bar Plot

seedbank_conserved <- table(Main_Data$seedbank_conserved)
print(seedbank_conserved)

group <- factor(c("Banked", "Not Banked"), levels = c("Banked", "Not Banked"))
value <- c(1051, 29011)
bar_data <- data.frame(group, value)

bar_data <- transform(bar_data,
                         percent = value / sum(value) * 100)

# Create the horizontal stacked bar plot
p <- ggplot(bar_data, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Banked" = "#026C3D", "Not Banked" = "#99C68E")) +
  labs(y = NULL, x = NULL, fill = "Banked Status") +
  coord_flip() +
  theme_minimal() +  # Use theme_minimal() for a clean look
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank())  # Remove minor gridlines

print(p)
# Save the plot using ggsave
ggsave("horizontal_stacked_bar_plot.png", plot = p, width = 8, height = 4, units = "in")

#Figure 5 - Life Form

lifeform <- table(Main_Data$lifeform_category, Main_Data$seedbank_conserved)
lifeform_df <- as.data.frame(lifeform)

# Assuming lifeform_df is your data frame
lifeform_df <- lifeform_df %>%
  group_by(Var1) %>%
  mutate(Percentage = paste0(round(Freq / sum(Freq) * 100, 1), "%"))

p <- ggplot(lifeform_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(x = "Lifeform Category", y = "Species Count", fill = "Banked in Seedbank") +
  scale_fill_manual(values = c("No" = "#a6bddb", "Yes" = "#045a8d")) + 
  theme_light() +
  theme(
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"), 
    axis.text.y = element_text(size = 11, colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black", face = "bold"),
    axis.line = element_line(color = "black"))

# Display the plot
print(p)
ggsave("figure7.png", width = 8, height = 6)

#Figure 6 - IUCN Threat vs. Conservation Status Box Plot 

iucn_assessed <- dplyr::filter(Main_Data, iucn_assessed == 'Yes')

ggplot(iucn_assessed, aes(x = seedbank_conserved, y = Bootstrapped_Prob)) +
  geom_boxplot() +
  ylab("Extinction Risk Probability") +
  xlab("Seedbank Conserved")

t_test_result <- t.test(Bootstrapped_Prob ~ seedbank_conserved, data = iucn_assessed)
print(t_test_result)

#Figure 7 - Threat Prediction vs. Conservation Status Box Plot 
Main_Data$prediction_prob <- as.numeric(Main_Data$prediction_prob)

ggplot(Main_Data, aes(x = seedbank_conserved, y = prediction_prob)) +
  geom_boxplot() +
  xlab("Seebank Conserved") +
  ylab("Extinction Risk Probability")

t_test_result_2 <- t.test(prediction_prob ~ seedbank_conserved, data = Main_Data)
print(t_test_result_2)

#Creating combined plot
#Adding Data Source column
Main_Data$datasource <- "Extinction Risk Predictions"
iucn_assessed$datasource <- "IUCN"
#removing threat predictions from IUCN Dataset leaving only bootstrapped predictions
iucn_assessed <- subset(iucn_assessed, select = -prediction_prob)
#renaming bootstrapped to prediction_prob before combining 
iucn_assessed <- iucn_assessed %>% rename(prediction_prob = Bootstrapped_Prob)

#removing bootstrapped column from main data
main_data_copy <- subset(Main_Data, select = -Bootstrapped_Prob)

combined_data <- rbind(main_data_copy, iucn_assessed)

ggplot(combined_data, aes(x = seedbank_conserved, y = prediction_prob, fill = interaction(datasource, seedbank_conserved))) +
  geom_boxplot() +
  facet_wrap(~ datasource) +
  xlab("Seedbank Conserved") +
  ylab("Extinction Risk Probability") +
  scale_fill_manual(values = c("IUCN.No" = "#a6bddb", "IUCN.Yes" = "#045a8d", 
                               "Threat Prediction.No" = "#a6bddb", "Threat Prediction.Yes" = "#045a8d")) +
  theme_light() + 
  theme(legend.position = "none")


#Figure 8 & 9 - Region Tables

threatened_not_conserved <- dplyr::filter(Main_Data, threat_pred == 'threatened', seedbank_conserved == 'No')

regions_table <- table(threatened_not_conserved$region)
regions_table <- as.data.frame(regions_table)

print(names(regions_table))
names(regions_table) <- c("Region", "Species_Number")
print(regions_table)
library(gt)

#Descending (Top 10)
desc_regions_table <- regions_table %>%
  arrange(desc(Species_Number)) %>%
  slice(1:10)

desc_regions_table %>% gt() %>%
  tab_header(title = "Quantity of Threatened, Non-Conserved Orchid Species by Region",
             subtitle = "Top 10 Regions with Largest number of non-conserved threatened species") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  )  
  
#Ascending (Bottom 10)
asc_regions_table <- regions_table %>%
  arrange((Species_Number)) %>%
  slice(1:10)

asc_regions_table %>% gt() %>%
  tab_header(title = "Quantity of Threatened, Non-Conserved Orchid Species by Region",
             subtitle = "10 Regions with smallest number of non-conserved threatened species") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  )  

#Full Regions Table:
full_regions_table <- regions_table %>%
  arrange(desc(Species_Number))

full_regions_table %>% gt() %>%
  tab_header(title = "Quantity of Threatened, Non-Conserved Orchid Species by Region") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  ) %>%
gtsave("fulltable.docx")



#Figure 10 - Regions with the most conserved Orchid genera
conserved <- dplyr::filter(Main_Data, seedbank_conserved == 'Yes')
conserved_regions <- table(conserved$region)
conserved_regions <- as.data.frame(conserved_regions)
print(names(conserved_regions))
names(conserved_regions) <- c("Region", "Species_Number")

desc_genus <- conserved_regions %>%
  arrange(desc(Species_Number)) %>%
  slice(1:10)

desc_genus %>% gt() %>%
  tab_header(title = "Quantity of Conserved Orchid Species",
             subtitle = "Top 10 Regions with Largest number of Orchids Conserved") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  )  

#Full Data
full_region_table <- conserved_regions %>%
  arrange(desc(Species_Number)) 

full_region_table %>% gt() %>%
  tab_header(title = "Quantity of Conserved Orchid Species by Region") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  ) %>%
  gtsave("fulltable2.docx")
#Figure 10 - Top 10 Genera with threatened non conserved Orchids 
threatened_not_conserved <- dplyr::filter(Main_Data, threat_pred == 'threatened', seedbank_conserved == 'No')
genus <- table(threatened_not_conserved$genera)
genus <- as.data.frame(genus)

print(names(genus))
names(genus) <- c("Genera", "Species_Number")

desc_genus <- genus %>%
  arrange(desc(Species_Number)) %>%
  slice(1:10)

desc_genus %>% gt() %>%
  tab_header(title = "Quantity of Threatened, Non-Conserved Orchid Species by Genera",
             subtitle = "Top 10 Genera with Largest number of non-conserved threatened species") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  ) 

#Full Data
full_genus_table <- genus %>%
  arrange(desc(Species_Number)) 

full_genus_table %>% gt() %>%
  tab_header(title = "Quantity of Conserved Orchid Species by Genera") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  ) %>%
  gtsave("fulltable3.docx")


#Figure 11 - Most conserved Orchid Genera
conserved_genera <- dplyr::filter(Main_Data, seedbank_conserved == 'Yes')

genus_conserved <- table(conserved_genera$genera)
genus_conserved <- as.data.frame(genus_conserved)
names(genus_conserved) <- c("Genera", "Species_Number")

desc_genus_conserved <- genus_conserved %>%
  arrange(desc(Species_Number)) %>%
  slice(1:10)

desc_genus_conserved %>% gt() %>%
  tab_header(title = "Quantity of Conserved Orchid Species by Genera",
             subtitle = "Top 10 Genera with Largest number of conserved species") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  )  

#Figure 12 - Most Conserved Regions
region_conserved <- table(conserved_genera$region)
region_conserved <- as.data.frame(region_conserved)
names(region_conserved) <- c("Region", "Species_Number")

desc_region_conserved <- region_conserved %>%
  arrange(desc(Species_Number)) %>%
  slice(1:10)

desc_region_conserved %>% gt() %>%
  tab_header(title = "Quantity of Conserved Orchid Species by Region",
             subtitle = "Top 10 Regions with Largest number of conserved species") %>%
  tab_source_note(
    source_note = "Data Source: World Checklist of Vascular Plants and Threat Predictions Dataset (Bachman et al,2024) "
  )  

#Figure 13 - LifeForm
#Now looking at life form
lifeform_table <- table(Main_Data$lifeform_category)
lifeform_table_df <- as.data.frame(lifeform_table)
lifeform_table

#Whole Orchidaceae family
ggplot(lifeform_table_df, aes(x= Freq, y = Var1)) +
  geom_bar(stat = "identity", width = 0.9, col = "darkgreen", fill = "honeydew3") +
  xlab("Count") +
  ylab("LifeForm") +
  ggtitle("Variation in life form in the Orchidaceae family") +
  theme_light() 

#Figure ? - regression Plot - number of native Orchids vs Proportion conserved


occurence_data <- read.csv("occurence_matrix.csv")


range_plot <- ggplot(occurence_data, aes(x = bin_code, y = proportion_conserved, fill = bin_code)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "red") +
  labs(x = "Number of Native Species Per Country (Categories)",
       y = "Proportion of Native Orchids Banked per Country") +
  scale_fill_manual(
    values = c("a" = "pink", "b" = "yellow", "c" = "darkolivegreen2", "d" = "seagreen", 
               "e" = "cyan4", "f" = "blue", "g" = "purple", "na" = "deeppink"),
    labels = c("a" = " a = <20 Species", "b" = "b = <60 Species", "c" = "c = <200 species",
               "d" = "d = <800 Species", "e" = "e = <2000 Species", "f" = "f = <4000 Species", 
               "g" = "g = <6000 Species", "na" = "na = No Species"),
    name = "Category Legend"
  ) +
  theme_light()

print(range_plot)
ggsave("range_plot.png", width = 8, height = 6)


#Comparative Data Figure

orchids_assessed <- data.frame(
  `Data_Source` = c("IUCN", "Extinction Risk Predictions"),
  `No_Orchids_Assessed` = c("6.3", "97.8")
)

orchids_assessed <- as.data.frame(orchids_assessed)

orchids_assessed$No_Orchids_Assessed <- as.numeric(orchids_assessed$No_Orchids_Assessed)

orchids_assessed_graph <- ggplot(orchids_assessed, aes(x = Data_Source, y = No_Orchids_Assessed, fill = Data_Source)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(x = "Data Source", y = "% of Orchidaceae Family Assessed") + 
  scale_fill_manual(values = c("#d6604d", "#fee090"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"), 
    axis.text.y = element_text(size = 11, colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black", face = "bold"),
    axis.line = element_line(color = "black"),
    legend.position = "none")

print(orchids_assessed_graph)

#
percent_threatened <- data.frame(
  `Data_Source` = c("IUCN", "Extinction Risk Predictions"),
  `No_Orchids_Threatened` = c("48.4", "64")
)

percent_threatened <- as.data.frame(percent_threatened)

percent_threatened$No_Orchids_Threatened <- as.numeric(percent_threatened$No_Orchids_Threatened)

percent_threatened_graph <- ggplot(percent_threatened, aes(x = Data_Source, y = No_Orchids_Threatened, fill = Data_Source)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(x = "Data Source", y = "% of Assessed Orchids Threatened") + 
  scale_fill_manual(values = c("#d6604d", "#fee090"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, margin = margin(t = 10), colour = "black", face = "bold"), 
    axis.text.y = element_text(size = 11, colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black", face = "bold"),
    axis.line = element_line(color = "black"),
    legend.position = "none")

print(percent_threatened_graph)

#Stacked Now 
