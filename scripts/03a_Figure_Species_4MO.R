#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#          Figure for individual species 4MO
#
#----------------------------------------------------------#
# Generates Figure S4 in the Supplement

library(ggplot2)

library(tidyr)
library(dplyr)

#----------------------------------------------------------#
# 1. Import data -----
#----------------------------------------------------------#
# 1.1 herbivory data -----
# this next step is based on the original wide dataset (wide_dataset_herbivory -> HerbivorySum_1MO_6sites_20241010_wide), however, few steps were done manually
# for each twig we had herbivory damage in proportion, for 4 patrols (A, C = starts of the experiments, B, D the respective ends of the experiments)
# each sapling individual thus has still 3 values (3 twigs)
# StartEndDiff = Proportion of the herbivory damage in Time 1 - Proportion of the herbivory damage in Time 0, NA is in all A and C patrols, as they are Time 0

dataset_herbivory_4MO <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv")
summary(dataset_herbivory_4MO)

custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_herbivory_4MO$Site <- factor(dataset_herbivory_4MO$Site, levels = custom_order)
dataset_herbivory_4MO$Site <-as.factor(dataset_herbivory_4MO$Site)           #
dataset_herbivory_4MO$Patrol <-as.factor(dataset_herbivory_4MO$Patrol)
dataset_herbivory_4MO$SpeciesAnal <-as.factor(dataset_herbivory_4MO$SpeciesAnal) 
dataset_herbivory_4MO$TwigCodeUnique <-as.factor(dataset_herbivory_4MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_4MO$SaplingCodeUnique <-as.factor(dataset_herbivory_4MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
summary(dataset_herbivory_4MO)

# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6

data <- dataset_herbivory_4MO %>%
  mutate(PatrolGroup = case_when(
    Patrol %in% c("A", "C") ~ "AC",
    Patrol %in% c("B", "D") ~ "BD",
    TRUE ~ as.character(Patrol)
  ))

wide_data <- data %>%
  pivot_wider(
    names_from = PatrolGroup,
    values_from = c(TwigArea_sum, TwigHerbivory_sum, TwigHerbProp, StartEndDiff),
    names_glue = "{.value}_{PatrolGroup}"
  )

head(wide_data)

wide_data$PatrolGroup <- ifelse(wide_data$Patrol %in% c("A", "C"), "AC", "DB")
head(wide_data)

# Replace 0 with 0.00001 and NA with 0 in both columns first
wide_data <- wide_data %>%
  mutate(
    TwigArea_sum_AC = ifelse(TwigArea_sum_AC == 0, 0.00001, TwigArea_sum_AC),
    TwigArea_sum_BD = ifelse(TwigArea_sum_BD == 0, 0.00001, TwigArea_sum_BD)
  )

# Replace NA with 0 in both columns
wide_data <- wide_data %>%
  mutate(
    TwigHerbProp_AC = ifelse(is.na(TwigHerbProp_AC), 0, TwigHerbProp_AC),
    TwigHerbProp_BD = ifelse(is.na(TwigHerbProp_BD), 0, TwigHerbProp_BD)
  )

selected_columns <- wide_data %>%
  select(SpeciesAnal, TwigHerbProp_AC, TwigHerbProp_BD)  # Add more columns if n
head(selected_columns)

# Replace NA with 0 in both columns first
wide_data <- wide_data %>%
  mutate(
    TwigHerbProp_AC = ifelse(is.na(TwigHerbProp_AC), 0, TwigHerbProp_AC),
    TwigHerbProp_BD = ifelse(is.na(TwigHerbProp_BD), 0, TwigHerbProp_BD)
  )

# Now combine the columns by summing them, but write NA if both are 0
wide_data <- wide_data %>%
  mutate(
    TwigHerbProp_combined = ifelse(TwigHerbProp_AC == 0 & TwigHerbProp_BD == 0, NA, TwigHerbProp_AC + TwigHerbProp_BD)
  )

# Check the results
head(wide_data %>% select(TwigHerbProp_AC, TwigHerbProp_BD, TwigHerbProp_combined))

write.csv(wide_data, "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/For_figure4MO_wide_data.csv", row.names = FALSE)


# you can also start directly here, by loading the wide data, or skip the next line if you continue from earlier lines
wide_data <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/For_figure4MO_wide_data.csv")

wide_data$PatrolGroup <-as.factor(wide_data$PatrolGroup)  
wide_data$Site <-as.factor(wide_data$Site)  
wide_data$SpeciesAnal <-as.factor(wide_data$SpeciesAnal) 
wide_data
summary(wide_data)

# Check rows with NA in SpeciesAnal
wide_data[is.na(wide_data$SpeciesAnal), ]

unique_species <- unique(na.omit(wide_data$SpeciesAnal))
unique_species

# Calculate mean values for Patrol BD group by species
mean_values <- wide_data %>%
  filter(PatrolGroup == "DB") %>%
  group_by(SpeciesAnal) %>%
  summarise(mean_TwigHerbProp = mean(TwigHerbProp_combined * 100, na.rm = TRUE))

# Display the mean values
print(mean_values)

# Custom order of species
custom_species_order <- c(
  "Celtis_latifolia",
  "Eucalyptus_tereticornis",
  "Betula_maximowicziana",
  "Prunus_ssiori",
  "Baccaurea_ramiflora",
  "Dysoxylum_arborescens",
  "Erythrospermum_candidum",
  "Haplostichanthus_ramiflorus",
  "Endiandra_leptodendron",
  "Magnolia_kobus",
  "Pimelodendron_amboinicum",
  "Pometia_pinnata",
  "Cleistanthus_myrianthus",
  "Cryptocarya_sp.",
  "Diospyros_kaki",
  "Shorea_wangtianshuea",
  "Syringa_reticulata",
  "Orophea_laui",
  "Dysoxylum_sp.",
  "Garcinia_cowa",
  "Gymnacranthera_paniculata",
  "Rockinghamia_angustifolia",
  "Fraxinus_lanuginosa",
  "Carpinus_cordata",
  "Quercus_robur",
  "Fraxinus_excelsior",
  "Carpinus_betulus",
  "Acer_platanoides",
  "Ulmus_glabra",
  "Tilia_cordata",
  "Argyrodendron_peralatum",
  "Saprosma_ternata",
  "Acer_pseudoplatanus",
  "Myristica_globosa",
  "Pittosporopsis_kerrii",
  "Cleidion_brevipetiolatum",
  "Acer_palmatum",
  "Breynia_oblingifolia",
  "Acacia_parramattensis",
  "Chrysophyllum_roxburghii",
  "Eucalyptus_pruinosa",
  "Ficus_erythrosperma",
  "Bursaria_spinosa",
  "Acer_mono")

# Create a factor for SpeciesAnal based on custom order
wide_data <- wide_data %>%
  mutate(SpeciesAnal = factor(SpeciesAnal, levels = custom_species_order))
summary(wide_data)

# Check unique values in SpeciesAnal
unique_species <- unique(wide_data$SpeciesAnal)

# Find species that are not in the custom order
missing_species <- unique_species[!(unique_species %in% custom_species_order)]

# Display missing species
missing_species
# It is all ok, if: factor()

site_symbols <- c(LAK = 17, TOM = 16, BUB = 15, KAK = 17, DRO = 16, EUC = 15)
site_colors  <- c(LAK =  "blue", TOM = "turquoise" , BUB =  "yellowgreen" , KAK = "orange", DRO = "red" , EUC =  "purple")

# Create the ggplot
p1 <- ggplot(
  wide_data,  # Your reshaped wide-format data
  aes(
    x = SpeciesAnal,  # Combined species and patrol group
    y = TwigHerbProp_combined * 100,  # Multiply by 100 to get percentages
    col = PatrolGroup  # Use PatrolGroup to differentiate points
  )) + 
  
  ylim(0, 40) +
  
  # Adding jitter for the observed data points
  geom_point(
    data = wide_data,
    aes(y = TwigHerbProp_combined*100),
    alpha = 0.6,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.1)) +
  
  # Manually set colors for factors with transparency
  scale_color_manual(values = c(alpha("grey", 0.5), alpha("black", 0.5))) +  # Set color for PatrolGroup with transparency
  
  # Adding the box plot
  geom_boxplot(
    aes(fill = NA),  # Fill by PatrolGroup for better separation
    outlier.shape = NA,  # Remove outliers
    alpha = 0.1,  # Set fill alpha
    width = 0.5) +  # Adjust box width
  
  # Adding points for the mean of DB patrol group only
  stat_summary(
    data = subset(wide_data, PatrolGroup == "DB"),  # Filter for BD patrol group only
    fun = "mean",  # Specify that we want the mean
    geom = "point",  # Plot the mean as a point
    shape = 20,  # Circle shape
    size = 2,  # Adjust size of the point
    fill = "#cd5e31",  # Color of the point
    color = "#cd5e31",  # Border color for the point
    stroke = 2) +  # Border thickness for the point
  
  labs(
    x = "Species",  # X-axis label
    y = expression(atop("Herbivory per twig (%)"))) +  # Y-axis label
  
  theme(
    text = element_text(size = 18),
    legend.position = "none") +  # No legend
  
  theme(axis.title = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin = margin(r = 20)),  # Margin for y-axis label
        plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margin
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) + # Rotate x-axis labels by 90 degrees
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))  # Center y-axis label
p1

# Adding symbols corresponding to the site before the species names
p1 <- p1 + 
  geom_point(
    aes(x = SpeciesAnal, y = 40),  # Adjust the y position slightly above or at the top
    shape = site_symbols[wide_data$Site],  # Symbols corresponding to site
    color = site_colors[wide_data$Site],  # Colors corresponding to site
    size = 3, 
    position = position_nudge(x = -0.1)  # Nudge the symbols slightly to the left of species labels
  )
p1

# Calculate mean, SD, and percentiles for Patrol BD group by species
summary_stats <- wide_data %>%
  filter(PatrolGroup == "DB") %>%
  group_by(SpeciesAnal) %>%
  summarise(
    mean_TwigHerbProp = mean(TwigHerbProp_combined * 100, na.rm = TRUE),
    sd_TwigHerbProp = sd(TwigHerbProp_combined * 100, na.rm = TRUE),
    p25_TwigHerbProp = quantile(TwigHerbProp_combined * 100, probs = 0.25, na.rm = TRUE),
    median_TwigHerbProp = median(TwigHerbProp_combined * 100, na.rm = TRUE),
    p75_TwigHerbProp = quantile(TwigHerbProp_combined * 100, probs = 0.75, na.rm = TRUE)
  )
summary_stats

# Save the table
write_csv(summary_stats, "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/summary_stats_4MO_species.csv")

