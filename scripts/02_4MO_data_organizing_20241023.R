#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#       Initial preparation of the data from 4MO
#
#----------------------------------------------------------#
# Does not generate any Figure, just prepares data 

# This script consist of two steps, first prepares the dataset per branches from leaves
# From Line ca. 90, twigs are being restructures after the summary is loaded (2 steps are done in Excel in between)

#----------------------------------------------------------#
# 0. libraries and graphical properties definition  -----
#----------------------------------------------------------#

# libraries
library(tidyverse)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(MuMIn)
library(emmeans)
library(performance)
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(see)
library(qqplotr)
library(readr)
library(bbmle)

# graphical properties definition
theme_set(theme_classic())
text_size <-  22

PDF_width <-  10
PDF_height <-  6

#----------------------------------------------------------#
# 1. Import data 4M0 and prepare them as sums per twigs -----
#----------------------------------------------------------#
# 1.1 Read 4MO herbivory data for individual leaves  -----
dataset_herbivory <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivoryLeavesRaw_4MO_6sites_20241023.csv")
summary(dataset_herbivory)

dataset_herbivory$Site <-as.factor(dataset_herbivory$Site)
dataset_herbivory$Species <-as.factor(dataset_herbivory$Species)
dataset_herbivory$Strata <-as.factor(dataset_herbivory$Strata)
dataset_herbivory$Individual <-as.factor(dataset_herbivory$Individual)
dataset_herbivory$Treatment <-as.factor(dataset_herbivory$Treatment)
dataset_herbivory$Experiment <-as.factor(dataset_herbivory$Experiment)
dataset_herbivory$Patrol <-as.factor(dataset_herbivory$Patrol)
dataset_herbivory$Twig <-as.factor(dataset_herbivory$Twig)
dataset_herbivory$Area.x <- as.numeric(dataset_herbivory$Area.x)
dataset_herbivory$Area.y <- as.numeric(dataset_herbivory$Area.y)
dataset_herbivory$Herbivory <- as.numeric(dataset_herbivory$Herbivory)
dataset_herbivory$HerbProp <- as.numeric(dataset_herbivory$HerbProp)
dataset_herbivory$SpeciesAnal <-as.factor(dataset_herbivory$SpeciesAnal)
dataset_herbivory$BranchPatrol <-as.factor(dataset_herbivory$BranchPatrol)
dataset_herbivory$TwigCode <-as.factor(dataset_herbivory$TwigCode)
summary(dataset_herbivory)

# reorganize sites
custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_herbivory$Site <- factor(dataset_herbivory$Site, levels = custom_order)

dataset_herbivory <- na.omit(dataset_herbivory)
class(dataset_herbivory$HerbProp)
class(dataset_herbivory$Area.x)
class(dataset_herbivory$Herbivory)
summary(dataset_herbivory)

# 1.2 herbivory data summing per twigs and patrols -----
dataset_herbivory_sum <-
  dataset_herbivory %>% 
  group_by(Site, Patrol, Experiment, SpeciesAnal, Species, Twig, TwigCode) %>% 
  summarize(
    .groups = "keep",
    TwigArea_sum = sum(Area.x),
    TwigHerbivory_sum = sum(Herbivory)
  )
summary(dataset_herbivory_sum)

names(dataset_herbivory_sum)[names(dataset_herbivory_sum) == "TwigCode"] <- "TwigCodeUnique"

dataset_herbivory_sum$HerbProp <-dataset_herbivory_sum$TwigHerbivory_sum/dataset_herbivory_sum$TwigArea_sum

# Save the summed dataset
write_csv(
  dataset_herbivory_sum,
  "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivoryTwigSum_4MO_6sites_20241023a.csv")

#In the next step, I opened the saved sheet, and manually added Treatment and TwigCodeNo Patrol by functions in Excel
#Treatment	TwigCodeNoPatrol
#1CN1	LAK_Acer_platanoides_1CN1_Blue
#2CN1	LAK_Acer_platanoides_2CN1_Blue
#3CN1	LAK_Acer_platanoides_3CN1_Blue
#4CN1	LAK_Acer_platanoides_4CN1_Blue
#SAVE AS = HerbivoryTwigSum_1MO_6sites_20241010b.csv

#----------------------------------------------------------#
# 2. Import dataset summarized per twigs -----
#----------------------------------------------------------#

dataset_herbivory_sum <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data//HerbivoryTwigSum_4MO_6sites_20241023b.csv")
summary(dataset_herbivory_sum)

dataset_herbivory_sum$Site <-as.factor(dataset_herbivory_sum$Site)
dataset_herbivory_sum$Patrol <-as.factor(dataset_herbivory_sum$Patrol)
dataset_herbivory_sum$Experiment <-as.factor(dataset_herbivory_sum$Experiment)
dataset_herbivory_sum$SpeciesAnal <-as.factor(dataset_herbivory_sum$SpeciesAnal)
dataset_herbivory_sum$Species <-as.factor(dataset_herbivory_sum$Species)
dataset_herbivory_sum$TwigCodeUnique <-as.factor(dataset_herbivory_sum$TwigCodeUnique)
dataset_herbivory_sum$TwigArea_sum <- as.numeric(dataset_herbivory_sum$TwigArea_sum)
dataset_herbivory_sum$TwigHerbivory_sum <- as.numeric(dataset_herbivory_sum$TwigHerbivory_sum)
dataset_herbivory_sum$HerbProp <- as.numeric(dataset_herbivory_sum$HerbProp)
dataset_herbivory_sum$Treatment <-as.factor(dataset_herbivory_sum$Treatment)
dataset_herbivory_sum$TwigCodeNoPatrol <-as.factor(dataset_herbivory_sum$TwigCodeNoPatrol)
summary(dataset_herbivory_sum)

# 1.3 turning the long format to wide one so the differences can be made  -----
# Verify unique values in the 'Patrol' column
unique(dataset_herbivory_sum$Patrol)

# Filter out rows with missing 'Patrol' values
dataset_herbivory_clean <- dataset_herbivory_sum %>%
  filter(!is.na(Patrol))

names(dataset_herbivory_clean)[names(dataset_herbivory_clean) == "HerbProp"] <- "TwigHerbProp"
summary(dataset_herbivory_clean)

dataset_herbivory_clean %>%
  group_by(SpeciesAnal, Site, TwigCodeNoPatrol, Patrol) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

# To keep the numerical Species SP is important here, as SP4 and Sp5 in EUC were the same botanical species due to their abundance
# Transform the data from long to wide format
wide_dataset_herbivory <- dataset_herbivory_clean %>%
  select(Species, SpeciesAnal, Site, TwigCodeNoPatrol, Patrol, TwigArea_sum, TwigHerbProp, TwigHerbivory_sum) %>%
  pivot_wider(names_from = Patrol, values_from = c(TwigArea_sum, TwigHerbProp, TwigHerbivory_sum),
              names_glue = "{.value}_{Patrol}")

# Display the first few rows of the wide format dataset
head(wide_dataset_herbivory)

# Save the dataset
write_csv(
  wide_dataset_herbivory,
  "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_4MO_6sites_20241023_wide.csv")


