#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#    Comparison of starting points for 1MO and 4MO experiments
#
#----------------------------------------------------------#
# Generates a comparions of A and C values for individual species but the Figure is not used in the manuscript

library(glmmTMB)
library(ggplot2)
library(AICcmodavg)
library(emmeans)
library(dplyr)
library(tidyverse)
library(tidyr)

# 1.1 Read herbivory data for twigs  -----
herbivory_data <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/1MO_4MO_AConly_forPairTest.csv")
summary(herbivory_data)

herbivory_data$Site <-as.factor(herbivory_data$Site)
herbivory_data$Species <-as.factor(herbivory_data$Species)
herbivory_data$Experiment <-as.factor(herbivory_data$Experiment)
herbivory_data$Patrol <-as.factor(herbivory_data$Patrol)
herbivory_data$TwigCodeNoPatrol <-as.factor(herbivory_data$TwigCodeNoPatrol)
herbivory_data$BranchCodeNoPatrol <-as.factor(herbivory_data$BranchCodeNoPatrol)
herbivory_data$TwigArea_sum <- as.numeric(herbivory_data$TwigArea_sum)
herbivory_data$TwigHerbivory_sum <- as.numeric(herbivory_data$TwigHerbivory_sum)
herbivory_data$TwigHerbProp <- as.numeric(herbivory_data$TwigHerbProp)
herbivory_data$SpeciesAnal <-as.factor(herbivory_data$SpeciesAnal)
herbivory_data$TwigCodeUnique  <-as.factor(herbivory_data$TwigCodeUnique )
herbivory_data$SaplingCodeUnique <-as.factor(herbivory_data$SaplingCodeUnique)
summary(herbivory_data)

# Define dropped species
dropped_species <- c("Celtis_latifolia", "Chrysophyllum_roxburghii", 
                     "Dysoxylum_arborescens", "Ficus_erythrosperma", 
                     "Gymnacranthera_paniculata", "Pimelodendron_amboinicum", 
                     "Erythrospermum_candidum")

# Filter out the dropped species
herbivory_data_filtered <- herbivory_data %>%
  filter(!SpeciesAnal %in% dropped_species)

# Fit models
glm.ACherb.sp_exp <- glmmTMB(TwigHerbProp + 0.0001 ~ SpeciesAnal * Experiment + 
                               (1|SaplingCodeUnique) + (1|Site:Patrol), 
                             family = beta_family(),
                             data = herbivory_data_filtered,
                             na.action = "na.exclude")

glm.ACherb_exp <- glmmTMB(TwigHerbProp + 0.0001 ~ Experiment + 
                            (1|SaplingCodeUnique) + (1|Site:Patrol), 
                          family = beta_family(),
                          data = herbivory_data_filtered,
                          na.action = "na.exclude")

# Compare models
AICctab(glm.ACherb.sp_exp, glm.ACherb_exp)

emmeans_results1 <- emmeans(glm.ACherb_exp, pairwise ~ Experiment, type = "response")
emmeans_results1


# Get EMMs for species with interaction
emmeans_results <- emmeans(glm.ACherb.sp_exp, pairwise ~ Experiment | SpeciesAnal, type = "response")
emmeans_results

# Convert EMMs results to data frame
emmeans_df <- as.data.frame(emmeans_results$emmeans)

# Transform the EMMs from logit scale to proportion scale
str(emmeans_df)
emmeans_df$proportion <- emmeans_df$response
emmeans_df

# The transformation is correct as follows:
#emmeans_df$proportion <- exp(emmeans_df$emmean) / (1 + exp(emmeans_df$emmean))
#emmeans_df

# Plot using the proportion values
ggplot(emmeans_df, aes(x = SpeciesAnal, y = proportion * 100, color = Experiment)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = pmax(proportion * 100 - SE * 100, 0), 
                    ymax = pmin(proportion * 100 + SE * 100, 100)), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Species", y = "Estimated Marginal Mean of Herbivory (%)", 
       title = "Herbivory Comparison Between Species Across Experiments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









