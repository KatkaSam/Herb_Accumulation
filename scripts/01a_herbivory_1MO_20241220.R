#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#                   Analyses from 1MO
#
#----------------------------------------------------------#
# Generates Figure 2 in the main text

library(glmmTMB)
library(ggplot2)
library(bbmle)
library(emmeans)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(tidyr)

#----------------------------------------------------------#
# 1. Import data -----
#----------------------------------------------------------#
# 1.1 herbivory data -----
# this next step is based on the original wide dataset (wide_dataset_herbivory -> HerbivorySum_1MO_6sites_20241010_wide), however, few steps were done manually
# for each twig we had herbivory damage in proportion, for 4 patrols (A, C = starts of the experiments, B, D the respective ends of the experiments)
# each sapling individual thus has still 3 values (3 twigs)
# StartEndDiff = Proportion of the herbivory damage in Time 1 - Proportion of the herbivory damage in Time 0, NA is in all A and C patrols, as they are Time 0

dataset_herbivory_1MO <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_1MO_6sites_20241023_wideTolong_v2.csv")
summary(dataset_herbivory_1MO)

custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_herbivory_1MO$Site <- factor(dataset_herbivory_1MO$Site, levels = custom_order)
dataset_herbivory_1MO$Site <-as.factor(dataset_herbivory_1MO$Site)           #
dataset_herbivory_1MO$Patrol <-as.factor(dataset_herbivory_1MO$Patrol)
dataset_herbivory_1MO$SpeciesAnal <-as.factor(dataset_herbivory_1MO$SpeciesAnal) 
dataset_herbivory_1MO$TwigCodeUnique <-as.factor(dataset_herbivory_1MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_1MO$SaplingCodeUnique <-as.factor(dataset_herbivory_1MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
summary(dataset_herbivory_1MO)

# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6

#----------------------------------------------------------#
# 2. Build models for B and D standing herbivory -----
#----------------------------------------------------------#

#now remove the A and C patrols, as the beginnings of the experiment
# B and D now actually represent 2 separate years in which the experiment was conducted
BD_herbivory_1MO <- dataset_herbivory_1MO[ which(dataset_herbivory_1MO$Patrol=='B' | dataset_herbivory_1MO$Patrol=='D'), ]
summary(BD_herbivory_1MO)

BD_herbivory_1MO_clean <- BD_herbivory_1MO[!is.na(BD_herbivory_1MO$TwigHerbProp), ]
summary(BD_herbivory_1MO_clean)


glm.herb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                           + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")
glm.herb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                          data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")

# compare the models
library(bbmle)
AICctab(glm.herb.null, glm.herb.site)

# I am enforcing the SITE model, because this is what I want to see 
# below are correct figures for null models
glm_herbivory_emmeans <-
  emmeans(
    glm.herb.site,
    pairwise ~ Site,
    type = "response")
glm_herbivory_emmeans

# Convert the contrasts to a data frame
contrast_summary <- as.data.frame(glm_herbivory_emmeans$contrasts)

# Apply the Bonferroni correction
adjusted_pvalues <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# Summarize the adjusted p-values
summary(adjusted_pvalues)

# Add adjusted p-values to the contrast summary
contrast_summary$bonferroni_p <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# View the updated contrast table
print(contrast_summary)

p1 <- (model_plot_01a <- 
         glm_herbivory_emmeans$emmeans %>% 
         as_tibble() %>% 
         ggplot(
           aes(
             x = Site,
             y = response * 100)) + 
         
         ylim(0, 40) +
         
         # Adding jitter for the observed data points
         geom_point(
           data = BD_herbivory_1MO_clean,
           aes(y = TwigHerbProp * 100),  # No fill or color applied here
           alpha = 0.2,
           position = position_jitter(width = 0.2),
           colour = "#52854C") +  # Jitter width to spread the points around the site axis
         
         geom_errorbar(
           aes(
             ymin = asymp.LCL * 100,
             ymax = asymp.UCL * 100),
           width = 0.3,
           size = 2) +
         
         geom_point(
           shape = 16,
           size = 4) +
         
         labs(
           x = "Site",  
           y = expression(atop("Standing herbivory damage", 
                          "one month into the growing season (%)"))) +
         
         theme(
           text = element_text(size = 18),
           legend.position = "top")) +
  
  scale_x_discrete(limits = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
    plot.margin = margin(20, 20, 20, 20)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
p1

#this is the correct Figure for null model
# Box-plot above raw data for null models, these graphs are used later on
p1_ns <- ggplot(
  BD_herbivory_1MO_clean,  # Use your raw data
  aes(
    x = Site,
    y = TwigHerbProp * 100)) + 
  
  ylim(0, 40) +
  
  # Adding jitter for the observed data points
  geom_point(
    alpha = 0.2,
    position = position_jitter(width = 0.2),
    colour = "#52854C") +  # Jitter width to spread the points around the site axis
  
  # Adding the box plot with means and percentiles
  geom_boxplot(
    aes(fill = NA),  # Optional fill by site for better visual separation
    outlier.shape = NA,  # Remove outliers (optional)
    alpha = 0, 
    width = 0.5) +  # Adjust box width
  
  # Adding mean points on top of the box plot
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 20, 
    size = 4, 
    color = "black") +  # Black dots representing the mean
  
  labs(
    x = "Site",  
    y = expression(atop("Standing herbivory damage", 
                        "one month into the growing season (%)"))) +
  
  theme(
    text = element_text(size = 18),
    legend.position = "none") +  # Hide legend if not needed
  
  scale_x_discrete(limits = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
        plot.margin = margin(20, 20, 20, 20)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
p1_ns

# Calculate mean and standard deviation for each site
summary_stats <- BD_herbivory_1MO_clean %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TwigHerbProp, na.rm = TRUE),  # Calculate mean
    SD = sd(TwigHerbProp, na.rm = TRUE)        # Calculate standard deviation
  )

# View the summary statistics
print(summary_stats)

#----------------------------------------------------------#
# 2. Build models for B-A and D-C - -----
#----------------------------------------------------------#
# i.e. change in herbivory as it accumulated over a monthy 
# since the beginning of the season

BD_herbivory_1MO$StartEndDiff <- ifelse(BD_herbivory_1MO$StartEndDiff < 0, 0, BD_herbivory_1MO$StartEndDiff)
#BD_herbivory_1MO$StartEndDiff <- abs(BD_herbivory_1MO$StartEndDiff)
summary(BD_herbivory_1MO)

BD_herbivory_1MO_clean2 <- BD_herbivory_1MO[!is.na(BD_herbivory_1MO$StartEndDiff), ]
summary(BD_herbivory_1MO_clean2)

glm.CHherb.null <-glmmTMB(StartEndDiff + 0.0001 ~ 1 +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean2,
                        na.action = "na.exclude")
glm.CHherb.site <-glmmTMB(StartEndDiff +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean2,
                        na.action = "na.exclude")

# compare the models
AICctab(glm.CHherb.null, glm.CHherb.site)

# I am enforcing the SITE model, because this is what I want to see 
# below are correct figures for null models
glm_CH.herbivory_emmeans <-
  emmeans(
    glm.CHherb.site,
    pairwise ~ Site,
    type = "response")
glm_CH.herbivory_emmeans


# Convert the contrasts to a data frame
contrast_summary <- as.data.frame(glm_CH.herbivory_emmeans$contrasts)

# Apply the Bonferroni correction
adjusted_pvalues <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# Summarize the adjusted p-values
summary(adjusted_pvalues)

# Add adjusted p-values to the contrast summary
contrast_summary$bonferroni_p <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# View the updated contrast table
print(contrast_summary)


p2 <- (model_plot_02 <- 
         glm_CH.herbivory_emmeans$emmeans %>% 
         as_tibble() %>% 
         ggplot(
           aes(
             x = Site,
             y = response * 100)) + 
         
         ylim(0, 40) +
         
         # Adding jitter for the observed data points
         geom_point(
           data = BD_herbivory_1MO_clean2,
           aes(y = StartEndDiff * 100),  # No fill or color applied here
           alpha = 0.2,
           position = position_jitter(width = 0.2),
           colour = "#52854C") +  # Jitter width to spread the points around the site axis
         
         geom_errorbar(
           aes(
             ymin = asymp.LCL * 100,
             ymax = asymp.UCL * 100),
           width = 0.3,
           size = 2,
           linetype = "dashed",
           colour = "orange4") +
         
         geom_point(
           shape = 16,
           size = 4,
           colour = "orange4") +
         
         labs(
           x = "Site",  
           y = expression(atop("Change of herbivory damage", 
                                 "over one month (%)"))) +
         
         theme(
           text = element_text(size = 18),
           legend.position = "top")) +
  
  scale_x_discrete(limits = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
    plot.margin = margin(20, 20, 20, 20)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
p2

# Box-plot above raw data for null models, these graphs are used later on
p2_ns <- ggplot(
  BD_herbivory_1MO_clean2,  # Use your raw data
  aes(
    x = Site,
    y = StartEndDiff * 100)) + 
  
  ylim(0, 40) +
  
  # Adding jitter for the observed data points
  geom_point(
    alpha = 0.2,
    position = position_jitter(width = 0.2),
    colour = "#52854C") +  # Jitter width to spread the points around the site axis
  
  # Adding the box plot with means and percentiles
  geom_boxplot(
    aes(fill = NA),  # Optional fill by site for better visual separation
    outlier.shape = NA,  # Remove outliers (optional)
    alpha = 0, 
    width = 0.5) +  # Adjust box width
  
  # Adding mean points on top of the box plot
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 20, 
    size = 4, 
    color = "black") +  # Black dots representing the mean
  
  labs(
    x = "Site",  
    y = expression(atop("Change of herbivory damage", 
                        "over one month (%)"))) +
  
  theme(
    text = element_text(size = 18),
    legend.position = "none") +  # Hide legend if not needed
  
  scale_x_discrete(limits = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
        plot.margin = margin(20, 20, 20, 20)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
p2_ns

# Calculate mean and standard deviation for each site
summary_stats.diff <- BD_herbivory_1MO_clean2 %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(StartEndDiff, na.rm = TRUE),  # Calculate mean
    SD = sd(StartEndDiff, na.rm = TRUE)        # Calculate standard deviation
  )

# View the summary statistics
print(summary_stats.diff)


 
library(ggpubr)
ggarrange(p1_ns, p2_ns,  
          labels = c("A)", "B)"),
          ncol = 2, nrow = 1)

#----------------------------------------------------------#
# 3.  Build models for A and C  -----
#----------------------------------------------------------#
# i.e. very beginning standing herbivory which you observe if you walk
# to a forest at the very beginning of the season 
dataset_herbivory_1MO <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_1MO_6sites_20241011_wideTolong.csv")
summary(dataset_herbivory_1MO)

custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_herbivory_1MO$Site <- factor(dataset_herbivory_1MO$Site, levels = custom_order)

dataset_herbivory_1MO$Site <-as.factor(dataset_herbivory_1MO$Site)           #
dataset_herbivory_1MO$Patrol <-as.factor(dataset_herbivory_1MO$Patrol)
dataset_herbivory_1MO$SpeciesAnal <-as.factor(dataset_herbivory_1MO$SpeciesAnal) 
dataset_herbivory_1MO$TwigCodeUnique <-as.factor(dataset_herbivory_1MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_1MO$SaplingCodeUnique <-as.factor(dataset_herbivory_1MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
summary(dataset_herbivory_1MO)

AC_herbivory_1MO <- dataset_herbivory_1MO[ which(dataset_herbivory_1MO$Patrol=='A' | dataset_herbivory_1MO$Patrol=='C'), ]
summary(AC_herbivory_1MO)

AC_herbivory_1MO_clean <- AC_herbivory_1MO[!is.na(AC_herbivory_1MO$TwigHerbProp), ]
summary(AC_herbivory_1MO_clean)

glm.ACherb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = AC_herbivory_1MO_clean)
glm.ACherb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                          data = AC_herbivory_1MO_clean)

# compare the models
library(bbmle)
AICctab(glm.ACherb.null, glm.ACherb.site)

glm_ACherbivory_emmeans <-
  emmeans(
    glm.ACherb.site,
    pairwise ~ Site,
    type = "response")
glm_ACherbivory_emmeans


# Convert the contrasts to a data frame
contrast_summary <- as.data.frame(glm_ACherbivory_emmeans$contrasts)

# Apply the Bonferroni correction
adjusted_pvalues <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# Summarize the adjusted p-values
summary(adjusted_pvalues)

# Add adjusted p-values to the contrast summary
contrast_summary$bonferroni_p <- p.adjust(contrast_summary$p.value, method = "bonferroni")

# View the updated contrast table
print(contrast_summary)


p3<-(model_plot_03a <- 
       glm_ACherbivory_emmeans$emmeans %>% 
       as_tibble() %>% 
       ggplot(
         aes(
           x=Site,
           y = response*100)) + 
       
       ylim(0,20) +
       
       # Adding jitter for the observed data points
       geom_point(
         data = AC_herbivory_1MO,
         aes(y = TwigHerbProp * 100),  # No fill or color applied here
         alpha = 0.2,
         position = position_jitter(width = 0.2),
         colour = "#52854C") +  # Jitter width to spread the points around the site axis
       
       
       geom_errorbar(
         aes(
           ymin = asymp.LCL * 100,
           ymax = asymp.UCL * 100),
         width = 0.3,
         size = 2) +
       
       geom_point(
         shape = 16,
         size = 4) +
       
       labs(
         x = "Site",  
         y = expression(atop("Standing herbivory damage", 
                          "at the beginning of growing season (%)"))) +
       
       theme(
         text = element_text(size = 18),
         legend.position = "top")) +
  
  scale_x_discrete(limits = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
    plot.margin = margin(20, 20, 20, 20)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
p3

# Calculate mean and standard deviation for each site
summary_stats.ac <- AC_herbivory_1MO_clean %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TwigHerbProp, na.rm = TRUE),  # Calculate mean
    SD = sd(TwigHerbProp, na.rm = TRUE)        # Calculate standard deviation
  )

# View the summary statistics
print(summary_stats.ac)

library(ggpubr)
ggarrange(p3, p2_ns, p1_ns,   
          labels = c("A)", "B)", "C)"),
          ncol = 3, nrow = 1)
