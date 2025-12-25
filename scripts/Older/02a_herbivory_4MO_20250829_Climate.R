#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#                   Analyses from 4MO
#
#----------------------------------------------------------#
# Generates Figure 3 in the main text

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

dataset_herbivory_4MO <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv")
summary(dataset_herbivory_4MO)

custom_order <- c("LAK", "TOM", "EUC", "BUB","DRO", "KAK")  # Replace with your desired order
dataset_herbivory_4MO$Site <- factor(dataset_herbivory_4MO$Site, levels = custom_order)
dataset_herbivory_4MO$Site <-as.factor(dataset_herbivory_4MO$Site) #
dataset_herbivory_4MO$Species <-as.factor(dataset_herbivory_4MO$Species) 
dataset_herbivory_4MO$Patrol <-as.factor(dataset_herbivory_4MO$Patrol)
dataset_herbivory_4MO$SpeciesAnal <-as.factor(dataset_herbivory_4MO$SpeciesAnal) 
dataset_herbivory_4MO$TwigCodeUnique <-as.factor(dataset_herbivory_4MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_4MO$SaplingCodeUnique <-as.factor(dataset_herbivory_4MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
summary(dataset_herbivory_4MO)

dataset_herbivory_4MO <- dataset_herbivory_4MO %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "DRO", "KAK") ~ "Tropical",
    TRUE ~ NA_character_
  ))
dataset_herbivory_4MO$Climate <-as.factor(dataset_herbivory_4MO$Climate)


dataset_herbivory_4MO$TwigCodeUniqueSP <- paste(dataset_herbivory_4MO$TwigCodeUnique, dataset_herbivory_4MO$Species, sep = "_")
dataset_herbivory_4MO$TwigCodeUniqueSP <- as.factor(dataset_herbivory_4MO$TwigCodeUniqueSP)

dataset_herbivory_4MO$SaplingCodeUniqueSP <- paste(dataset_herbivory_4MO$SaplingCodeUnique, dataset_herbivory_4MO$Species, sep = "_")
dataset_herbivory_4MO$SaplingCodeUniqueSP <-as.factor(dataset_herbivory_4MO$SaplingCodeUniqueSP)
summary(dataset_herbivory_4MO)

# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6

#----------------------------------------------------------#
# 2. Build models for B and D standing herbivory -----
#----------------------------------------------------------#

#now remove the A and C patrols, as the beginnings of the experiment
# B and D now actually represent 2 separate years in which the experiment was conducted
BD_herbivory_4MO <- dataset_herbivory_4MO[ which(dataset_herbivory_4MO$Patrol=='B' | dataset_herbivory_4MO$Patrol=='D'), ]
summary(BD_herbivory_4MO)

BD_herbivory_4MO_clean <- BD_herbivory_4MO[!is.na(BD_herbivory_4MO$TwigHerbProp), ]
summary(BD_herbivory_4MO_clean)

sum(BD_herbivory_4MO_clean$TwigHerbProp == 0)
sum(BD_herbivory_4MO_clean$TwigHerbProp == 1)

glm.herb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                           + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                        data = BD_herbivory_4MO_clean,
                        na.action = "na.exclude")
glm.herb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                          + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                          data = BD_herbivory_4MO_clean,
                        na.action = "na.exclude")
glm.herb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                             + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP) + (1|Patrol), family = beta_family(),
                           data = BD_herbivory_4MO_clean,
                           na.action = "na.exclude")


glm.herb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                          + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP), family = beta_family(),
                        data = BD_herbivory_4MO_clean,
                        na.action = "na.exclude")
glm.herb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                          + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP), family = beta_family(),
                        data = BD_herbivory_4MO_clean,
                        na.action = "na.exclude")
glm.herb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                             + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP) , family = beta_family(),
                           data = BD_herbivory_4MO_clean,
                           na.action = "na.exclude")
glm.herb.patrol <-glmmTMB(TwigHerbProp +0.0001 ~ Patrol +
                             + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP) , family = beta_family(),
                           data = BD_herbivory_4MO_clean,
                           na.action = "na.exclude")
glm.herb.patrolSite <-glmmTMB(TwigHerbProp +0.0001 ~ Patrol + Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP) , family = beta_family(),
                          data = BD_herbivory_4MO_clean,
                          na.action = "na.exclude")

# compare the models
library(bbmle)
AICctab(glm.herb.null, glm.herb.site, glm.herb.climate)

summary(glm.herb.climate)
library(MuMIn)
r.squaredGLMM(glm.herb.climate)


# I am enforcing the SITE model, because this is what I want to see 
# below are correct figures for null models
# correct results, for the Climate only
glm_herbClimate_emmeans <- emmeans(glm.herb.climate, pairwise ~ Climate, type = "response", adjust = "tukey")
# View the results
glm_herbClimate_emmeans$contrasts

custom_cols <- c("Temperate" = "#1f78b4",   # dark green
                 "Tropical"  = "#e31a1c")   # orange

# --- 1. Define site positions (sorted by latitude) ---
site_positions <- tibble(
  Site = c("LAK", "TOM", "EUC", "BUB",  "DRO", "KAK"),
  x_pos = c(1, 2, 3,   # Temperate sites (close together)
            4, 5, 6) # Tropical sites (close together, but far from temperates)
)

# --- 2. Clean raw data and add x_pos & Climate ---
BD_herbivory_4MO_clean <- BD_herbivory_4MO_clean %>%
  select(-matches("x_pos\\.")) %>%  # drop any old x_pos.x / x_pos.y
  mutate(
    Climate = if_else(Site %in% c("LAK","TOM","EUC"),
                      "Temperate", "Tropical")
  ) %>%
  left_join(site_positions, by = "Site")

summary(BD_herbivory_4MO_clean)  # check only one x_pos exists, rerun if not


# --- 3. Prepare climate means ---
climate_means <- glm_herbClimate_emmeans$emmeans %>%
  as_tibble() %>%
  mutate(
    Climate = factor(Climate, levels = c("Temperate","Tropical")),
    x_pos_min = ifelse(
      Climate == "Temperate", 
      min(site_positions$x_pos[c(1,2,3)])- 0.3,  # extend a bit before site 1
      min(site_positions$x_pos[c(4,5,6)]) - 0.3   # extend a bit before site 4
    ),
    x_pos_max = ifelse(
      Climate == "Temperate", 
      max(site_positions$x_pos[c(1,2,3)]) + 0.3,  # extend a bit after site 3
      max(site_positions$x_pos[c(4,5,6)]) + 0.3   # extend a bit after site 6
    ),
    x_pos_line = (x_pos_min + x_pos_max)/2
  )

# --- 4. Create ribbon data ---
climate_ribbon <- climate_means %>%
  rowwise() %>%
  mutate(
    x_vals = list(c(x_pos_min, x_pos_max)),
    ymin_vals = list(rep(asymp.LCL*100,2)),
    ymax_vals = list(rep(asymp.UCL*100,2))
  ) %>%
  unnest(c(x_vals, ymin_vals, ymax_vals)) %>%
  rename(x = x_vals, ymin = ymin_vals, ymax = ymax_vals)

# --- 5. Define colours ---
custom_site_cols <- c(
  "LAK" = "#3182bd",  "TOM" = "#3182bd",  "EUC" = "#3182bd",
  "BUB" = "#fb6a4a",  "KAK" = "#fb6a4a",  "DRO" = "#fb6a4a"
)
custom_climate_cols <- c(
  "Temperate" = "#084594",
  "Tropical"  = "#b10026"
)

# --- 6. Plot ---
P3<-ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_4MO_clean,
    aes(x = x_pos, y = TwigHerbProp*100, colour = Site),
    alpha = 0.2, position = position_jitter(width = 0.2),
    show.legend = FALSE
  ) +
  
  # adjust Y scale
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(0,35,5)
  ) +
  
  # CI ribbons
  geom_ribbon(
    data = climate_ribbon,
    aes(x = x, ymin = ymin, ymax = ymax, fill = Climate, group = Climate),
    alpha = 0.2
  ) +
  
  # Mean lines
  geom_line(
    data = climate_means %>%
      rowwise() %>%
      mutate(x = list(c(x_pos_min, x_pos_max)),
             y = list(rep(response*100, 2))) %>%
      unnest(c(x,y)),
    aes(x = x, y = y, colour = Climate, group = Climate),
    size = 1.5
  ) +
  
  
  scale_x_continuous(
    breaks = site_positions$x_pos,
    labels = site_positions$Site
  ) +
  
  scale_colour_manual(values = c(custom_site_cols, custom_climate_cols)) +
  scale_fill_manual(values = custom_climate_cols) +
  
  labs(
    x = "Site (ordered by latitude)",
    y = expression(atop("Standing herbivory damage",
                        "four months into the growing season (%)"))
  ) +
  
  theme(
    text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20,20,20,20)
  ) +
  
  # overlap line and ribbon in legend
  guides(
    colour = guide_legend(override.aes = list(fill = NA, linewidth = 1.5)),
    fill   = guide_legend(override.aes = list(alpha = 0.2))
  )
P3

# Calculate mean and standard deviation for each site
summary_stats <- BD_herbivory_4MO_clean %>%
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

BD_herbivory_4MO$StartEndDiff <- ifelse(BD_herbivory_4MO$StartEndDiff < 0, 0, BD_herbivory_4MO$StartEndDiff)
#BD_herbivory_4MO$StartEndDiff <- abs(BD_herbivory_4MO$StartEndDiff)
summary(BD_herbivory_4MO)

BD_herbivory_4MO_clean2 <- BD_herbivory_4MO[!is.na(BD_herbivory_4MO$StartEndDiff), ]
summary(BD_herbivory_4MO_clean2)

glm.CHherb.null <-glmmTMB(StartEndDiff + 0.0001 ~ 1 +
                          + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                        data = BD_herbivory_4MO_clean2,
                        na.action = "na.exclude")
glm.CHherb.site <-glmmTMB(StartEndDiff +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                        data = BD_herbivory_4MO_clean2,
                        na.action = "na.exclude")
glm.CHherb.Climate <-glmmTMB(StartEndDiff +0.0001 ~ Climate +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                          data = BD_herbivory_4MO_clean2,
                          na.action = "na.exclude")

# compare the models
AICctab(glm.CHherb.null, glm.CHherb.site, glm.CHherb.Climate)

# I am enforcing the SITE model, because this is what I want to see 
glm_CH.herbivory_emmeans <-
  emmeans(
    glm.CHherb.site,
    pairwise ~ Site,
    type = "response")
glm_CH.herbivory_emmeans

# Tukey-corrected pairwise comparisons
glm_CH.herbivory_emmeans <- emmeans(glm.CHherb.site, pairwise ~ Site, type = "response", adjust = "tukey")

# View the results
glm_CH.herbivory_emmeans$contrasts

# Add Climate emmeans
site_climate <- tibble::tibble(
  Site = c("LAK","TOM","EUC","BUB","DRO","KAK"),
  Climate = c("Temperate","Temperate","Temperate",
              "Tropical","Tropical","Tropical")
)
BDemmeans_with_climate <- glm_CH.herbivory_emmeans$emmeans %>%
  as_tibble() %>%
  left_join(site_climate, by = "Site")


P2 <- ggplot()+ 
  # Raw observed points in site colours
  geom_point(
    data = BD_herbivory_4MO_clean2,
    aes(x = Site, y = StartEndDiff * 100, colour = Site),
    alpha = 0.2,
    position = position_jitter(width = 0.2),
    show.legend = FALSE   # hide site colours from legend
  ) + 
  
  # Error bars in climate colours
  geom_errorbar(
    data = BDemmeans_with_climate,
    aes(x = Site, 
        ymin = asymp.LCL * 100, ymax = asymp.UCL * 100, colour = Climate),
    width = 0.3,
    size = 1.2
  ) +
  
  # Means in climate colours
  geom_point(
    data = BDemmeans_with_climate,
    aes(x = Site, y = response * 100, colour = Climate),
    shape = 16, size = 4
  ) +
  
  # adjust Y scale
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(0,35,5)
  ) +
  
  # manual colours
  scale_colour_manual(values = c(custom_site_cols, custom_climate_cols)) +
  
  
  labs(
    x = "Site",  
    y = expression(atop("Change of herbivory damage", 
                        "over four months (%)"))) +
  
  scale_x_discrete(limits = c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")) +
  
  # Theme
  theme(
    text = element_text(size = 16),
    legend.position = "none",            # remove legend
    axis.title = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )
P2

# Calculate mean and standard deviation for each site
summary_stats.diff <- BD_herbivory_4MO_clean2 %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(StartEndDiff, na.rm = TRUE),  # Calculate mean
    SD = sd(StartEndDiff, na.rm = TRUE)        # Calculate standard deviation
  )

# View the summary statistics
print(summary_stats.diff)


#----------------------------------------------------------#
# 3.  Build models for A and C  -----
#----------------------------------------------------------#
# i.e. very beginning standing herbivory which you observe if you walk
# to a forest at the very beginning of the season 
dataset_herbivory_4MO <- read.csv("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv")
summary(dataset_herbivory_4MO)

custom_order <- c("LAK", "TOM", "EUC", "BUB",  "DRO", "KAK" )  # Replace with your desired order
dataset_herbivory_4MO$Site <- factor(dataset_herbivory_4MO$Site, levels = custom_order)

dataset_herbivory_4MO$Site <-as.factor(dataset_herbivory_4MO$Site)           #
dataset_herbivory_4MO$Patrol <-as.factor(dataset_herbivory_4MO$Patrol)
dataset_herbivory_4MO$SpeciesAnal <-as.factor(dataset_herbivory_4MO$SpeciesAnal) 
dataset_herbivory_4MO$TwigCodeUnique <-as.factor(dataset_herbivory_4MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_4MO$SaplingCodeUnique <-as.factor(dataset_herbivory_4MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
summary(dataset_herbivory_4MO)

AC_herbivory_4MO <- dataset_herbivory_4MO[ which(dataset_herbivory_4MO$Patrol=='A' | dataset_herbivory_4MO$Patrol=='C'), ]
summary(AC_herbivory_4MO)

AC_herbivory_4MO <- AC_herbivory_4MO %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "DRO", "KAK") ~ "Tropical",
    TRUE ~ NA_character_
  ))
AC_herbivory_4MO$Climate <-as.factor(AC_herbivory_4MO$Climate)


AC_herbivory_4MO$TwigCodeUniqueSP <- paste(AC_herbivory_4MO$TwigCodeUnique, AC_herbivory_4MO$Species, sep = "_")
AC_herbivory_4MO$TwigCodeUniqueSP <- as.factor(AC_herbivory_4MO$TwigCodeUniqueSP)

AC_herbivory_4MO$SaplingCodeUniqueSP <- paste(AC_herbivory_4MO$SaplingCodeUnique, AC_herbivory_4MO$Species, sep = "_")
AC_herbivory_4MO$SaplingCodeUniqueSP <-as.factor(AC_herbivory_4MO$SaplingCodeUniqueSP)
summary(AC_herbivory_4MO)


AC_herbivory_4MO_clean <- AC_herbivory_4MO[!is.na(AC_herbivory_4MO$TwigHerbProp), ]
summary(AC_herbivory_4MO_clean)

glm.ACherb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                        data = AC_herbivory_4MO_clean)
glm.ACherb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                          data = AC_herbivory_4MO_clean)
glm.ACherb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                            + (1|SpeciesAnal) + (1|SaplingCodeUniqueSP)+ (1|Patrol), family = beta_family(),
                          data = AC_herbivory_4MO_clean)

# compare the models
library(bbmle)
AICctab(glm.ACherb.null, glm.ACherb.site, glm.ACherb.climate)

glm_ACherbivory_emmeans <-
  emmeans(
    glm.ACherb.site,
    pairwise ~ Site,
    type = "response")
glm_ACherbivory_emmeans

# Tukey-corrected pairwise comparisons
glm_ACherbivory_emmeans <- emmeans(glm.ACherb.site, pairwise ~ Site, type = "response", adjust = "tukey")

# View the results
glm_ACherbivory_emmeans$contrasts

# Add Climate emmeans
site_climate <- tibble::tibble(
  Site = c("LAK","TOM","EUC","BUB","DRO","KAK"),
  Climate = c("Temperate","Temperate","Temperate",
              "Tropical","Tropical","Tropical")
)
ACemmeans_with_climate <- glm_ACherbivory_emmeans$emmeans %>%
  as_tibble() %>%
  left_join(site_climate, by = "Site")

P1 <- ggplot()+ 
  # Raw observed points in site colours
  geom_point(
    data = AC_herbivory_4MO_clean,
    aes(x = Site, y = TwigHerbProp * 100, colour = Site),
    alpha = 0.2,
    position = position_jitter(width = 0.2),
    show.legend = FALSE   # hide site colours from legend
  ) + 
  
  # Error bars in climate colours
  geom_errorbar(
    data = ACemmeans_with_climate,
    aes(x = Site, 
        ymin = asymp.LCL * 100, ymax = asymp.UCL * 100, colour = Climate),
    width = 0.3,
    size = 1.2
  ) +
  
  # Means in climate colours
  geom_point(
    data = ACemmeans_with_climate,
    aes(x = Site, y = response * 100, colour = Climate),
    shape = 16, size = 4
  ) +
  
  # adjust Y scale
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(0,35,5)
  ) +
  
  # manual colours
  scale_colour_manual(values = c(custom_site_cols, custom_climate_cols)) +
  
  
  labs(
    x = "Site",  
    y = expression(atop("Standing herbivory damage", 
                        "at the beginning of season (%)"))) +
  
  scale_x_discrete(limits = c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")) +
  
  # Theme
  theme(
    text = element_text(size = 16),
    legend.position = "none",            # remove legend
    axis.title = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )
P1


# Calculate mean and standard deviation for each site
summary_stats.ac <- AC_herbivory_4MO_clean %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TwigHerbProp, na.rm = TRUE),  # Calculate mean
    SD = sd(TwigHerbProp, na.rm = TRUE)        # Calculate standard deviation
  )

# View the summary statistics
print(summary_stats.ac)

library(ggpubr)
ggarrange(P1, P2, P3,  
          labels = c("A)", "B)", "C)"),
          ncol = 3, nrow = 1)
