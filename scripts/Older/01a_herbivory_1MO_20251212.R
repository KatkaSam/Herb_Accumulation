#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  12Dec2025
#
#                   Analyses from 1MO
#
#----------------------------------------------------------#
# Generates Figure 2 in the main text on the associated manuscript

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

dataset_herbivory_1MO <- read.csv(
  "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_1MO_6sites_20241023_wideTolong_v2.csv")
summary(dataset_herbivory_1MO)

# Assign climatic zone
dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))

# Add latitude data
Sites <- data.frame(Site = c("LAK", "TOM",  "EUC", "BUB", "DRO", "KAK"),
                    Lat = c(51.2, 42.68, 33.62, 21.6, 16.1, 5.13))

dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  left_join(Sites, by = "Site")
summary(dataset_herbivory_1MO)

# Set factor levels and structure
custom_order <- c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")  # Replace with your desired order
dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  mutate(
    Site = factor(Site, levels = custom_order),
    Patrol = factor(Patrol),
    SpeciesAnal = factor(SpeciesAnal),
    TwigCodeUnique = factor(TwigCodeUnique),
    SaplingCodeUnique = factor(SaplingCodeUnique),
    Climate = factor(Climate)
  )
summary(dataset_herbivory_1MO)

# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6

##----------------------------------------------------------#
# 1. Build models for A and C (initial standing herbivory) -----
#----------------------------------------------------------#
# i.e. very beginning standing herbivory which you observe if you walk
# to a forest at the very beginning of the season 
AC_herbivory_1MO <- dataset_herbivory_1MO %>%
  filter(Patrol %in% c("A", "C"))
summary(AC_herbivory_1MO)

AC_herbivory_1MO_clean <- AC_herbivory_1MO[!is.na(AC_herbivory_1MO$TwigHerbProp), ]
summary(AC_herbivory_1MO_clean)

AC_herbivory_1MO_clean <- AC_herbivory_1MO %>%
  filter(!is.na(TwigHerbProp)) %>%
  mutate(
    Climate = case_when(
      Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
      Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
      TRUE ~ NA_character_
    ),
    Climate = factor(Climate)
  )
summary(AC_herbivory_1MO_clean)

# SaplingCodeUnique codes site_species_individual_patrol .... for example BUB_Baccaurea_ramiflora_2CN1_D

# Mixed models
glm.ACherb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                        data = AC_herbivory_1MO_clean,
                        na.action = "na.exclude")
glm.ACherb.lat <-glmmTMB(TwigHerbProp +0.0001 ~ poly(Lat,2) +
                         + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                       data = AC_herbivory_1MO_clean,
                       na.action = "na.exclude")
glm.ACherb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                             + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                           data = AC_herbivory_1MO_clean,
                           na.action = "na.exclude")

# Compare models
AICctab(glm.ACherb.null, glm.ACherb.lat, glm.ACherb.climate)

# Colours for climates
custom_cols <- c("Temperate" = "#1f78b4",   # dark green
                "Tropical"  = "#e31a1c")   # orange

## Predict the values
newData <- data.frame(
  Lat = rep(seq(55, 0, length.out = 500), 2),
  Climate = rep(c("Tropical", "Temperate"), each = 500),
  SpeciesAnal = factor(
    "Acacia_parramattensis",
    levels = levels(model.frame(glm.ACherb.lat)$SpeciesAnal)
  ),
  SaplingCodeUnique = factor(
    "1",
    levels = levels(model.frame(glm.ACherb.lat)$SaplingCodeUnique)
  )
)

# Predictions with CI
preds <- predict(glm.ACherb.lat, newdata = newData, se.fit = TRUE, type = "response")
newData$Herbivory <- preds$fit
newData$SE <- preds$se.fit
z_value <- qnorm(0.975)
newData <- newData %>%
  mutate(
    LowerCI = Herbivory - z_value * SE,
    UpperCI = Herbivory + z_value * SE
  )

# Plot quickly to see the prediction line
model_plot_01 <-plot(AC_herbivory_1MO_clean$TwigHerbProp ~ 
                       jitter(AC_herbivory_1MO_clean$Lat), col = c("#1f78b4", "#e31a1c")[as.numeric(as.factor(AC_herbivory_1MO_clean$Climate))])
lines(newData$Lat[newData$Climate == "Tropical"], 
      newData$Herbivory[newData$Climate == "Tropical"], col = "black")
lines(newData$Lat[newData$Climate == "Temperate"], 
      newData$Herbivory[newData$Climate == "Temperate"], col = "black")

#Print NewDataDensity to see the predictions and intervals
summary(newData)
str(newData)

# Save predictions
newData %>%
  as_tibble() %>%
  write_csv("data/output/Predictions_herbivory_CI_20251212.csv")

#----------------------------------------------------------#
# Plotting AC start
#----------------------------------------------------------#
# Graphical theme defaults
theme_set(theme_classic())
PDF_width <- 10
PDF_height <- 6

P1 <- ggplot()+ 
  # Raw observed points in site colours
  geom_point(
    data = AC_herbivory_1MO_clean,
    aes(x = Lat, y = TwigHerbProp * 100, colour = Climate),
    alpha = 0.2,
    position = position_jitter(width = 2, height = 0.005),
    show.legend = FALSE   # hide site colours from legend
  ) + 
  
  # Add the confidence interval ribbon
  geom_ribbon(
    data = newData,
    aes(
      x = Lat,
      ymin = LowerCI*100,
      ymax = UpperCI*100),
    alpha = 0.1) +
  
  # Add the fitted line
  geom_line(
    data = newData,
    aes(
      x = Lat,
      y = Herbivory*100,
      col = "black"),
    size = 1, linetype = "dashed") +
  
  
  scale_x_continuous(
    trans = "reverse",
    breaks = c(51.2, 42.68, 33.62, 21.6, 16.1, 5.13),
    labels = c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")
  ) +
  
  labs(
    x = "Latitude",  
    y = expression(atop("Standing herbivory damage", 
                        "at the beginning of season (%)"))) +
    theme(
    text = element_text(size = 14),
    legend.position = "none") +
  
  scale_fill_manual(values = c("#1f78b4", "#e31a1c", "black")) +
  scale_color_manual(values = c("#1f78b4", "#e31a1c", "black")) +
  
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
P1

#----------------------------------------------------------#
# Summary statistics by site
#----------------------------------------------------------#

summary_stats.ac <- AC_herbivory_1MO_clean %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TwigHerbProp, na.rm = TRUE),
    SD = sd(TwigHerbProp, na.rm = TRUE)
  )
print(summary_stats.ac)

#----------------------------------------------------------#
# 2. Build models for CHANGE IN HERBIVORY B-A and D-C -  P2 CHANGE ----- 
#----------------------------------------------------------#
# i.e. change in herbivory as it accumulated over a monthy 
# since the beginning of the season
dataset_herbivory_1MO <- read.csv(
  "C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/HerbivorySum_1MO_6sites_20241023_wideTolong_v2.csv")
summary(dataset_herbivory_1MO)

# Assign climatic zone
dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))

# Add latitude data
Sites <- data.frame(Site = c("LAK", "TOM",  "EUC", "BUB", "DRO", "KAK"),
                    Lat = c(51.2, 42.68, 33.62, 21.6, 16.1, 5.13))


dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  left_join(Sites, by = "Site")
summary(dataset_herbivory_1MO)

# Set factor levels and structure
custom_order <- c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")  # Replace with your desired order
dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  mutate(
    Site = factor(Site, levels = custom_order),
    Patrol = factor(Patrol),
    SpeciesAnal = factor(SpeciesAnal),
    TwigCodeUnique = factor(TwigCodeUnique),
    SaplingCodeUnique = factor(SaplingCodeUnique),
    Climate = factor(Climate)
  )
summary(dataset_herbivory_1MO)

BD_herbivory_1MO <- dataset_herbivory_1MO[ which(dataset_herbivory_1MO$Patrol=='B' | dataset_herbivory_1MO$Patrol=='D'), ]
summary(BD_herbivory_1MO)

BD_herbivory_1MO_clean <- BD_herbivory_1MO[!is.na(BD_herbivory_1MO$TwigHerbProp), ]
summary(BD_herbivory_1MO_clean)

BD_herbivory_1MO$StartEndDiff <- ifelse(BD_herbivory_1MO$StartEndDiff < 0, 0, BD_herbivory_1MO$StartEndDiff)
summary(BD_herbivory_1MO)

BD_herbivory_1MO_clean2 <- BD_herbivory_1MO[!is.na(BD_herbivory_1MO$StartEndDiff), ]
summary(BD_herbivory_1MO_clean2)

BD_herbivory_1MO_clean2 <- BD_herbivory_1MO_clean2 %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))
summary(BD_herbivory_1MO_clean2)

BD_herbivory_1MO_clean2 <- BD_herbivory_1MO_clean2 %>%
  mutate(
    Site = factor(Site, levels = custom_order),
    Patrol = factor(Patrol),
    SpeciesAnal = factor(SpeciesAnal),
    TwigCodeUnique = factor(TwigCodeUnique),
    SaplingCodeUnique = factor(SaplingCodeUnique),
    Climate = factor(Climate)
  )
summary(BD_herbivory_1MO_clean2)

glm.CHherb.null <-glmmTMB(StartEndDiff + 0.0001 ~ 1 +
                            (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                          data = BD_herbivory_1MO_clean2,
                          na.action = "na.exclude")
glm.CHherb.lat <-glmmTMB(StartEndDiff +0.0001 ~ poly(Lat,2) +
                            (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                          data = BD_herbivory_1MO_clean2,
                          na.action = "na.exclude")
glm.CHherb.climate <-glmmTMB(StartEndDiff +0.0001 ~ Climate +
                              (1|SpeciesAnal) + (1|SaplingCodeUnique) , family = beta_family(),
                             data = BD_herbivory_1MO_clean2,
                             na.action = "na.exclude")

# Compare models
AICctab(glm.CHherb.null, glm.CHherb.lat, glm.CHherb.climate)

## Predict the values
newData_CH<- data.frame(
  Lat = rep(seq(55, 0, length.out = 500), 2),
  Climate = rep(c("Tropical", "Temperate"), each = 500),
  SpeciesAnal = factor(
    "Acacia_parramattensis",
    levels = levels(model.frame(glm.CHherb.lat)$SpeciesAnal)
  ),
  SaplingCodeUnique = factor(
    "EUC_Acacia_parramattensis_1CN1_B",
    levels = levels(model.frame(glm.CHherb.lat)$SaplingCodeUnique)
  )
)
newData_CH

# Predict density values with standard errors
preds <- predict(glm.CHherb.lat, newdata = newData_CH, se.fit = TRUE, type = "response")

# Extract predictions and standard errors
newData_CH$Herbivory <- preds$fit
newData_CH$SE <- preds$se.fit

# Plot quickly to see the prediction line
model_plot_01 <-plot(BD_herbivory_1MO_clean2$TwigHerbProp ~ 
                       jitter(BD_herbivory_1MO_clean2$Lat), col = c("#1f78b4", "#e31a1c")[as.numeric(as.factor(BD_herbivory_1MO_clean2$Climate))])
lines(newData_CH$Lat[newData_CH$Climate == "Tropical"], 
      newData_CH$Herbivory[newData_CH$Climate == "Tropical"], col = "black")
lines(newData_CH$Lat[newData_CH$Climate == "Temperate"], 
      newData_CH$Herbivory[newData_CH$Climate == "Temperate"], col = "black")

# Predictions with CI
z_value <- qnorm(0.975)
newData_CH <- newData_CH %>%
  mutate(
    LowerCI = Herbivory - z_value * SE,
    UpperCI = Herbivory + z_value * SE
  )
newData_CH

#Print NewDataDensity to see the predictions and intervals
summary(newData_CH)
str(newData_CH)

newData %>% 
  as_tibble() %>% 
  write_csv("data/output/Predictions_change_herbivory_CI_20251212.csv")

############# draw the resulting plot
P2 <- ggplot()+ 
  # Raw observed points in site colours
  geom_point(
    data = BD_herbivory_1MO_clean2,
    aes(x = Lat, y = StartEndDiff * 100, colour = Climate),
    alpha = 0.2,
    position = position_jitter(width = 2, height = 0.005),
    show.legend = FALSE   # hide site colours from legend
  ) + 
  
  # Add the confidence interval ribbon
  geom_ribbon(
    data = newData_CH,
    aes(
      x = Lat,
      ymin = LowerCI*100,
      ymax = UpperCI*100),
    alpha = 0.1) +
  
  # Add the fitted line
  geom_line(
    data = newData_CH,
    aes(
      x = Lat,
      y = Herbivory*100,
      col = "black"),
    size = 1, linetype = "dashed") +
  
  # adjust Y scale
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(0,35,5)
  ) +
  
  labs(
    x = "Latitude",  
    y = expression(atop("Change of herbivory damage", 
                        "over one month (%)"))) +
  
  theme(
    text = element_text(size = 14),
    legend.position = "none") +
  
  scale_x_continuous(
    trans = "reverse",
    breaks = c(51.2, 42.68, 33.62, 21.6, 16.1, 5.13),
    labels = c("LAK", "TOM", "EUC", "BUB", "DRO", "KAK")
  ) +
  
  scale_fill_manual(values = c("#1f78b4", "#e31a1c", "black")) +
  scale_color_manual(values = c("#1f78b4", "#e31a1c", "black")) +
  
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),  # Adjust margin for y-axis label
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Adjust overall plot margin
  
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)   # Center y-axis label
  )
P2

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
ggarrange(P1, P2,  
          labels = c("A)", "B)"),
          ncol = 2, nrow = 1)

#----------------------------------------------------------#
# 3. Build models for B and D standing herbivory           P3 END----- 
#-----------------------------------------------------#
#now remove the A and C patrols, as the beginnings of the experiment
# B and D now actually represent 2 separate years in which the experiment was conducted

summary(dataset_herbivory_1MO)
BD_herbivory_1MO <- dataset_herbivory_1MO[ which(dataset_herbivory_1MO$Patrol=='B' | dataset_herbivory_1MO$Patrol=='D'), ]
summary(BD_herbivory_1MO)

BD_herbivory_1MO_clean <- BD_herbivory_1MO[!is.na(BD_herbivory_1MO$TwigHerbProp), ]
summary(BD_herbivory_1MO_clean)


glm.herb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                           + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                        data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")
glm.herb.lat <-glmmTMB(TwigHerbProp +0.0001 ~ poly(Lat,2) +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                          data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")
glm.herb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique), family = beta_family(),
                        data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")
# compare the models
AICctab(glm.herb.null, glm.herb.lat, glm.herb.climate)
# if latitude is better than climate, it means that there is something specific to each site/latitude
# whis is not the case ... climate model is now the best

# correct results, for the Climate only
glm_herbClimate_emmeans <- emmeans(glm.herb.climate, pairwise ~ Climate, type = "response", adjust = "tukey")
# View the results
glm_herbClimate_emmeans$contrasts

custom_cols <- c("Temperate" = "#1f78b4",   # dark green
                 "Tropical"  = "#e31a1c")   # orange


# Plot the results ... raw data per sites, overlapped by the means of climate model - with temperate sites close to each other

# --- 1. Define site positions (sorted by latitude) ---
site_positions <- tibble(
  Site = c("LAK", "TOM", "EUC", "BUB",  "DRO", "KAK"),
  x_pos = c(1, 2, 3,   # Temperate sites (close together)
            4, 5, 6) # Tropical sites (close together, but far from temperates)
)

# --- 2. Clean raw data and add x_pos & Climate ---
BD_herbivory_1MO_clean <- BD_herbivory_1MO_clean %>%
  select(-matches("x_pos\\.")) %>%  # drop any old x_pos.x / x_pos.y
  mutate(
    Climate = if_else(Site %in% c("LAK","TOM","EUC"),
                      "Temperate", "Tropical")
  ) %>%
  left_join(site_positions, by = "Site")

summary(BD_herbivory_1MO_clean)  # check only one x_pos exists, rerun if not


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
  "LAK" = "#1f78b4",  "TOM" = "#1f78b4",  "EUC" = "#1f78b4",
  "BUB" = "#e31a1c",  "KAK" = "#e31a1c",  "DRO" = "#e31a1c"
)
custom_climate_cols <- c(
  "Temperate" = "#1f78b4",
  "Tropical"  = "#e31a1c"
)


# --- 6. Plot ---
P3<-ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_1MO_clean,
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
    x = "Latitude",
    y = expression(atop("Standing herbivory damage",
                        "one month into the growing season (%)"))
  ) +
  
  theme(
    text = element_text(size = 14),
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

library(ggpubr)
ggarrange(P1, P2, P3,   
          labels = c("A)", "B)", "C)"),
          ncol = 3, nrow = 1)
