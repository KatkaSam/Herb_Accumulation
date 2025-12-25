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

dataset_herbivory_1MO <- dataset_herbivory_1MO %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))

custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_herbivory_1MO$Site <- factor(dataset_herbivory_1MO$Site, levels = custom_order)
dataset_herbivory_1MO$Site <-as.factor(dataset_herbivory_1MO$Site)           #
dataset_herbivory_1MO$Patrol <-as.factor(dataset_herbivory_1MO$Patrol)
dataset_herbivory_1MO$SpeciesAnal <-as.factor(dataset_herbivory_1MO$SpeciesAnal) 
dataset_herbivory_1MO$TwigCodeUnique <-as.factor(dataset_herbivory_1MO$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
dataset_herbivory_1MO$SaplingCodeUnique <-as.factor(dataset_herbivory_1MO$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
dataset_herbivory_1MO$Climate <-as.factor(dataset_herbivory_1MO$Climate)    
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
glm.herb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique) + (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean,
                        na.action = "na.exclude")

# compare the models
library(bbmle)
AICctab(glm.herb.null, glm.herb.site, glm.herb.climate)
# if site is better than climate, it means that there is something specific to each site
# whis is not the case ... climate model is now the best

# I am enforcing the SITE model, because this is what I want to see for inspection only
# below are correct figures for null models
glm_herbivory_emmeans <- emmeans(glm.herb.site, pairwise ~ Site, type = "response", adjust = "tukey")
glm_herbivory_emmeans$contrasts

# correct results, for the Climate only
glm_herbClimate_emmeans <- emmeans(glm.herb.climate, pairwise ~ Climate, type = "response", adjust = "tukey")
# View the results
glm_herbClimate_emmeans$contrasts

custom_cols <- c("Temperate" = "#1f78b4",   # dark green
                 "Tropical"  = "#e31a1c")   # orange

# Take emmeans results and add Climate info based on Site
#plot_data <- glm_herbivory_emmeans$emmeans %>%
#  as_tibble() %>%
#  mutate(Climate = case_when(
#    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
#    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical"
#  ))
#summary(plot_data)

# 1. Create consistent site positions FIRST
site_positions <- tibble(
  Site = c("LAK", "TOM", "EUC", "BUB", "KAK", "DRO"),
  x_pos = c(1, 1.2, 1.4,   # Temperate sites (close together)
            2.2, 2.4, 2.6) # Tropical sites (close together, but far from temperates)
)
site_positions

#model_means <- glm_herbClimate_emmeans$emmeans %>%
#  as_tibble() %>%
#  mutate(
#    x_pos = c(1.8, 3.0),  # place Temperate and Tropical in middle of site clusters
#    Climate = factor(Climate, levels = c("Temperate","Tropical"))
#  )

# 2. Also add Climate to raw data
# plot_data <- plot_data %>% left_join(site_positions, by = "Site")
BD_herbivory_1MO_clean <- BD_herbivory_1MO_clean %>%
  mutate(Climate = case_when(
    Site %in% c("LAK","TOM","EUC") ~ "Temperate",
    Site %in% c("BUB","KAK","DRO") ~ "Tropical"
  )) %>%
  left_join(site_positions, by = "Site")
summary(BD_herbivory_1MO_clean)

# 3. Build model means (placing at "middle" positions manually or as average)
model_means <- glm_herbClimate_emmeans$emmeans %>%
  as_tibble() %>%
  mutate(
    x_pos = c(1.7, 2.9),
    Climate = factor(Climate, levels = c("Temperate","Tropical"))
  )
model_means

# Shades for sites (raw data)
custom_site_cols <- c(
  "LAK" = "#084594",   # dark blue
  "TOM" = "#3182bd",   # medium blue
  "EUC" = "#6baed6",   # light blue
  "BUB" = "#e31a1c",   # dark red
  "KAK" = "#fb6a4a",   # medium red
  "DRO" = "#fcae91"    # light red
)

# Climate colours (model means) – same dark blue & dark red
custom_climate_cols <- c(
  "Temperate" = "#1f78b4",
  "Tropical"  = "#e31a1c"
)

ggplot() +
  # Raw site-level data (shaded colours)
  geom_point(
    data = BD_herbivory_1MO_clean,
    aes(x = x_pos, y = TwigHerbProp * 100, colour = Site),
    alpha = 0.2,
    position = position_jitter(width = 0.05),
    show.legend = FALSE   # hide site legend
  ) +
  ylim(0, 40) +
  
  # Model means + CI (Climate colours)
  geom_errorbar(
    data = model_means,
    aes(x = x_pos, ymin = asymp.LCL*100, ymax = asymp.UCL*100, colour = Climate),
    width = 0.2, size = 2
  ) +
  geom_point(
    data = model_means,
    aes(x = x_pos, y = response*100, colour = Climate),
    size = 4
  ) +
  
  labs(
    x = "Site",
    y = expression(atop("Standing herbivory damage",
                        "one month into the growing season (%)"))
  ) +
  
  scale_x_continuous(
    breaks = site_positions$x_pos,
    labels = site_positions$Site,
    expand = expansion(add = 0.3)
  ) +
  
  # Apply both palettes (site shades + climate colours)
  scale_colour_manual(values = c(custom_site_cols, custom_climate_cols)) +
  
  # One clean theme block
  theme(
    text = element_text(size = 18),
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 20)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )


################ Lines for climate everalying the data
# --- 1) Site positions in the requested order ---
site_positions <- tibble(
  Site = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC"),
  x_pos = 1:6
)

# Padding to extend lines/ribbons a bit beyond site bounds
pad <- 0.3

# --- 2) Clean data, add Climate and x_pos ---
BD_herbivory_1MO_clean <- BD_herbivory_1MO_clean %>%
  select(-matches("x_pos\\.")) %>%     # drop any old x_pos.* leftovers
  mutate(
    Climate = if_else(Site %in% c("LAK","TOM","EUC"),
                      "Temperate", "Tropical")
  ) %>%
  left_join(site_positions, by = "Site")
summary(BD_herbivory_1MO_clean)

# --- 3) Pull climate-level emmeans (assumes columns response, asymp.LCL, asymp.UCL) ---
climate_means <- glm_herbClimate_emmeans$emmeans %>%
  as_tibble() %>%
  mutate(Climate = factor(Climate, levels = c("Temperate","Tropical")))

# --- 4) Define contiguous segments per climate so Temperate has a GAP over 3–5 ---
segments_spec <- tribble(
  ~Climate,     ~sites,                 ~seg_id,
  "Temperate",  c("LAK","TOM"),         "temp_12",
  "Temperate",  c("EUC"),               "temp_6",
  "Tropical",   c("BUB","KAK","DRO"),   "trop_345"
)

# Build a rectangular CI and a horizontal mean line for each segment
segments_df <- segments_spec %>%
  rowwise() %>%
  mutate(
    # numeric x-range of this segment (with padding)
    xs = list(sort(site_positions$x_pos[match(sites, site_positions$Site)])),
    xmin = min(xs) - pad,
    xmax = max(xs) + pad
  ) %>%
  ungroup() %>%
  # attach the corresponding emmean row for each climate
  left_join(climate_means %>% select(Climate, response, asymp.LCL, asymp.UCL),
            by = "Climate") %>%
  mutate(
    y     = response * 100,
    ymin  = asymp.LCL * 100,
    ymax  = asymp.UCL * 100
  )

# Points positions (one per climate, centered over *all* its sites)
climate_points <- tibble(
  Climate   = factor(c("Temperate","Tropical"), levels = c("Temperate","Tropical")),
  x_center  = c(
    mean(site_positions$x_pos[site_positions$Site %in% c("LAK","TOM","EUC")]),
    mean(site_positions$x_pos[site_positions$Site %in% c("BUB","KAK","DRO")])
  )
) %>%
  left_join(climate_means %>% select(Climate, response), by = "Climate") %>%
  mutate(y = response * 100)

# --- 5) Colours ---
custom_site_cols <- c(
  "LAK" = "#084594",  "TOM" = "#3182bd",  "EUC" = "#6baed6",
  "BUB" = "#b10026",  "KAK" = "#e31a1c",  "DRO" = "#fb6a4a"
)
custom_climate_cols <- c(
  "Temperate" = "#084594",
  "Tropical"  = "#b10026"
)

# --- 6) Plot ---
ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_1MO_clean,
    aes(x = x_pos, y = TwigHerbProp*100, colour = Site),
    alpha = 0.2, position = position_jitter(width = 0.1),
    show.legend = FALSE
  ) +
  
  ylim(0, 40) +
  
  # CI ribbons as rectangles per segment (robust to grouping)
  geom_rect(
    data = segments_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Climate),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  
  # Mean lines as horizontal segments per segment (creates the gap)
  geom_segment(
    data = segments_df,
    aes(x = xmin, xend = xmax, y = y, yend = y, colour = Climate),
    linewidth = 1.5
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
                        "one month into the growing season (%)"))
  ) +
  theme(
    text = element_text(size = 18),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20,20,20,20)
  )

################ I FINISHED HERE #########################
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

BD_herbivory_1MO_clean2 <- BD_herbivory_1MO_clean2 %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))

custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
BD_herbivory_1MO_clean2$Site <- factor(BD_herbivory_1MO_clean2$Site, levels = custom_order)
BD_herbivory_1MO_clean2$Site <-as.factor(BD_herbivory_1MO_clean2$Site)           #
BD_herbivory_1MO_clean2$Patrol <-as.factor(BD_herbivory_1MO_clean2$Patrol)
BD_herbivory_1MO_clean2$SpeciesAnal <-as.factor(BD_herbivory_1MO_clean2$SpeciesAnal) 
BD_herbivory_1MO_clean2$TwigCodeUnique <-as.factor(BD_herbivory_1MO_clean2$TwigCodeUnique)     #  E.g. BUB_Baccaurea_ramiflora_1CN1_C_Blue - includes the nested design
BD_herbivory_1MO_clean2$SaplingCodeUnique <-as.factor(BD_herbivory_1MO_clean2$SaplingCodeUnique) #  E.g. BUB_Baccaurea_ramiflora_1CN1_C doesn't include info about the twig
BD_herbivory_1MO_clean2$Climate <-as.factor(BD_herbivory_1MO_clean2$Climate)    
summary(BD_herbivory_1MO_clean2)

glm.CHherb.null <-glmmTMB(StartEndDiff + 0.0001 ~ 1 +
                          + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean2,
                        na.action = "na.exclude")
glm.CHherb.site <-glmmTMB(StartEndDiff +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = BD_herbivory_1MO_clean2,
                        na.action = "na.exclude")
glm.CHherb.climate <-glmmTMB(StartEndDiff +0.0001 ~ Climate +
                             + (1|SpeciesAnal) + (1|SaplingCodeUnique) + (1|Site:Patrol), family = beta_family(),
                           data = BD_herbivory_1MO_clean2,
                           na.action = "na.exclude")

# compare the models
AICctab(glm.CHherb.null, glm.CHherb.site, glm.CHherb.climate)

# I am enforcing the SITE model, because this is what I want to see 
# below are correct figures for null models
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

############# NOT ADJUSTED TO THE NEW RESULTS YET
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

AC_herbivory_1MO_clean <- AC_herbivory_1MO_clean %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))
AC_herbivory_1MO_clean$Climate <-as.factor(AC_herbivory_1MO_clean$Climate) 
summary(AC_herbivory_1MO_clean)

glm.ACherb.null <-glmmTMB(TwigHerbProp +0.0001 ~ 1 +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                        data = AC_herbivory_1MO_clean)
glm.ACherb.site <-glmmTMB(TwigHerbProp +0.0001 ~ Site +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                          data = AC_herbivory_1MO_clean)
glm.ACherb.climate <-glmmTMB(TwigHerbProp +0.0001 ~ Climate  +
                            + (1|SpeciesAnal) + (1|SaplingCodeUnique)+ (1|Site:Patrol), family = beta_family(),
                          data = AC_herbivory_1MO_clean)

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
