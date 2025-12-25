library(tidyverse)
library(emmeans)

# --- 1. Define site positions (sorted by latitude) ---
site_positions <- tibble(
  Site = c("LAK", "TOM", "EUC", "BUB", "KAK", "DRO"),
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
  "LAK" = "#084594",  "TOM" = "#3182bd",  "EUC" = "#6baed6",
  "BUB" = "#b10026",  "KAK" = "#e31a1c",  "DRO" = "#fb6a4a"
)
custom_climate_cols <- c(
  "Temperate" = "#084594",
  "Tropical"  = "#b10026"
)

# --- 6. Plot ---
ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_1MO_clean,
    aes(x = x_pos, y = TwigHerbProp*100, colour = Site),
    alpha = 0.2, position = position_jitter(width = 0.1),
    show.legend = FALSE
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
  
  # Mean points
  geom_point(
    data = climate_means,
    aes(x = x_pos_line, y = response*100, colour = Climate),
    size = 4
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





# --- 1. Define site positions (new order) ---
site_positions <- tibble(
  Site = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC"),
  x_pos = 1:6
)

# --- 2. Clean raw data and add x_pos & Climate ---
BD_herbivory_1MO_clean <- BD_herbivory_1MO_clean %>%
  select(-matches("x_pos\\.")) %>%  # drop any old x_pos
  mutate(
    Climate = if_else(Site %in% c("LAK","TOM","EUC"),
                      "Temperate", "Tropical")
  ) %>%
  left_join(site_positions, by = "Site")
summary(BD_herbivory_1MO_clean)

# --- 3. Prepare climate means with site groups ---
climate_means <- glm_herbClimate_emmeans$emmeans %>%
  as_tibble() %>%
  mutate(Climate = factor(Climate, levels = c("Temperate","Tropical"))) %>%
  rowwise() %>%
  mutate(
    site_group = list(
      if (Climate == "Temperate") c("LAK","TOM","EUC") else c("BUB","KAK","DRO")
    ),
    x_vals = list(site_positions$x_pos[site_positions$Site %in% site_group])
  ) %>%
  ungroup()

# --- 4. Build ribbons (separate per contiguous cluster) ---
make_ribbon <- function(df, site_subset) {
  x_sub <- sort(site_positions$x_pos[site_positions$Site %in% site_subset])
  tibble(
    Climate = unique(df$Climate),
    x = c(min(x_sub) - 0.3, max(x_sub) + 0.3),
    ymin = rep(df$asymp.LCL*100, 2),
    ymax = rep(df$asymp.UCL*100, 2)
  )
}

make_line <- function(df, site_subset) {
  x_sub <- sort(site_positions$x_pos[site_positions$Site %in% site_subset])
  tibble(
    Climate = unique(df$Climate),
    x = c(min(x_sub) - 0.3, max(x_sub) + 0.3),
    y = rep(df$response*100, 2)
  )
}

climate_ribbon <- bind_rows(
  # Temperate = two disjoint groups: 1–2 and 6
  make_ribbon(climate_means %>% filter(Climate=="Temperate"), c("LAK","TOM")),
  make_ribbon(climate_means %>% filter(Climate=="Temperate"), c("EUC")),
  # Tropical = 3–5
  make_ribbon(climate_means %>% filter(Climate=="Tropical"), c("BUB","KAK","DRO"))
)

climate_lines <- bind_rows(
  make_line(climate_means %>% filter(Climate=="Temperate"), c("LAK","TOM")),
  make_line(climate_means %>% filter(Climate=="Temperate"), c("EUC")),
  make_line(climate_means %>% filter(Climate=="Tropical"), c("BUB","KAK","DRO"))
)

climate_points <- climate_means %>%
  rowwise() %>%
  mutate(
    x_center = mean(site_positions$x_pos[site_positions$Site %in% site_group])
  ) %>%
  ungroup()

# --- 5. Define colours ---
custom_site_cols <- c(
  "LAK" = "#084594",  "TOM" = "#3182bd",  "EUC" = "#6baed6",
  "BUB" = "#b10026",  "KAK" = "#e31a1c",  "DRO" = "#fb6a4a"
)
custom_climate_cols <- c(
  "Temperate" = "#084594",
  "Tropical"  = "#b10026"
)

# --- 6. Plot ---
ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_1MO_clean,
    aes(x = x_pos, y = TwigHerbProp*100, colour = Site),
    alpha = 0.2, position = position_jitter(width = 0.1),
    show.legend = FALSE
  ) +
  
  # CI ribbons
  geom_ribbon(
    data = climate_ribbon,
    aes(x = x, ymin = ymin, ymax = ymax, fill = Climate, group = interaction(Climate, x)),
    alpha = 0.2
  ) +
  
  # Mean lines
  geom_line(
    data = climate_lines,
    aes(x = x, y = y, colour = Climate, group = interaction(Climate, x)),
    size = 1.5
  ) +
  
  # Mean points
  geom_point(
    data = climate_points,
    aes(x = x_center, y = response*100, colour = Climate),
    size = 4
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



--- 1. Define site positions (sorted by latitude) ---
  site_positions <- tibble(
    Site = c("LAK", "TOM", "EUC", "BUB", "KAK", "DRO"),
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
  "LAK" = "#084594",  "TOM" = "#3182bd",  "EUC" = "#6baed6",
  "BUB" = "#b10026",  "KAK" = "#e31a1c",  "DRO" = "#fb6a4a"
)
custom_climate_cols <- c(
  "Temperate" = "#084594",
  "Tropical"  = "#b10026"
)

# --- 6. Plot ---
ggplot() +
  # Raw site points
  geom_point(
    data = BD_herbivory_1MO_clean,
    aes(x = x_pos, y = TwigHerbProp*100, colour = Site),
    alpha = 0.2, position = position_jitter(width = 0.1),
    show.legend = FALSE
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
  
  # Mean points
  geom_point(
    data = climate_means,
    aes(x = x_pos_line, y = response*100, colour = Climate),
    size = 4
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

