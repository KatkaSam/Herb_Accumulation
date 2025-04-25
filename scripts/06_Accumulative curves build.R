#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#         Just graphical accumulation of herbivory
#
#----------------------------------------------------------#
# Generates Figure 4 in the main text

# Example of the dataset based on the provided table
herbivory_data <- data.frame(
  X_value = c(0, 0.1, 1.00, 4.00),   # The X-axis values
  LAK = c(0.07, 0.06, 3.69, 5.46),       # Data for LAK
  TOM = c(0.01, 0.34, 5.58, 7.73),       # Data for TOM
  BUB = c(1.46, 1.19, 5.74, 6.86),       # Data for BUB
  KAK = c(4.09, 4.53, 6.09, 7.46),       # Data for KAK
  DRO = c(4.46, 5.62, 7.09, 7.48),       # Data for DRO
  EUC = c(1.77, 2.98, 5.26, 7.28)        # Data for EUC
)

# Reshape the data to long format for ggplot
library(tidyr)
herbivory_data_long <- herbivory_data %>%
  pivot_longer(cols = -X_value, names_to = "Species", values_to = "Herbivory")

# Load ggplot2
library(ggplot2)

# Incorporating site-specific symbols and colors into the graph

site_symbols <- c(LAK = 17, TOM = 16, BUB = 15, KAK = 17, DRO = 16, EUC = 15)
site_colors  <- c(LAK =  "blue", TOM = "turquoise" , BUB =  "yellowgreen" , KAK = "orange", DRO = "red" , EUC =  "purple")

p1 <- ggplot(herbivory_data_long, aes(x = X_value, y = Herbivory, color = Species, group = Species)) +
  
  # Adding the lines for each species
  geom_line(size = 1.5, aes(color = Species)) +            
  
  # Adding points with the corresponding site symbols
  geom_point(aes(shape = Species, color = Species), size = 4) + 
  
  # Mapping site-specific symbols and colors
  scale_shape_manual(values = site_symbols) + 
  scale_color_manual(values = site_colors) + 
  
  # Custom X-axis breaks
  scale_x_continuous(breaks = c(0, 0.1, 1.00, 4.00)) + 
  
  # Axis labels and title
  labs(
    x = "Month 
    Since the begining of experiment", 
    y = "Mean herbivory damage (%)", 
    title = "Herbivory accumulation at study sites"
  ) +
  
  # Customized theme
  theme_minimal() +
  
  # Adjusting text sizes and margins
  theme(
    text = element_text(size = 18),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # Margin for x-axis title
    axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Margin for y-axis title
    axis.text = element_text(size = 14),                              # Tick labels
    legend.text = element_text(size = 16),                            # Legend text size
    legend.title = element_text(size = 16),                           # Legend title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Center title
    plot.margin = margin(20, 20, 20, 20)                              # Adjust plot margins
  ) +
  
  # Customizing axis ticks and lines
  theme(
    axis.line = element_line(size = 1, color = "black"),      # Black and bold axis lines
    axis.ticks = element_line(size = 1, color = "black"),     # Black tick marks
    axis.ticks.length = unit(0.25, "cm"),                    # Tick mark length
    panel.grid.major = element_line(color = "grey80"),        # Light grid lines
    panel.grid.minor = element_blank()                        # Remove minor grid lines
  ) +
  
  theme(legend.position = "top")  # Position legend at the top

p1
