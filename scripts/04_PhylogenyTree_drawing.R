#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#          Figure for individual species 4MO
#
#----------------------------------------------------------#
# Generates Figure S1 in Supplement

library(ape)
tree <- read.tree("C:/Users/ksam/Documents/GitHub/Herb_Accumulation/data/1MO_BABE_species.nwk")
tree

# Load the table
# CURRENTLY I HAVE NO IDEA WHERE THIS FILE IS
species_sites <- read.csv("C:/Users/ksam/Downloads/Phylogeny/1MO_BABE_species_site.csv")
species_sites

# Match species names between the tree and the table
species_sites$species <- trimws(species_sites$species)
tree$tip.label <- trimws(tree$tip.label)

# Create a vector to map species to their sites
species_order <- match(tree$tip.label, species_sites$species)
ordered_sites <- species_sites$site[species_order]  # Ensure the correct column name

# Assign symbols and colors for each site
site_symbols <- c(LAK = 17, TOM = 16, BUB = 15, KAK = 17, DRO = 16, EUC = 15)
site_colors  <- c(LAK =  "blue", TOM = "turquoise" , BUB =  "yellowgreen" , KAK = "orange", DRO = "red" , EUC =  "purple")

# Plot the tree
plot(tree, 
           show.tip.label = TRUE, 
           edge.color = "black", 
           type = "phylogram", 
           cex = 0.6, 
           label.offset = 5)

# Add symbols corresponding to study sites behind species names
for (i in 1:length(tree$tip.label)) {
  site <- ordered_sites[i]
  if (!is.na(site)) {  # Check for any missing data
    tiplabels(pch = site_symbols[site], 
              col = site_colors[site], 
              adj = 1.2, 
              cex = 1,  # Reduce the symbol size (adjust as needed)
              frame = "n", 
              tip = i)
  }
}

# Add a legend to the plot
legend("topright", legend = names(site_colors), 
       pch = site_symbols, col = site_colors, 
       title = "Study Sites", 
       bty = "n",  # No box around the legend
       pt.cex = 1.5,  # Size of the points
       cex = 0.8)  # Text size
