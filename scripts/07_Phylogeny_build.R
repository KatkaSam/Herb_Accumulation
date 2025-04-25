#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#        Building of the phylogeny for the analyses
#
#----------------------------------------------------------#

install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("jinyizju/V.PhyloMaker2")

# Load the V.PhyloMaker2 package
library(V.PhyloMaker2)

# Define the vector of plant taxa you are interested in
plant_taxa <- c("Acacia parramattensis", "Acer mono", "Acer palmatum", 
                "Acer platanoides", "Acer pseudoplatanus", 
                "Argyrodendron peralatum", "Baccaurea ramiflora", 
                "Betula maximowicziana", "Breynia oblingifolia", 
                "Bursaria spinosa", "Carpinus betulus", "Carpinus cordata", 
                "Celtis latifolia", "Chrysophyllum roxburghii", 
                "Cleidion brevipetiolatum", "Cleistanthus myrianthus", 
                "Cryptocarya sp.", "Diospyros kaki", 
                "Dysoxylum arborescens", "Dysoxylum sp.", 
                "Endiandra leptodendron", "Erythrospermum candidum", 
                "Eucalyptus pruinosa", "Eucalyptus tereticornis", 
                "Ficus erythrosperma", "Fraxinus excelsior", 
                "Fraxinus lanuginosa", "Garcinia cowa", 
                "Gymnacranthera paniculata", "Haplostichanthus ramiflorus", 
                "Magnolia kobus", "Myristica globosa", "Orophea laui", 
                "Pimelodendron amboinicum", "Pittosporopsis kerrii", 
                "Pometia pinnata", "Prunus ssiori", "Quercus robur", 
                "Rockinghamia angustifolia", "Saprosma ternata", 
                "Shorea wangtianshuea", "Syringa reticulata", 
                "Tilia cordata", "Ulmus glabra")

# Create the phylogenetic tree
# 'method' can be "topology", "data", or "both". You can choose based on your needs.
phylo_tree <- V.PhyloMaker(plant_taxa, method = "topology")

# Plot the phylogenetic tree
plot(phylo_tree, main = "Phylogenetic Tree of Selected Plant Taxa")

# Save the tree to a file (optional)
# Specify the file path and format (e.g., "tree.nwk" for Newick format)
write.tree(phylo_tree, file = "phylogenetic_tree.nwk")


