## Herb_Accumulation

This repository contains an RStudio project for analyzing data related to herbivore accumulatioon (i.e., change in herbivory) and standing herbivory. 
It contains data on chewing insect herbivory in understory woody plants across six sites spanning 51°N to 33°S, covering temperate and tropical regions: 
Leipzig in Germany, Tomakomai in Japan, Bubeng in China, Kakoba in Papua New Guinea, and Cairns and Sydney in Australia. It focuses on surveyed standing herbivory 
and herbivory rates over one- and four-month periods on 44 tree species in forest understory. 
The manuscript is currently in review, with core session data and history preserved for reproducibility.

### Project Structure
Data: Contains 14 data sheets and 5 sheets with metadata, with raw as well as summarized data from the 1 month and 4 months long experiment, during which accumulation of the herbivory was surveyed.
Figures: Includes Figures ready for the manuscript - for the main body as well as for the Supplements
Scripts: Includes 11 items of the R files with all scripts used in the manuscript
Herb_Accumulation: The RStudio project file to open the analysis environment.

____List of the scripts in the deposit____
* 01_1MO_data_organizing_20251213.R – Serves to organize the raw data of 1MO experiment and prepare the packages and sets the visuals. The script uses: HerbivoryLeavesRaw_1MO_6sites_20241010.csv and generates file HerbivoryTwigSum_1MO_6sites_20251010.csv and HerbivorySum_1MO_6sites_20252512_wide.csv. No figure is generated here. 
* 02_herbivory_1MO_20251213.R – Compares models considering individual factors explaining the variability in herbivory from 1MO experiment, separately for start of the experiment, end of the experiment and the change in the herbivory over the duration of the experiment. The script uses HerbivorySum_1MO_6sites_20241023_wideTolong_v2.csv. Figure 2 from the main text is generated.
 * 03_Sensitivity_analysis_BUB_1PatrolOnly_20251213.R – Follows the script 02_herbivory_1MO_20251213.R and tests the same things as above but removes the BUB site from the data. Same data needs to be loaded as above.  No figure is generated here.
 * 04_4MO_data_organizing_20241023.R – Serves to organize the raw data of 4MO experiment and prepare the packages and sets the visuals. The script uses: HerbivoryLeavesRaw_4MO_6sites_20241010.csv and generates file HerbivoryTwigSum_4MO_6sites_20241023a.csv and HerbivorySum_4MO_6sites_20241023_wideTolong.csv. No figure is generated here.
* 05_herbivory_4MO_20251214.R - Compares models considering individual factors explaining the variability in herbivory from 1MO experiment, separately for start of the experiment, end of the experiment and the change in the herbivory over the duration of the experiment. The script uses HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv. Figure 3 from the main text is generated.
* 06_Figure_Species_1MO.R – This script generates Figure S3 (herbivory on individual tree species in 1MO)  in the Appendix 1. It uses data from HerbivorySum_1MO_6sites_20241011_wideTolong.csv
* 07_Figure_Species_4MO.R – This script generates Figure S4 (herbivory on individual tree species in 4MO) in the Appendix 1. It uses data from HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv
* 08_PhylogenyTree_drawing.R – This script is used to draw phylogeny tree, using 1MO_BABE_species.nwk and 1MO_BABE_species_site.csv. Figure S1 in Appendix 1 is generated here. 
* 09_AC_hebrivory_comparison.R – This script tests the potential difference at the beginning of the experiments 1MO and 4MO. It uses subset of the data from 1MO_4MO_AConly_forPairTest.csv. No figure is generated here.
* 10_Accumulative curves build.R – This script is used to draw Figure 4 in the main text. No data needs to be loaded; all are entered in the script.
* 11_Phylogeny_build.R -  This script is used to generate the phylogeny tree. It uses list of the species and generates phylogenetic_tree.nwk

____List of the data provided (with indication to their metadata)____
*	HerbivoryLeavesRaw_1MO_6sites_20241010.csv - Metadata_1
*	HerbivoryLeavesRaw_4MO_6sites_20241023.csv - Metadata_1
*	HerbivorySum_1MO_6sites_20241011_wideTolong.csv - Metadata_2
*	HerbivorySum_1MO_6sites_20241023_wideTolong_v2.csv - Metadata_2
*	HerbivorySum_4MO_6sites_20241023_wideTolong.csv - Metadata_2
*	HerbivorySum_4MO_6sites_20241023_wideTolong_fixed.csv	- Metadata_2
*	HerbivoryTwigSum_1MO_6sites_20241010.csv - Metadata_3
*	HerbivoryTwigSum_1MO_6sites_20241010a.csv - Metadata_3
*	HerbivoryTwigSum_1MO_6sites_20241010b.csv - Metadata_3
*	HerbivoryTwigSum_4MO_6sites_20241023a.csv	- Metadata_3
*	HerbivoryTwigSum_4MO_6sites_20241023b.csv - Metadata_3
*	1MO_BABE_species.nwk (for 08_PhylogenyTree_drawing.R) - No Metadata
*	1MO_BABE_species_site.csv (for 08_PhylogenyTree_drawing.R) - Metadata_4
*	1MO_4MO_AConly_forPairTest.csv (for 09_AC_herbivory_comparison.R) - Metadata_5


### TERMS OF USE
Copyright (c) [Katerina Sam], [2025]

This repository is provided for academic transparency and reproducibility.

The data included herein may not be used, redistributed, or published in any form without the explicit written permission of the author. Any use of the data that contributes substantially to a publication or presentation must include the author as a co-author.

Use of analysis code without data is permitted under the MIT License (or similar open license if you prefer), provided appropriate attribution is given.

Contact: [katerina.sam.cz@gmail.com]

All rights reserved.
