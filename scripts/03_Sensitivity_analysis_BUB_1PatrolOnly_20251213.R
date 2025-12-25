#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  12Dec2025
#
#                   Analyses from 1MO
#
#----------------------------------------------------------#

glm.CHherb.null_noPatrol <- glmmTMB(
  StartEndDiff + 0.0001 ~ 1 +
    (1|SpeciesAnal) + (1|SaplingCodeUnique),
  family = beta_family(),
  data = BD_herbivory_1MO_clean2
)

glm.CHherb.site_noPatrol <- glmmTMB(
  StartEndDiff + 0.0001 ~ Site +
    (1|SpeciesAnal) + (1|SaplingCodeUnique),
  family = beta_family(),
  data = BD_herbivory_1MO_clean2
)

glm.CHherb.climate_noPatrol <- glmmTMB(
  StartEndDiff + 0.0001 ~ Climate +
    (1|SpeciesAnal) + (1|SaplingCodeUnique),
  family = beta_family(),
  data = BD_herbivory_1MO_clean2
)

AICctab(glm.CHherb.null_noPatrol, glm.CHherb.site_noPatrol, glm.CHherb.climate_noPatrol)


################# SECOND APPROACH #####################
# --- 1. Remove BUB ---
BD_noBUB <- BD_herbivory_1MO_clean2 %>%
  dplyr::filter(Site != "BUB")
summary(BD_noBUB)

site_climate_noBUB <- tibble::tibble(
  Site = c("LAK","TOM","EUC","DRO","KAK"),
  Climate = c("Temperate","Temperate","Temperate","Tropical","Tropical")
)

BD_noBUB <- BD_noBUB %>%
  dplyr::left_join(site_climate_noBUB, by = "Site")

table(BD_noBUB$Climate, useNA = "ifany")

# --- 2. Fit null model ---
glm.CHherb.null_noBUB <- glmmTMB(
  StartEndDiff + 0.0001 ~ 1 +
    (1 | SpeciesAnal) +
    (1 | SaplingCodeUnique) +
    (1 | Site:Patrol),       # nested patrol within site
  family = beta_family(),
  data = BD_noBUB,
  na.action = "na.exclude"
)

# --- 3. Fit site model ---
glm.CHherb.site_noBUB <- glmmTMB(
  StartEndDiff + 0.0001 ~ Site +
    (1 | SpeciesAnal) +
    (1 | SaplingCodeUnique) +
    (1 | Site:Patrol),
  family = beta_family(),
  data = BD_noBUB,
  na.action = "na.exclude"
)

# --- 4. Fit climate model ---
# Make sure you have a Climate column; if not, add it
site_climate <- tibble::tibble(
  Site = c("LAK","TOM","EUC","DRO","KAK"),
  Climate = c("Temperate","Temperate","Temperate","Tropical","Tropical")
)

BD_noBUB <- BD_noBUB %>%
  dplyr::left_join(site_climate, by = "Site")

glm.CHherb.climate_noBUB <- glmmTMB(
  StartEndDiff + 0.0001 ~ Climate +
    (1 | SpeciesAnal) +
    (1 | SaplingCodeUnique) +
    (1 | Site:Patrol),
  family = beta_family(),
  data = BD_noBUB,
  na.action = "na.exclude"
)

# --- 5. Compare models with AICc ---
library(AICcmodavg)
AICctab(glm.CHherb.null_noBUB, glm.CHherb.site_noBUB, glm.CHherb.climate_noBUB)


# We tested whether one-month changes in herbivory damage were explained by Site or Climate 
# using beta GLMMs with random intercepts for species identity, sapling identity, and patrol (nested within site). 
# In the full dataset, model comparison using AICc indicated that the null model was best supported, 
# with little evidence for effects of Site or Climate. 
# To assess whether the single patrol at BUB influenced these results, we conducted sensitivity analyses: 
# (i) removing the Patrol random effect, and 
# (ii) removing BUB entirely while keeping Patrol as a random effect. 
# When Patrol was removed, the Site model was strongly supported over the null, whereas Climate remained unsupported. 
# When BUB was removed, both Site and Climate models were slightly more plausible than the null, 
# but ΔAICc values remained small (<2), indicating that the fixed effects were still weak predictors. 
# Overall, these results suggest that most variation in short-term herbivory changes is explained by species 
# and individual sapling differences, rather than by broad-scale site or climate categories.