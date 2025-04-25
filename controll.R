

# # # #controls


install.packages("ggpubr")

#  libraries
library(readxl)
library(dplyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(bbmle)
#LAK Load the data
dataset_coleoptera_final<- read_xlsx("C:/Users/honza/OneDrive - Jihočeská univerzita v Českých Budějovicích/Dokumenty/PH.D/coleoptera manuscript/abundance_treatment/Coleoptera_final_WIDE.xlsx")

LAK_cn_model <- dataset_coleoptera_final %>%
  select(TreeCode, Code, Strata, Site, Treatment, Individual, PlantSpeciesAnal, Patrol,Tot.ab.f) %>%
  drop_na() %>%
  mutate_if(is.character, as.factor)

# Summary of the dataset
summary(LAK_cn_model)

# Subset the data - Lak je jedna site, LEIPZIG, Patrol je rok (2021 a 2020). Ta samá data mame jeste pro Tomakomai
LAK_cn <- filter(LAK_cn_model,
                 Treatment %in% c("CN2"),
                 Site == "LAK",
                 Patrol %in% c("D", "B"))

# Summary after subsetting
summary(LAK_cn)

summary(LAK_cn_model)
nlevels(LAK_cn_model$PlantSpeciesAnal)

# Scale the response variable
LAK_cn_model$Tot.ab.f<-log(LAK_cn_model$Tot.ab.f)
summary(LAK_cn_model$Tot.ab.f)



# Fit different linear mixed-effects models

#test different models #patrol is always in the models
lm_null<-lmer(Tot.ab.f~Patrol  +(1|PlantSpeciesAnal),LAK_cn_model,na.action = "na.fail", REML = F)
lm_full<-lmer(Tot.ab.f~Strata + Patrol  +(1 |PlantSpeciesAnal),LAK_cn_model ,na.action = "na.fail", REML = F)

AICctab(lm_null, lm_full)
# model selected
lm_full1<-lmer(Tot.ab.f~  Strata+Patrol   + (1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_cn_model,na.action = "na.fail", REML = F)

#compare between random effect
lm_full2<-lmer(Tot.ab.f~  Strata + Patrol + Treatment:Strata +(1 |PlantSpeciesAnal)+(1|Individual),LAK_cn_model,na.action = "na.fail", REML = F)




# Compare models based on AIC

aictab3<-AICctab(lm_full1, lm_full2, lm_null, lm_full)
#best lm_full1, stejne jako pro predchozi 
#test different models #patrol is always in the models

# Model selected
model_selected <- lm_full1
# Perform post-hoc tests
check_model(lm_full1)

#emmeans 
lm_select <-
  emmeans(
    model_selected,
    pairwise ~ Strata,
    type = "response", reverse=F)
# Generate interaction plots
p <- plot(lm_select)

# Save the plot
ggsave("interaction_plot.jpeg", p, width = 10, height = 8, dpi = 300)
p<-exp(-6.19)-exp(-6.65) #BIRD EMEAN-CONTROL EMEAN/CONTROL EMEAN
pp<-p/exp(-6.65)
pp*100
plot(lm_select)
lm_select$emmeans %>% 
  as_tibble() %>% 
  write_xlsx("C:/Users/honza/OneDrive - Jihočeská univerzita v Českých Budějovicích/Dokumenty/PH.D/coleoptera manuscript/result/emmeans_coleoptera2mo.csv")

library(rstatix)

LAK_2m_model <-LAK_2m_model  %>%
  mutate(Strata=factor(Strata)) %>%
  set_ref_level("Strata", ref="Understory") 

#Interaction plots
pc<- lm_select$emmeans %>%
  as_tibble() %>%
  ggplot(aes(
    x = Strata,
    y = exp(emmean) * 10000,
    col = Strata,
    fill = Strata)) + 
  geom_point(
    data = LAK_cn_model,
    aes(y = exp(Tot.ab.f) * 10000),
    alpha = 0.5,
    size = 1.5,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.2)) +
  geom_errorbar(
    aes(
      ymin = exp(lower.CL) * 10000,
      ymax = exp(upper.CL) * 10000),
    width = 0.4,
    position = position_dodge(width = 0.5, preserve = "single"),
    size = 1.5) +
  geom_point(
    shape = 21,  # Changed shape to 21 to match the p1 plot
    position = position_dodge(width = 0.5),
    size = 4) +
  labs(
    x = "Strata",
    y = "Coleoptera density (ind. per m²)") +
  scale_color_manual(values = c("Understory" = "darkgreen", "Canopy" = "#56B4E9")) +
  scale_fill_manual(values = c("Understory" = "darkgreen", "Canopy" = "#56B4E9")) +
  theme(
    text = element_text(size = 20),
    axis.text = element_text(color = "black"), 
    legend.position = "top") +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme_classic() +
  theme(legend.position = "top") +
  theme(axis.text = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  scale_y_continuous(n.breaks = 10, trans = 'log10')

pc
# Print the plot
print(pc)
ggsave(p1, file = "coleopteraCONTROLlak.jpeg", dpi = 300, width=11, height=8)




#TOM Load the data
dataset_coleoptera_final<- read_xlsx("C:/Users/honza/OneDrive - Jihočeská univerzita v Českých Budějovicích/Dokumenty/PH.D/coleoptera manuscript/abundance_treatment/Coleoptera_final_WIDE.xlsx")

TOM_cn_model <- dataset_coleoptera_final %>%
  select(TreeCode, Code, Strata, Site, Treatment, Individual, PlantSpeciesAnal, Patrol,Tot.ab.f) %>%
  drop_na() %>%
  mutate_if(is.character, as.factor)

# Summary of the dataset
summary(TOM_cn_model)

# Subset the data - TOM je jedna site, LEIPZIG, Patrol je rok (2021 a 2020). Ta samá data mame jeste pro Tomakomai
TOM_cn <- filter(TOM_cn_model,
                 Treatment %in% c("CN2"),
                 Site == "TOM",
                 Patrol %in% c("D", "B"))

# Summary after subsetting
summary(TOM_cn)

summary(TOM_cn)
nlevels(TOM_cn$PlantSpeciesAnal)

# Scale the response variable
TOM_cn$Tot.ab.f<-log(TOM_cn$Tot.ab.f)
summary(TOM_cn$Tot.ab.f)



# Fit different linear mixed-effects models

#test different models #patrol is always in the models
tom_null<-lmer(Tot.ab.f~Patrol  +(1|PlantSpeciesAnal),TOM_cn,na.action = "na.fail", REML = F)
tom_full<-lmer(Tot.ab.f~Strata + Patrol  +(1 |PlantSpeciesAnal),TOM_cn ,na.action = "na.fail", REML = F)


# model selected
tom_full1<-lmer(Tot.ab.f~  Strata+Patrol   + (1 + Strata|PlantSpeciesAnal)+(1|Individual),TOM_cn,na.action = "na.fail", REML = F)

#compare between random effect
tom_full2<-lmer(Tot.ab.f~  Strata + Patrol + Treatment:Strata +(1 |PlantSpeciesAnal)+(1|Individual),TOM_cn,na.action = "na.fail", REML = F)




# Compare models based on AIC

aictab3<-AICctab(tom_full1, tom_full2, tom_null, tom_full)
#best lm_full1, stejne jako pro predchozi 
#test different models #patrol is always in the models



# Model selected
tom_model_selected <- tom_full1
# Perform post-hoc tests
check_model(lm_full1)

#emmeans 
tom_lm_select <-
  emmeans(
    tom_model_selected,
    pairwise ~ Strata,
    type = "response", reverse=F)
# Generate interaction plots
tom_p <- plot(tom_lm_select)
tom_p
#Interaction plots
tc<- tom_lm_select$emmeans %>%
  as_tibble() %>%
  ggplot(aes(
    x = Strata,
    y = exp(emmean) * 10000,
    col = Strata,
    fill = Strata)) + 
  geom_point(
    data = TOM_cn,
    aes(y = exp(Tot.ab.f) ),
    alpha = 0.5,
    size = 1.5,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.2)) +
  geom_errorbar(
    aes(
      ymin = exp(lower.CL) * 10000,
      ymax = exp(upper.CL) * 10000),
    width = 0.4,
    position = position_dodge(width = 0.5, preserve = "single"),
    size = 1.5) +
  geom_point(
    shape = 21,  # Changed shape to 21 to match the p1 plot
    position = position_dodge(width = 0.5),
    size = 4) +
  labs(
    x = "Strata",
    y = "Coleoptera density (ind. per m²)") +
  scale_color_manual(values = c("Understory" = "darkgreen", "Canopy" = "#56B4E9")) +
  scale_fill_manual(values = c("Understory" = "darkgreen", "Canopy" = "#56B4E9")) +
  theme_classic() +
  theme(
    text = element_text(size = 20),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 16),
    axis.line = element_line(colour = "black", linewidth = 1.2, linetype = "solid"),
    axis.ticks = element_line(colour = "black", linewidth = 1.2, linetype = "solid"),
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)) +
  scale_y_continuous(n.breaks = 10, trans = 'log10')

# Print the plot
print(tc)
ggsave(p1, file = "coleopteraCONTROLTOM.jpeg", dpi = 300, width=11, height=8)



# # # # Leipzig predation
# Summary of the dataset
summary(dataset_coleoptera_final)

# Subset the data - Lak je jedna site, LEIPZIG, Patrol je rok (2021 a 2020). Ta samá data mame jeste pro Tomakomai
LAK_2m <- filter(dataset_coleoptera_final,
                 Treatment %in% c("ALL", "ANT", "CN2", "VER"),
                 Site == "LAK",
                 Patrol %in% c("D", "B"))

# Summary after subsetting
summary(LAK_2m)

# Prepare the data #treeCode je jeden řádek dat, je to treatment, druh, strata, individuum a sezona
LAK_2m_model <- LAK_2m %>%
  select(TreeCode, Code, Strata, Treatment, Individual, PlantSpeciesAnal, Patrol,Tot.ab.f) %>%
  drop_na() %>%
  mutate_if(is.character, as.factor)

summary(LAK_2m_model)
nlevels(LAK_2m_model$PlantSpeciesAnal)

# Scale the response variable
LAK_2m_model$Tot.ab.f<-log(LAK_2m_model$Tot.ab.f)
summary(LAK_2m_model$Tot.ab.f)



# Fit different linear mixed-effects models
lm_null <- lmer(Tot.ab.f ~ Patrol + (1 | PlantSpeciesAnal), LAK_2m_model, na.action = "na.fail", REML = F)
lm_full <- lmer(Tot.ab.f ~ Treatment + Patrol + (1 | PlantSpeciesAnal), LAK_2m_model, na.action = "na.fail", REML = F)
#full model selected
lm_full1<-lmer(Tot.ab.f~Treatment + Patrol + Strata + Treatment:Strata +(1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)

#compare between random effect
lm_full2<-lmer(Tot.ab.f~Treatment + Patrol + Strata + Treatment:Strata +(1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)




# Compare models based on AIC
aictab1<-AICctab(lm_null, lm_full)
aictab2<-AICctab(lm_full1, lm_full2)
aictab3<-AICctab(lm_full1, lm_full2, lm_full)
#best lm_full
#test different models #patrol is always in the models
lm_pat<-lmer(Tot.ab.f~Patrol  +(1|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)
lm_treatpat<-lmer(Tot.ab.f~Treatment + Patrol  +(1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)

lm_patrstr<-lmer(Tot.ab.f~Patrol + Strata  +(1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)
lm_patrstr2<-lmer(Tot.ab.f~Patrol + Strata  + (1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)


lm_nointer<-lmer(Tot.ab.f~Treatment + Patrol + Strata  +(1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)
lm_nointer2<-lmer(Tot.ab.f~Treatment + Patrol + Strata + (1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)

lm_full<-lmer(Tot.ab.f~Treatment + Patrol + Strata + Treatment:Strata +(1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)
lm_full2<-lmer(Tot.ab.f~Treatment + Patrol + Strata + Treatment:Strata +(1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)

#model pro porovnani canopy vs understory.  Patrol je sezona(rok), plant species JE DRUH STromu, individual je jedenjedinec stromu
lm_control<-lmer(Tot.ab.f~Strata + Patrol +(1|Treatment) +(1 + Strata|PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)
lm_control<-lmer(Tot.ab.f~Strata + Patrol +(1|Treatment) +(1 |PlantSpeciesAnal)+(1|Individual),LAK_2m_model,na.action = "na.fail", REML = F)


AICctab(lm_pat,lm_treatpat, lm_patrstr, lm_nointer, lm_full, lm_patrstr2, lm_nointer2, lm_full2)


# Model selected
model_selected <- lm_nointer
# Perform post-hoc tests
check_model(lm_nointer)
model_canopy <-lm_control

#emmeans 
lm_select <-
  emmeans(
    model_selected,
    pairwise ~ Treatment|Strata,
    type = "response", reverse=F)
# Generate interaction plots
p <- plot(lm_select)
#emmeanscontrol
lm_control <-
  emmeans(
    model_canopy,
    pairwise ~ Strata,
    type = "response", reverse=F)
# Save the plot
ggsave("interaction_plot.jpeg", p, width = 10, height = 8, dpi = 300)
p<-exp(-6.19)-exp(-6.65) #BIRD EMEAN-CONTROL EMEAN/CONTROL EMEAN
pp<-p/exp(-6.65)
pp*100
plot(lm_select)
lm_select$emmeans %>% 
  as_tibble() %>% 
  write_xlsx("C:/Users/honza/OneDrive - Jihočeská univerzita v Českých Budějovicích/Dokumenty/PH.D/coleoptera manuscript/result/emmeans_coleoptera2mo.csv")

library(rstatix)

LAK_2m_model <-LAK_2m_model  %>%
  mutate(Strata=factor(Strata)) %>%
  set_ref_level("Strata", ref="Understory") 

#Interaction plots
p1<-(model_plot_01 <- 
       lm_select$emmeans %>% 
       as_tibble() %>% 
       ggplot(
         aes(
           x=Treatment,
           y = exp(emmean)*10000,
           col = Strata,
           fill=Strata
         )) + 
       geom_point(
         data = LAK_2m_model,
         aes(y = exp(Tot.ab.f)*10000),
         alpha = 0.5,
         size = 1.5,
         position = position_jitterdodge(
           dodge.width = 0.5,
           jitter.width = 0.2)) +
       geom_errorbar(
         aes(
           ymin = exp(lower.CL)*10000,
           ymax = exp(upper.CL)*10000),
         width=0.4,
         position = position_dodge(width = 0.5, preserve = "single"),
         size = 1.5)+
       
       geom_point(
         shape = 21,
         position = position_dodge(width = 0.5),
         size = 4) +
       
       labs(
         x = "Treatment",
         y = "Coleoptera density (ind. per m²)")+
       scale_color_manual(values = c( "Understory"="darkgreen","Canopy"="#56B4E9"))+
       scale_fill_manual(values = c( "Understory"="darkgreen","Canopy"="#56B4E9"))+
       
       theme(
         text = element_text(size = 20),
         axis.text=element_text(color="black"), 
         legend.position = "top"))+
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))+ theme_classic()+theme(legend.position="top")+theme(axis.text=element_text(size=16))+
  scale_x_discrete(limits=c("CN2", "ANT", "VER", "ALL"))+theme(axis.title =element_text(size=16))+theme(legend.text = element_text(size=16))+theme(legend.title =element_text(size=16))+scale_y_continuous(n.breaks=10, trans='log10')
p1 
