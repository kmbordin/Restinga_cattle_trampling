# packages ----
library(tidyverse)
library(here)
library(vegan)
library(reshape2)
library(doBy)
library(lme4)
library(lmerTest)
library(ggrepel)

# species list dataset ----
load("data/species.list.RData")
#write.table(spp, file = "data/species.list.txt")
# load variables -----
load("data/model.vars.RData")
#write.table(table, file="data/variables.txt")

# data analysis -----

#########################################################################################
# QUESTION 1:Is there a segregation in butterfly species composition among these habitats, 
# including identifying indicator species for each habitat?
#########################################################################################
# environmental and structure on spp composition ----
envs <- table %>% 
  select(mean_temperature,mean_humidity,canopy_cover,understory_height)
nmds <- table %>% 
  select(MDS1,MDS2)
vec.sp <- envfit(nmds, envs, perm=1000)

vec.sp.df<- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- vec.sp.df %>% 
  mutate(species = replace(species, species == "mean_temperature", "Mean temperature")) %>%
  mutate(species = replace(species, species == "mean_humidity", "Mean humidity")) %>%
  mutate(species = replace(species, species == "canopy_cover", "Canopy cover")) %>%
  mutate(species = replace(species, species == "understory_height", "Understory height")) %>% 
  slice(-1)

hull = table %>%
  group_by(disturbance) %>%
  slice(chull(MDS1,MDS2))

# plot nmds ----
#png('results/NMDS.png', units="in", width=5, height=5, res=300)
ggplot(table, mapping = aes(x = MDS1, y = MDS2, colour = disturbance, fill= disturbance, size=I(1))) + 
  geom_point()+
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted") +
  scale_color_manual(values=c("#D55E00","#CC79A7","#0072B2"))+
  scale_fill_manual(values=c("#D55E00","#CC79A7","#0072B2"))+
  theme_light()+theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "bottom",
                      panel.background = element_blank())+
  geom_polygon(data = hull, alpha = 0.2)+
  geom_segment(data=vec.sp.df,mapping =  aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.2, "cm")),inherit.aes = FALSE, colour="black", linewidth = 0.5) + 
  geom_text_repel(data=vec.sp.df,mapping =  aes(x=MDS1,y=MDS2,label=species),size=3, inherit.aes = FALSE)
#dev.off()
#
#
#########################################################################################
# QUESTION 3: How do environmental variables and vegetation structure of Restinga forests 
# affect the richness, abundance, and dominance of fruit-feeding butterfly assemblages 
# due to indirect disturbances caused by cattle presence? 
#########################################################################################
# model for rarefied richness ----
rr <- lmer(rarefied_rich ~ mean_temperature + mean_humidity + canopy_cover + understory_height + (1|random), data = table)
summary(rr)
performance::check_predictions(rr)
performance::check_residuals(rr)
performance::model_performance(rr)
rr.res <- broom.mixed::tidy(rr, effects="fixed")%>% 
  mutate(factor = "rr")

# model for dominance ----
simps <- lmer(dominance ~ mean_temperature + mean_humidity + canopy_cover + understory_height + (1|random), data = table)
summary(simps)
performance::check_predictions(simps)
performance::check_residuals(simps)
performance::model_performance(simps)
simps.res <- broom.mixed::tidy(simps, effects="fixed")%>% 
  mutate(factor = "simps")

# model for abundance ----
abund <- glmer.nb(abundance ~ mean_temperature + mean_humidity + canopy_cover + understory_height + (1|random), family = "poisson",data = table)
summary(abund)
performance::check_predictions(abund)
performance::check_residuals(abund)
performance::model_performance(abund)
abund.res <- broom.mixed::tidy(abund, effects="fixed") %>% 
  mutate(factor = "abund")

# all linear model's results ----
results <- bind_rows(rr.res,simps.res,abund.res) %>% 
  mutate(across(where(is.numeric), round, 4))
results
#write.table(results, "results/model.results.txt")
#
#########################################################################################
# QUESTION 2: Are there differences in butterfly diversity parameters among Restinga forests 
# with low levels of cattle trampling compared with medium and high levels of disturbance?
#########################################################################################
# SEE MAIN MANUSCRIPT FILE - iNEXT online

##########################################################################################
# Supplementary info 1: correlation among variables 
##########################################################################################
correl <- table %>% 
  select(mean_temperature,mean_humidity,canopy_cover,understory_height) %>% 
  rename(`Mean temperature`= mean_temperature,
         `Mean humidity` = mean_humidity, 
         `Canopy cover`= canopy_cover,
         `Understory height`= understory_height)
correl <- cor(correl)
#png('results/correlation.png', units="in", width=4, height=4, res=300)
corrplot::corrplot(correl, method = 'number', col = 'black')
#dev.off()
