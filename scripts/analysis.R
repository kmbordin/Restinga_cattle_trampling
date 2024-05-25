# packages ----
library(tidyverse)
library(here)
library(vegan)
library(reshape2)
library(doBy)
library(lme4)
library(lmerTest)
library(ggrepel)

# data loading and corrections ----
data1 <- read.csv2(here("data", "dados_campo_kmb.csv"))
data2 <- read.csv2(here("data", "dados_campo2_kmb.csv"))

data1 <- data1 %>% 
  mutate(year = as.character(year),
         disturbance = as.character(disturbance)) %>% 
  unite(sampling, c(site,year), sep = "_", remove = FALSE) %>% 
  mutate(mean.temperature = as.numeric(mean.temperature), 
         mean.humidity = as.numeric(mean.humidity))

data2 <- data2 %>% 
  mutate(year = as.character("Ano1"),
         disturbance = as.character(disturbance)) %>% 
  unite(sampling, c(site,year), sep = "_", remove = FALSE) %>% 
  mutate(understory_height = as.numeric(understory_height),
         canopy_cover = as.numeric(canopy_cover))

# response variables -----
community <- reshape2::dcast(data1,  sampling~species, value.var="n.ind",fun.agg = sum)
community <- community %>% remove_rownames %>% column_to_rownames(var="sampling")

S <- specnumber(community)
raremax <- min(S)

diversities <- data.frame(invsimp.Q2 = diversity(community, "inv"),
                          density.abund = rowSums(community), 
                          rarefy = rarefy(community,raremax)) 
diversities <- diversities%>% 
  mutate(sampling = rownames(diversities))

# predictor variables ----
temp.mean <- data.frame(summary_by(data1, mean.temperature ~ sampling,na.rm=TRUE, FUN=mean, keep.names = TRUE))

humi.mean <- data.frame(summary_by(data1, mean.humidity ~ sampling,na.rm=TRUE, FUN=mean, keep.names = TRUE))

canopy.cover <- data.frame(summary_by(data2, canopy_cover ~ sampling,na.rm=TRUE, FUN=mean, keep.names = TRUE))

under.height <- data.frame(summary_by(data2, understory_height ~ sampling,na.rm=TRUE, FUN=mean, keep.names = TRUE))

climatic <- merge(temp.mean, humi.mean, by.x = "sampling", by.y = "sampling")
structural <- merge(canopy.cover, under.height, by.x = "sampling", by.y = "sampling")

dist <- data1 %>% 
  select(sampling, disturbance, site)
dist <- unique(dist)

climatic <- climatic %>% 
  separate(sampling, c("site", "year"), remove = FALSE)
structural <- structural %>% 
  separate(sampling, c("site", "year"), remove = FALSE)

preds <- merge(climatic, structural, by.x = "site", by.y = "site")

preds <- preds %>% 
  rename(sampling = sampling.x,
         year.clima = year.x,
         info.str = sampling.y, 
         year.str = year.y) 
preds <- preds%>% 
  relocate(sampling, .before = site) %>%  
  relocate(site, .before= sampling) %>% 
  arrange(site)
dist <- dist %>% 
  select(site, disturbance) %>% 
  arrange(site)
preds <- bind_cols(preds, dist)

# nmds ----
nmds <- metaMDS(community, distance = "bray", autotransform = FALSE)
nmds
nmds.scores <- as.data.frame(nmds$points)
nmds.scores <- nmds.scores %>% 
  mutate(site = preds$site...1, 
         disturbance = preds$disturbance)
teste <- nmds.scores %>%
  group_by(disturbance) %>%
  slice(chull(MDS1,MDS2))

vec.sp <- envfit(nmds$points, correl, perm=1000)
vec.sp.df<- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- vec.sp.df %>% 
  mutate(species = replace(species, species == "mean.temperature", "Mean temperature")) %>%
  mutate(species = replace(species, species == "mean.humidity", "Mean humidity")) %>%
  mutate(species = replace(species, species == "canopy_cover", "Canopy cover")) %>%
  mutate(species = replace(species, species == "understory_height", "Understory height")) %>% 
  slice(-1)
  

ggplot(nmds.scores, mapping = aes(x = MDS1, y = MDS2, colour = disturbance, fill= disturbance, size=I(1))) + geom_point()+
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted") +
  scale_color_manual(values=c("#D55E00","#CC79A7","#0072B2"))+
  scale_fill_manual(values=c("#D55E00","#CC79A7","#0072B2"))+
  theme_light()+theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = c(1,1),
                      panel.background = element_blank())+
  geom_polygon(data = teste, alpha = 0.2)+
  geom_segment(data=vec.sp.df,mapping =  aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.2, "cm")),inherit.aes = FALSE, colour="black", linewidth = 0.5) + 
  geom_text_repel(data=vec.sp.df,mapping =  aes(x=MDS1,y=MDS2,label=species),size=3, inherit.aes = FALSE)

# merging all variables ----
data <- merge(preds, diversities, by.x = "sampling", by.y = "sampling")

data <- bind_cols(data, nmds$points) %>% 
  rename(site = site...2) 
  
correl <- data %>% 
  select(mean.temperature,mean.humidity,canopy_cover,understory_height)
cor(correl)

# models -----
rr <- lmer(rarefy ~ mean.temperature + mean.humidity + canopy_cover + understory_height + (1|year.clima), data = data)
summary(rr)
plot(rr)
performance::check_predictions(rr)
performance::check_residuals(rr)
performance::model_performance(rr)

simps <- lmer(invsimp.Q2 ~ mean.temperature + mean.humidity + canopy_cover + understory_height + (1|year.clima), data = data)
summary(simps)
plot(simps)
performance::check_predictions(simps)
performance::check_residuals(simps)
performance::model_performance(simps)

abund <- glmer.nb(density.abund ~ mean.temperature + mean.humidity + canopy_cover + understory_height + (1|year.clima), family = "poisson",data = data)
summary(abund)
plot(abund)
performance::check_predictions(abund)
performance::check_residuals(abund)
performance::model_performance(abund)

composicao <- lmer(MDS1 ~ mean.temperature + mean.humidity + canopy_cover + understory_height + (1|year.clima), data=data)
summary(composicao)
plot(composicao)
performance::check_predictions(composicao)
performance::check_residuals(composicao)
performance::model_performance(composicao)

composicao2 <- lmer(MDS2 ~ mean.temperature + mean.humidity + canopy_cover + understory_height + (1|year.clima), data=data)
summary(composicao2)
plot(composicao2)
performance::check_predictions(composicao2)
performance::check_residuals(composicao2)
performance::model_performance(composicao2)
