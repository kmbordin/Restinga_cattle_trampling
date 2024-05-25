# packages ----
library(tidyverse)
library(here)
library(vegan)
library(reshape2)
library(doBy)
library(lme4)
library(lmerTest)

# data loading and corrections ----
data1 <- read.csv2(here("data", "dados_campo_kmb.csv"))
data2 <- read.csv2(here("data", "dados_campo2_kmb.csv"))

data1 <- data1 %>% 
  mutate(year = as.character(year),
         disturbance = as.character(disturbance)) %>% 
  unite(sampling, c(site,year), sep = "_", remove = FALSE) %>% 
  mutate(mean.temperature = as.numeric(mean.temperature), 
         mean.humidity = as.numeric(mean.humidity))

data2 <-data2 %>% 
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
  relocate(site, .before= sampling)

# merging all variables ----
data <- merge(preds, diversities, by.x = "sampling", by.y = "sampling")
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
