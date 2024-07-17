preds %>% 
  filter(disturbance=="ausente") %>% 
  summarise(umi = mean(mean.humidity),
            temp=mean(mean.temperature),
            temp.sd = sd(mean.temperature),
            umi.sd = sd(mean.humidity),
            doss=mean(canopy_cover),
            doss.sd = sd(canopy_cover),
            hei=mean(understory_height),
            hei.sd = sd(understory_height))
preds %>% 
  filter(disturbance=="medio") %>% 
  summarise(umi = mean(mean.humidity),
            temp=mean(mean.temperature),
            temp.sd = sd(mean.temperature),
            umi.sd = sd(mean.humidity),
            doss=mean(canopy_cover),
            doss.sd = sd(canopy_cover),
            hei=mean(understory_height),
            hei.sd = sd(understory_height))
preds %>% 
  filter(disturbance=="alto") %>% 
  summarise(umi = mean(mean.humidity),
            temp=mean(mean.temperature),
            temp.sd = sd(mean.temperature),
            umi.sd = sd(mean.humidity),
            doss=mean(canopy_cover),
            doss.sd = sd(canopy_cover),
            hei=mean(understory_height),
            hei.sd = sd(understory_height))


