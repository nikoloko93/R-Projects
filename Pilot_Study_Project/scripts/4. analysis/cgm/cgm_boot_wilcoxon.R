library(tidyverse)
library(boot)
library(stats)

## Extracting first 2 weeks of SPE2
Before <- cgm_me_firstandlast %>% 
  filter(id=="BEFORE") %>% 
  mutate(TIR = between(egv,80,180))

Before_prop <- Before %>%
  group_by(savvy_id) %>%
  summarise(TIR = mean(TIR,na.rm=TRUE))

## Extracting last 7 and 8
After <- cgm_me_firstandlast %>% 
  filter(id=="AFTER") %>% 
  mutate(TIR = between(egv,80,180))

After_prop <- After %>%
  group_by(savvy_id) %>%
  summarise(TIR = mean(TIR,na.rm=TRUE))

## Combining the data
boot_data <- merge(Before_prop,After_prop,by="savvy_id") 
colnames(boot_data) <- c("savvy_id","pre_mean","post_mean")

##-------------------------------- BOOTSTRAPPING ----------------------------------#
boot_data <- merge(Before_prop,After_prop,by="savvy_id") 
colnames(boot_data) <- c("savvy_id","pre_mean","post_mean")

##-------------------------------- BOOTSTRAPPING ----------------------------------#
diff <- boot(boot_data, statistic=diff_fn,R=1000)
boot.ci(diff, type = "norm")

##-------------------------------- WILCOXON TEST ----------------------------------#
wilcox.test(boot_data$pre_mean, boot_data$post_mean, paired=TRUE)
