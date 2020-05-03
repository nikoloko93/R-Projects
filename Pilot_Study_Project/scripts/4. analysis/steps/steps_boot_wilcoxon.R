library(tidyverse)
library(boot)
library(stats)

## Extracting first 2 weeks of SPE2
Before <- stp_me_firstandlast %>%
  filter(id=="BEFORE")%>%
  group_by(savvy_id) %>%
  summarise(avg_steps = mean(steps,na.rm=TRUE), weights = n()) 

## Extracting last 2 weeks of SPE2
After <- stp_me_firstandlast %>%
  filter(id=="AFTER")%>%
  group_by(savvy_id) %>%
  summarise(avg_steps = mean(steps,na.rm=TRUE), weights = n())

## Combining the data
boot_data <- merge(Before,After,by="savvy_id")
colnames(boot_data) <- c("savvy_id","pre_mean","pre_weight","post_mean","post_weight")

#----------------------------------- BOOTSTRAPPING ------------------------------------#
diff <- boot(boot_data, statistic=diff_fn,R=1000)
boot.ci(diff, type = "norm")

#----------------------------------- WILCOXON TEST ------------------------------------#
wilcox.test(boot_data$pre_mean, boot_data$post_mean, paired=TRUE)