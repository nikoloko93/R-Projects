library(tidyverse)

compg <- cgm_comp_clean %>% 
  mutate(Group="Comparison") %>% data.frame()
speg <- cgm_spe_clean %>% 
  mutate(Group="SPE") %>% data.frame()
egv_raw3 <- rbind(compg,speg)

#------------------------------ PERMUTATION TEST ON MEAN EGV ---------------------------------#
data2 <- egv_raw3 %>% 
  group_by(savvy_id, Group) %>% 
  summarise(egv_mean=mean(egv), wei = n())

permu_egv <-permu.test.weighted(data = "data2", voi = "egv_mean",
                               group.me = "Group", weight.me = "wei", iter=1000)

tibble(`OBSERVED DIFFERENCE`=permu_egv$mean,`P-VALUE`=permu_egv$pvalue)

#--------------------------------- PERMUTATION TEST ON TIR -----------------------------------#
data2a <- egv_raw3 %>% 
  mutate(TIR=between(egv, 80, 180))

data2b <- data2a %>% 
  group_by(savvy_id, Group) %>% 
  summarise(TIR=mean(TIR, na.rm=TRUE))
permu <- permu.test.unweighted(data = data2b, data2b$TIR, group.me = data2b$Group)

tibble(`OBSERVED DIFFERENCE`=permu$mean[[1]],`P-VALUE`=permu$pvalue)
