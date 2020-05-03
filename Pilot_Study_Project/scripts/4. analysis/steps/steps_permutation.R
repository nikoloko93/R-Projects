library(tidyverse)

# pulling steps data of comp group 
step2 <- tibble()
for (i in 1:20){
  c <- step_raw %>% 
    filter(savvy_id==comp_dates$savvy_id[i]) %>% 
    filter(step_date>=as.Date(comp_dates$start_date[i])&
             step_date<=as.Date(comp_dates$new_end[i]))
  step2 <- rbind(step2, c)
}

# merging steps data of spe2 and comp group
data1 <- rbind(stp_me[,c(1,3,2,4,8)], step2[,c(1:4,8)])

data1a <- data1 %>% 
  filter(!is.na(steps)) %>% 
  group_by(savvy_id, Group) %>% 
  summarise(steps=mean(steps), cnt=n())

# running the function permu.test.weigts()
permu_step <- permu.test.weighted(data = "data1a", voi = "steps",
                                 group.me = "Group", weight.me = "cnt", iter=1000)

tibble(`OBSERVED DIFFERENCE`=permu_step$mean,`P-VALUE`=permu_step$pvalue)
