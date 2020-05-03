library(tidyverse)

#------------------------------ EXTRACTING DATES OF SPE2 AND COMP GROUP ------------------------------#

## Setting the duration of analysis for only 60 days --
## starting from the participants' first date upto their 60th day
## Some participants only have data prior to their 60th day, in which case their end date is their max date

spe_stend1 <- cgm_spe_clean %>%
  filter(savvy_id %in% c(21071,20177)) %>% 
  group_by(savvy_id) %>% 
  summarise(start_date=min(date_local), end_date=max(date_local)) %>% data.frame()
spe_stend1$start_date <- c(as.Date("2018-08-27"), as.Date("2018-08-20"))

spe_stend2 <- cgm_spe_clean %>%
  filter(!savvy_id %in% c(21071,20177)) %>% 
  group_by(savvy_id) %>% 
  summarise(start_date=min(date_local), end_date=max(date_local)) %>% data.frame()
spe_stend <- rbind(spe_stend1, spe_stend2) 

spe_stend <- spe_stend %>% 
  mutate(new_end1 = start_date+60) %>% 
  mutate(new_end = ifelse(new_end1>end_date, as.character(end_date), as.character(new_end1))) 
spe_stend$new_end <- as.Date(spe_stend$new_end)
spe_stend <- spe_stend %>% 
  mutate(s14=start_date+14, e14=new_end-14)

comp_dates <- cgm_comp_clean %>%
  group_by(savvy_id) %>%
  summarise(start_date = min(date_local), new_end = min(date_local) + 60, end_date = max(date_local)) %>%
  mutate(days = end_date-start_date)


#------------------------------ SPE2 STEPS ------------------------------#
step1 <- step_raw %>% 
  filter(Group=="SPE")

# 60th day is the end_date of each participant for step
# if the 60th day exceed 2018-10-25, max available data, then, end_date is 2018-10-25 else the 60th day
spe_stend3 <- spe_stend %>% 
  select(savvy_id,start_date, end_date, new_end1) %>% 
  mutate(new_end=ifelse(new_end1>as.Date("2018-10-25"),"2018-10-25",as.character(new_end1 )))
spe_stend3$new_end <- as.Date(spe_stend3$new_end)
spe_stend3 <- spe_stend3 %>% 
  mutate(s14=start_date+14, e14=new_end-14)

# sequence of 60 days, first 14 days, and last 14 days
stp_me <- tibble()
stp_me_firstandlast <- tibble()
for (i in 1:20){
  d <- tibble(savvy_id=spe_stend$savvy_id[i],
              step_date=seq.Date(spe_stend3$start_date[i], spe_stend3$new_end[i], by=1))
  s14 <- tibble(savvy_id=spe_stend3$savvy_id[i],
                step_date=seq.Date(spe_stend3$start_date[i], spe_stend3$s14[i], by=1), id="BEFORE")
  e14 <- tibble(savvy_id=spe_stend3$savvy_id[i],
                step_date=seq.Date(spe_stend3$e14[i], spe_stend3$new_end[i], by=1), id="AFTER")
  sne14 <- rbind(s14,e14)
  sne14 <- left_join(sne14,step1, by=c("savvy_id", "step_date") )
  d <- left_join(d, step1, by=c("savvy_id", "step_date"))
  stp_me <- rbind(stp_me, d)
  stp_me_firstandlast <- rbind(stp_me_firstandlast, sne14)
}

# filter out 20686 since the data for this participant is small
stp_me_firstandlast<-stp_me_firstandlast %>% 
  filter(!savvy_id==20686)

#------------------------------ SPE2 CGM ------------------------------#
# sequence of 60 days, first 14 days, and last 14 days
cgm_me <- tibble()
cgm_me_firstandlast <- tibble()
for (i in 1:20){
  d <- tibble(savvy_id=spe_stend$savvy_id[i],
              date_local=seq.Date(spe_stend$start_date[i], spe_stend$new_end[i], by=1))
  s14 <- tibble(savvy_id=spe_stend$savvy_id[i],
                date_local=seq.Date(spe_stend$start_date[i], spe_stend$s14[i], by=1), id="BEFORE")
  e14 <- tibble(savvy_id=spe_stend$savvy_id[i],
                date_local=seq.Date(spe_stend$e14[i], spe_stend$new_end[i], by=1), id="AFTER")
  sne14 <- rbind(s14,e14)
  sne14 <- left_join(sne14,cgm_spe_clean, by=c("savvy_id", "date_local") )
  d <- left_join(d, cgm_spe_clean, by=c("savvy_id", "date_local"))
  cgm_me <- rbind(cgm_me, d)
  cgm_me_firstandlast <- rbind(cgm_me_firstandlast, sne14)
}

# filter out 20686 since the data for this participant is small
cgm_me_firstandlast<-cgm_me_firstandlast %>% 
  filter(!savvy_id==20686)
