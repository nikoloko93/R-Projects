library(bigrquery)
library(tidyverse)
options("httr_oauth_cache"="~/.httr-oauth",httr_oob_default = TRUE)

#------------------------------ SPE2 ------------------------------#

## Extracts local date and time, egv, and demogs -- age, gender, raf, mm2017, mm2018
query <- "SELECT A.savvy_id,DATE(A.egv_date_time_local) AS date_local, TIME(A.egv_date_time_local) AS time_local,A.egv,C.birth_year,C.gender,D.raf_max,C.mm_2017,C.mm_2018
FROM GMP.fact_egv as A LEFT JOIN GMP.fact_cohort as B ON A.savvy_id = B.savvy_id
LEFT JOIN GMP.dim_member as C ON B.savvy_id = C.savvy_id
LEFT JOIN GMP.fact_raf_score as D ON C.savvy_id = D.savvy_id
WHERE B.dim_cohort_id = 11 AND (D.year = '2018' OR D.year IS NULL)"
cgm_spe <- query_exec(query, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)

## Check if start dates are around August
cgm_spe %>%
  group_by(savvy_id) %>%
  summarise(start_date = min(date_local), end_date = max(date_local)) %>%
  arrange(start_date)

## Check for missing/discontinuous readings for more than 1 week
missing <- cgm_spe %>% # just assume 0 for discontinuous days
  filter(savvy_id %in% c(20177,21071,20686,21094,14952,10632)) %>% # 20686 - 10 days / 21094 - 43 days
  group_by(savvy_id, date_local) %>%
  summarise(mean_egv = mean(egv))

seq_date <- crossing(seq.Date(min(cgm_spe$date_local),max(cgm_spe$date_local),by=1) %>% as.data.frame(),unique(missing$savvy_id) %>% as.data.frame())
colnames(seq_date) <- c("date_local","savvy_id")
line_df <- merge(seq_date,missing,by=c("savvy_id","date_local"),all.x=TRUE)

ggplot(data = line_df, aes(x=date_local,y=mean_egv)) +
  geom_line(aes(color = as.factor(savvy_id))) +
  scale_x_date(date_breaks = '1 week') +
  scale_color_discrete(name = "SAVVY_ID") +
  ggtitle("MISSING CGM READINGS") +
  xlab("DATE LOCAL") +
  ylab("MEAN EGV") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold"),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed")) 

## Check for duplicates
dupes <- cgm_spe %>% # 18 people have duplicates
  group_by(savvy_id, date_local, time_local,egv) %>%
  summarise(count = n()) %>%
  filter(count > 1)

## Check for same datetime, diff egv
dupes2 <- cgm_spe %>% # 18 people have duplicates
  group_by(savvy_id, date_local, time_local) %>%
  summarise(count = n()) %>%
  filter(count > 1)

cgm_spe  %>% # 20 Oct 2018 readings for savvy_id 32463 have same date and time but different egv readings
  filter(savvy_id %in% dupes2$savvy_id,date_local %in% dupes2$date_local,
         time_local %in% dupes2$time_local) %>% 
  arrange(time_local) %>%
  select(savvy_id,date_local,time_local,egv)

## CLEANING PROPER: Removing the duplicates
## Part 1 utilized the summarise function to 1) remove the duplicate readings and 
## 2) get the average for same datetime but diff readings
cgm_spe_part1 <- cgm_spe %>%
  group_by(savvy_id, date_local,time_local) %>%
  summarise(egv = mean(egv))
cgm_spe_part2 <- cgm_spe %>% 
  select(c("savvy_id","birth_year","gender","raf_max","mm_2017","mm_2018")) %>%
  unique()
cgm_spe_clean <- left_join(cgm_spe_part1,cgm_spe_part2,by=c("savvy_id"))


#------------------------------ COMPARISON GROUP ------------------------------#

## Loading savvy_ids with matched age, gender, and RAF with SPE2 members
comp_SPE <- CompandSPEgroups

## Pulling in a string of savvy_ids to be pulled from big query
comp_ID <- comp_SPE %>%
  filter(Group == "Comparison") %>%
  pull(savvy_id)
x <- numeric()
for(i in 1:length(comp_ID)) {
  id <- comp_ID[i]
  x <- paste0(x,id,sep=",")
}
x <- substr(x,1,nchar(x)-1)

## Loading the data from big query
query <- paste0("SELECT A.savvy_id,DATE(A.egv_date_time_local) AS date_local, TIME(A.egv_date_time_local) AS time_local,A.egv,C.birth_year,C.gender,D.raf_max,C.mm_2017,C.mm_2018 FROM GMP.fact_egv as A LEFT JOIN GMP.fact_cohort as B ON A.savvy_id = B.savvy_id LEFT JOIN GMP.dim_member as C ON B.savvy_id = C.savvy_id
LEFT JOIN GMP.fact_raf_score as D ON C.savvy_id = D.savvy_id WHERE A.savvy_id IN (", x,") AND (D.year = '2018' OR D.year IS NULL)")
cgm_comp <- query_exec(query, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)

## Comp group members have varying start and end dates of CGM use (some are still using it until now)
## We set a definition that we'll only get 2 months worth of data from them starting from their first dates
## as SPE2 lasted only from Aug 29 to Oct 24 (~ 2 months)

## Check for ## Check for missing/discontinuous readings for more than 1 week
cgm_comp %>%
  group_by(savvy_id) %>%
  summarise(start_date = min(date_local), end_date = max(date_local), new_end = min(date_local) + 60) %>%
  mutate(days = end_date - start_date) %>%
  arrange(start_date)

cgm_comp_new <- cgm_comp %>%
  group_by(savvy_id) %>%
  filter(between(date_local,min(date_local),min(date_local) + 60))

missing <- cgm_comp_new %>% # just assume 0 for discontinuous days
  filter(savvy_id %in% unique(cgm_comp$savvy_id)[16:20],
         date_local <= new_end) %>% 
  group_by(savvy_id, date_local) %>%
  summarise(mean_egv = mean(egv))

seq_date <- crossing(seq.Date(min(cgm_comp_new$date_local),max(cgm_comp_new$date_local),by=1) %>% as.data.frame(),unique(missing$savvy_id) %>% as.data.frame())
colnames(seq_date) <- c("date_local","savvy_id")
line_df <- merge(seq_date,missing,by=c("savvy_id","date_local"),all.x=TRUE)

ggplot(data = line_df, aes(x=date_local,y=mean_egv)) +
  geom_line(aes(color = as.factor(savvy_id))) +
  scale_x_date(date_breaks = '1 week') +
  scale_color_discrete(name = "SAVVY_ID") +
  ggtitle("MISSING CGM READINGS") +
  xlab("DATE LOCAL") +
  ylab("MEAN EGV") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold"),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed")) 

## Check for duplicates
dupes <- cgm_comp_new %>% # 8 people have duplicates
  group_by(savvy_id, date_local, time_local,egv) %>%
  summarise(count = n()) %>%
  filter(count > 1)

## Check for same datetime, diff egv
dupes2 <- cgm_comp_new %>% # 8 people have duplicates
  group_by(savvy_id, date_local, time_local) %>%
  summarise(count = n()) %>%
  filter(count > 1)

## CLEANING PROPER: Removing the duplicates
## Part 1 utilized the summarise function to 1) remove the duplicate readings and 
## 2) get the average for same datetime but diff readings
cgm_comp_part1 <- cgm_comp_new %>%
  group_by(savvy_id, date_local,time_local) %>%
  summarise(egv = mean(egv))
cgm_comp_part2 <- cgm_comp_new %>% 
  select(c("savvy_id","birth_year","gender","raf_max","mm_2017","mm_2018")) %>%
  unique()
cgm_comp_clean <- left_join(cgm_comp_part1,cgm_comp_part2,by=c("savvy_id"))
