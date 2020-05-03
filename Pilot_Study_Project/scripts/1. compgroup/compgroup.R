library(bigrquery)
library(tidyverse)
options("httr_oauth_cache"="~/.httr-oauth", httr_oob_default=TRUE)

# creating query
query <- "select A.savvy_id, A.min_egv_date_time_local, A.max_egv_date_time_local, B.dim_cohort_id,
C.cohort, C.cohort_type, D.birth_year, D.gender, D.mm_2017, D.mm_2018 
from GMP.fact_egv_summary  as  A 
left join GMP.fact_cohort       as  B on A.savvy_id = B.savvy_id 
left join GMP.dim_cohort        as  C on B.dim_cohort_id = C.dim_cohort_id 
left join GMP.dim_member        as  D on A.savvy_id = D.savvy_id  
order by A.savvy_id, B.dim_cohort_id"

query1 <- "select A.savvy_id, A.year, A.raf_max
from GMP.fact_raf_score as A"

# running the query created to pull the egv and raf data
current <-  query_exec(query, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)
raf <-  query_exec(query1, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)


# removing duplicate raf recording
a <- raf %>% group_by(savvy_id, year) %>% summarise(Raf=mean(raf_max))

# removing duplicate egv reading
current2 <- current %>% 
  group_by(savvy_id, min_egv_date_time_local, max_egv_date_time_local, dim_cohort_id, cohort, cohort_type, birth_year,
gender, mm_2017, mm_2018) %>% 
  summarise(n=n())

# joining the two table
current3 <- left_join(current2, a, "savvy_id")

# add variable age in the data as well as the limits of age and raf
# limits will help us identify who is at least closest to the SPE participants
current4 <- current3 %>% 
  spread(key = "year", value = "Raf") %>% 
  mutate(Age=2018-birth_year) %>% 
  mutate(left = ifelse(is.na(`2018`),0, `2018`-0.15), right = ifelse(is.na(`2018`),0, `2018`+0.15),
         agel = Age-2, ager = Age+2)
  


# Removing SPE, SPE2, test, enrolled participants
# Our objective is to get the control participants that dont have any interventions to other pilots
# We remove SPEs since these are the target populations

# checking the counts
tmp1<- current4 %>% 
  filter(cohort_type=="test"|cohort_type=="enrolled")
tmp2<- current4 %>% 
  filter(cohort_type=="eligible")
SPE_2 <- current4 %>% 
  filter(dim_cohort_id==11)

# filtering out participants who are enrolled and at the same time are eligible and filtering out SPE participant 
eli_par <- current4 %>% 
  filter(cohort_type=="eligible") %>% 
  filter(!savvy_id %in% tmp1$savvy_id) %>%
  filter(!savvy_id%in% SPE_2$savvy_id) %>% 
  filter(dim_cohort_id!=7) %>% 
  filter(dim_cohort_id!=8)


# SPE group data
Speme <- SPE_2[, c(1,8, 22:26, Raf=20)]

# possible comaprison group
Compme <- eli_par %>% 
  group_by(savvy_id, gender, Age ) %>% 
  summarise(me=mean(`2018`)) %>% 
  mutate(Raf=ifelse(is.na(me), 0, me))

# checking for possible matches for every participant in SPE2
eval_data <- tibble()
for (i in 1:20){
  b <- Compme %>% 
    mutate(iden=ifelse(gender==SPE_2$gender[i], "T", "F"),
           iden2=ifelse(Age>=SPE_2$agel[i]&Age<=SPE_2$ager[i],"T", "F")) %>% 
    mutate(ident=ifelse(iden=="T"&iden2=="T", "T", "F")) %>%  
    select(-iden,-iden2) %>% filter(ident=="T")
  b$Sav_id <- SPE_2$savvy_id[i]
  b$Raf_T <- SPE_2$`2018`[i]
  b$gender_t<- SPE_2$gender[i]
  b$Age_t <- SPE_2$Age[i]
  eval_data <- rbind(eval_data,data.frame(b))
}
# save the identified possible comparison for each SPE participants for manual matching later
# write.csv(eval_data, "eval_data.csv")

# after having the list of possible comparison for every SPE2 participant, manually match SPE pt 
# to the list of identified comparison in excel
# pull up the identified comparison group

comparison_grp <- read_csv("comparison_grp.csv")

# join the data  
comp_group <- left_join(comparison_grp, Compme)[,-4] %>% 
  mutate(Group="Comparison")
spe_group <- Speme[,c(1:3, 8)] %>% 
  mutate(Raf=ifelse(is.na(`2018`), 0, `2018`), Group="SPE") %>% 
  select(-`2018`)

# join spe and comaprison data
CompandSPEgroups <- rbind(spe_group, comp_group)
  

