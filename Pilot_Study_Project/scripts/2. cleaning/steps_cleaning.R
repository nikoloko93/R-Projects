library(tidyverse)
library(bigquery)
options("httr_oauth_cache"="~/.httr-oauth",httr_oob_default = TRUE)

# creating query to pull up data
Q_Step <- paste("select A.savvy_id, A.step_day_id, A.step_date, A.steps
                from GMP.fact_step_day as A WHERE savvy_id IN (",paste0(unique(CompandSPEgroups$savvy_id), collapse = ","), ")  ")
Step <- query_exec(Q_Step, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)

Step <- data.frame(Step %>% group_by(savvy_id, step_day_id, step_date) %>% 
                     summarise(steps=mean(steps)))

step_raw <- left_join(Step, CompandSPEgroups, by="savvy_id")