library(bigrquery)
library(tidyverse)
library(readr)
library(lubridate)
library(data.table)

options("httr_oauth_cache"="~/.httr-oauth",httr_oob_default = TRUE)

#------------------------------ LOADING THE DATA ------------------------------#

## Loading tags for open-ended questions
Q78_tag <- read_csv("Q78_tag.csv")
Q83_tag <- read_csv("Q83_tag.csv")

## Loading the data from big query
query <- "select question_nbr, savvy_id, coaching_date, question, answer, coaching_data_create_date_time_ct
from GMP.fact_coaching 
where question_nbr=20 or question_nbr=30 or question_nbr between 77 and 83"
survey <- query_exec(query, project = "research-00", use_legacy_sql = FALSE, max_pages = Inf)

#------------------------------ EXPLORATION STARTS ------------------------------#

## survey data started at 27 Aug 2018
survey_data <- survey %>% 
  filter(!is.na(answer)) %>% 
  filter(coaching_date >= as.Date("2018-08-20"))

## Standardizing format for coaching dates to week nos
week_no <- survey_data %>%
  select(savvy_id,coaching_date) %>%
  unique() %>% 
  group_by(savvy_id) %>%
  arrange(savvy_id,coaching_date) %>% 
  mutate(week_no = row_number())

survey_data <- left_join(survey_data,week_no,by=c("savvy_id","coaching_date")) %>%
  select(savvy_id,coaching_date,week_no,question_nbr,question,answer)

survey_spe2 <- survey_data %>%
  filter(savvy_id %in% cgm_spe_clean$savvy_id) 

## Check if there are duplicate questions
survey_spe2 %>% # savvy_id 21071 have 2 answers for the same question in the same date
  group_by(savvy_id, coaching_date, question_nbr) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

#------------------------------ REMOVING DUPLICATES ------------------------------#

survey_spe2 <- survey_spe2 %>%
  filter(!(savvy_id == 21071 & answer == "No comment at this time." & coaching_date == as.Date("2018-10-11"))) 
survey_spe2 <- survey_spe2[!duplicated(survey_spe2),]
