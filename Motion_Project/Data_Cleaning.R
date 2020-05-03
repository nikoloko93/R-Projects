library(RODBC)
library(tidyverse)
library(lubridate)
library(imputeTS)


# pulling up the data
db <- odbcConnect("dbsep3832")
df1 <- sqlQuery(db ,"SELECT * FROM pdb_MotionEnrollmentEngagement..KA_Motion_Enr_DropRate_201807")



# arrange table by  groups, Id,and yearmo
# adding new variables: Year_mo in string ,number of days in a month, and percentage of activeness

df2 <- df1 %>% 
  filter(Age>=18) %>% 
  filter(KA_Flag==1) %>% 
  filter(!(is.na(Group1_Flag)&is.na(Group2_Flag))) %>% 
  filter(!is.na(ComEnrld_Yr_Mo)) %>% 
  mutate(yr_mo=ifelse(is.na(Year_Mo), ComEnrld_Yr_Mo, Year_Mo)) %>%
  arrange(Indv_Sys_ID,yr_mo) %>% 
  mutate(Year_mo_str=paste0(month.name[as.numeric(str_sub(as.character(yr_mo), -2,-1))],
                            " " ,str_sub(as.character(yr_mo),1,4)),
         num_days=days_in_month(ymd(as.numeric(paste0(as.character(yr_mo), "01")))),
         cnt_active1=ifelse(is.na(Cnt_Active),0,Cnt_Active),
         percent_active=cnt_active1/num_days,
         eligible_flag=ifelse(is.na(Year_Mo), "not eligible", "eligible"), 
         enroll_flag=ifelse(is.na(Year_Mo_Enrollment),"not enrolled","enrolled"),
         active_flag=ifelse(ActiveFlag_YrMo==1, "Active", "Inactive"),
         active_flag1=ifelse(cnt_active1 >0 & (is.na(Adjustment_Incentive)|Adjustment_Incentive>=40), "Active", "Inactive"))

# checking for duplicates: Findings: Overall, There are 50 members with duplicate readings.
a <- df2 %>% 
   group_by(Indv_Sys_ID, yr_mo) %>% 
   summarise(n=n()) %>% 
   filter(n>1) %>% 
   mutate(id1=paste0(Indv_Sys_ID, yr_mo))
 
# List of members with duplicate readings
 lst1 = unique(a$Indv_Sys_ID)

# Pulling out duplicate data
df2a <- df2 %>% 
  mutate(id2 = paste0(Indv_Sys_ID, yr_mo)) %>% 
  filter(id2 %in% a$id1) 



df2b = tibble()
aa = filter(a,!Indv_Sys_ID  %in% c(1381674208))

for (i in 1:nrow(aa)){
  b = df2a %>% 
    filter(id2==aa[i,4][[1]])
  if (nrow(b)==2){
    if (sum(is.na(b$Cnt_Active))==sum(!is.na(b$Cnt_Active))){
      c=b %>% filter(!is.na(b$Cnt_Active))
      df2b = rbind(df2b,c)
    }
  }
  else if(nrow(b)==4){
     c=b %>% filter(!is.na(Year_Mo_Enrollment))
     df2b=rbind(df2b,c[2,])
  }
 
}


aaa = filter(a,Indv_Sys_ID ==1381674208)
for (i in 1:nrow(aaa)){
   b = df2a %>% 
    filter(id2==aaa[i,4][[1]])
   df2b=rbind(df2b,b[2,])
}


a1 <- df2b %>% 
  group_by(Indv_Sys_ID, yr_mo) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>% 
  mutate(id3=paste0(Indv_Sys_ID, yr_mo))


# View(df2a%>% filter(Indv_Sys_ID==1381674208))

df3 <- df2 %>% 
  mutate(id2 = paste0(Indv_Sys_ID, yr_mo)) %>% 
  filter(!id2 %in% a$id1)
df3 <- rbind(df3, df2b)

# filtering out members who are not enrolled in UHC
df3 <- df3 %>% filter(!is.na(ComEnrld_Yr_Mo))


str(df3)
df3[14:19][df3[14:19]<0] <- 0
df3[14:19][is.na(df3[14:19])] <- 0
df3$Med_Income[df3$Med_Income==0] <- NA
df3$Gdr_Cd[df3$Gdr_Cd=="U"] <-NA
df3$Gdr_Cd <- factor(df3$Gdr_Cd)
levels(df3$Gdr_Cd)
df3[31:37][df3[31:37]==0] <- "N"
df3[31:37][df3[31:37]==1] <- "Y"
df3[39][df3[39]==0] <- NA




#dividing the data into two groups
Group1_dat <- df3 %>% filter(!is.na(Group1_Flag))
Group2_dat <- df3 %>% filter(!is.na(Group2_Flag))

mem1 <- Group1_dat %>% 
  filter(eligible_flag=='eligible') %>% 
  filter(Year_mo_str=="July 2017") %>%
  select(Indv_Sys_ID)
  
mem2 <- Group2_dat %>% 
  filter(eligible_flag=='eligible') %>% 
  filter(Year_mo_str=="July 2018") %>%
  select(Indv_Sys_ID)
 
Group1_dat <- left_join(mem1, Group1_dat, by = "Indv_Sys_ID")
Group2_dat <- left_join(mem2, Group2_dat, by = "Indv_Sys_ID")


save(Group1_dat, file="g1.rda")
save(Group2_dat, file="g2.rda")



