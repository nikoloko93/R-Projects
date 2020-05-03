# Code for reading and formatting data for Key Account Motion Enrollment Rate Drop Grant
# 
# Created by: Joyvalerie Mondejar
# Date:       12/07/2018
# 
# Inputs:  pdb_MotionEnrollmentEngagement.dbo.KA_Motion_Enr_DropRate (on DBSEP3832 server inside NGIS)
# Inputs:  pdb_MotionEnrollmentEngagement.dbo.KA_Motion_Enr_DropRate_201807 (on DBSEP3832 server inside NGIS)
# Code for Key Account Motion data
library(data.table)
library(ggplot2)
library(RODBC)
library(zoo)
library(lubridate)

rm(list = ls())

# setwd("/app1/home/jmondej3/Motion Enrollment Drop")
db <- odbcConnect("dbsep3832")

enrich <- function(dat) {
  dat[, Gdr_Cd := factor(Gdr_Cd)]
  dat[, Year_Mo := as.integer(Year_Mo)]
  # dat[, RAF_Score := sample(c(0,1), .N, replace = T)]
  # dat[, RAF_Score := runif(.N, min=0, max=100)]
  # dat[, c("RAF_Score", "Med_Income", "Total_Incentive"):=lapply(.SD, as.character),
  #     .SDcols = c("RAF_Score", "Med_Income", "Total_Incentive")]
  dat[, c("IP_Allw_Amt", "OP_Allw_Amt", "DR_Allw_Amt", "Rx_Allw_Amt", "ER_Allw_Amt", "Total_Allw_Amt", 
          "Med_Income", "Total_Incentive", "Adjustment_Incentive", "RAF_Score"):=lapply(.SD, as.numeric), 
      .SDcols = c("IP_Allw_Amt", "OP_Allw_Amt", "DR_Allw_Amt", "Rx_Allw_Amt", "ER_Allw_Amt", "Total_Allw_Amt", 
                  "Med_Income", "Total_Incentive", "Adjustment_Incentive",  "RAF_Score")]  
  dat[, c("Cnt_Active","Cnt_FIT", "Cnt_F", "Cnt_I", "Cnt_T") := lapply(.SD, as.integer),.SDcols = c("Cnt_Active","Cnt_FIT", "Cnt_F", "Cnt_I", "Cnt_T")]
  dat[, c("T2D_Flag", "Hypertension_Flag", "Dep_Anx_Flag", "COPD_Flag", "CHF_Flag", "RA_Flag", "ChronicPain_Flag") :=
        lapply(.SD, as.character), .SDcols = c("T2D_Flag", "Hypertension_Flag", "Dep_Anx_Flag", "COPD_Flag", "CHF_Flag", "RA_Flag", "ChronicPain_Flag")]
  dat[, Sbscr := factor(Sbscr_Ind, levels = 0:1, labels = c("Dependent", "Subscriber"))]
  dat[, USR_Ind := factor(USR_Ind, levels = c("U", "S", "R", NA))]
  dat[, Year_Mo_Enrollment := as.integer(Year_Mo_Enrollment)]
  # dat[, Enroll_Flag := sample(c(0,1),nrow(dat),replace=T)]
  dat[, Mo_Enroll_Flg := as.numeric(Mo_Enroll_Flg)]
  dat[, Enroll_Flag := ifelse(!is.na(Year_Mo_Enrollment), 1, 0)]
  dat[, ActiveFlag_YrMo := as.numeric(ActiveFlag_YrMo)]
  dat[, c('Year', 'Month') := .(substr(Year_Mo, 1, 4), substr(Year_Mo, 5, 6))]
  dat[, Year_Mo_ := paste(Year, Month, sep = '-')]
  dat[, Year_Mo_ := as.yearmon(Year_Mo_)]
  
}

data_all <- data.table(sqlQuery(db, "select * from pdb_MotionEnrollmentEngagement.dbo.KA_Motion_Enr_DropRate_201807", as.is = TRUE))
setkeyv(data_all, c('Indv_Sys_ID', 'ComEnrld_Yr_Mo', 'Year_Mo', 'Cnt_Active', 'Year_Mo_Enrollment'))
dim(data_all); dim(unique(data_all, by = c("Indv_Sys_ID", "Year_Mo")))  #  Now 465758     39  463815     39
dim(unique(data_all, by = c("Indv_Sys_ID")))  # 55946    39
data_all[is.na(ComEnrld_Yr_Mo),.N]  # 74142
data_all[is.na(Year_Mo),.N]  #   2395
data_all[is.na(Year_Mo_Enrollment),.N]  #  Now  232866

mm <- enrich(data_all)
all.equal(mm$Mo_Enroll_Flg, mm$Enroll_Flag)
save(mm, file = "Data/mm.rda")
mm[Group1_Flag == 1,.N, keyby = Year_Mo]
mm[Group2_Flag == 1,.N, keyby = Year_Mo]
dim(unique(mm, by = c("Indv_Sys_ID")))  # 55946    43
dim(unique(data_all[Group1_Flag == 1], by = c("Indv_Sys_ID")))  # 15081    44
dim(unique(data_all[Group2_Flag == 1], by = c("Indv_Sys_ID")))  # 53021    44
dim(mm); dim(unique(mm, by = c("Indv_Sys_ID", "Year_Mo")))  # 465758     44    463815     44          1943


# Population 1: Members of employers eligible for KA motion program in July 2017
data_1 <- data.table(sqlQuery(db, "select * from pdb_MotionEnrollmentEngagement.dbo.KA_Motion_Enr_DropRate_201807 where Group1_Flag = 1", as.is = TRUE))
setkeyv(data_1, c('Indv_Sys_ID', 'ComEnrld_Yr_Mo', 'Year_Mo', 'Cnt_Active', 'Year_Mo_Enrollment'))
dim(data_1); dim(unique(data_1, by = c("Indv_Sys_ID", "Year_Mo")))  #  210263     39     210259     39
dim(unique(data_1, by = c("Indv_Sys_ID")))  #  15081    39

mm1 <- enrich(data_1)
save(mm1, file = "Data/mm1.rda")


# Population 2: Members of employers eligible for KA motion program in July 2018
data_2 <- data.table(sqlQuery(db, "select * from pdb_MotionEnrollmentEngagement.dbo.KA_Motion_Enr_DropRate_201807 where Group2_Flag = 1", as.is = TRUE))
setkeyv(data_2, c('Indv_Sys_ID', 'ComEnrld_Yr_Mo', 'Year_Mo', 'Cnt_Active', 'Year_Mo_Enrollment'))
dim(data_2); dim(unique(data_2, by = c("Indv_Sys_ID", "Year_Mo")))  # 431903     39    431854     39
dim(unique(data_2, by = c("Indv_Sys_ID")))  # 53021    39

mm2 <- enrich(data_2)
save(mm2, file = "Data/mm2.rda")

odbcClose(db)


# End script


