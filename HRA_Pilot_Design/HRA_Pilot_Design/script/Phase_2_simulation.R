library(readxl)
library(tidyverse)
library(effsize)
library(pwr)
library(parallel)
library(foreach)
library(doParallel)
library(lme4)
library(ggExtra)
# Load the data
HRA_Pharmacy_dat <- read_excel("Data/HRA_Pharmacy_Analysis_v5.xlsx")

# REMOVING MAILED ORDER PHARMACIES: BRIOVARX, DIVVYDOSE, PILLPACK

HRA_Pharma_clean <- HRA_Pharmacy_dat %>% 
  mutate(PH_name = gsub("\\-+$","",gsub("\\s+$","",gsub("#","",gsub("[0-9]","", Pharmacy_Name))))) %>% 
  select(Pharmacy_NPI, Pharmacy_Name, PH_name, everything()) %>% 
  filter(!grepl("BRIOVARX|DIVVYDOSE|PILLPACK", Pharmacy_Name))

# CHECKING DUPLICATES
dups_data <- HRA_Pharma_clean %>%
  filter(Pharmacy_NPI %in% HRA_Pharma_clean[duplicated(HRA_Pharma_clean$Pharmacy_NPI),]$Pharmacy_NPI)


dist_dup <-HRA_Pharma_clean %>%
  filter(Pharmacy_NPI %in% HRA_Pharma_clean[duplicated(HRA_Pharma_clean$Pharmacy_NPI),]$Pharmacy_NPI) %>%
  group_by(Pharmacy_NPI) %>%
  summarise(
    Pharmacy_Name = paste(unique(Pharmacy_Name), collapse = " | "), 
    PH_name = paste(unique(PH_name), collapse = " | "),
    Pharmacy_Network = paste(unique(Pharmacy_Network), collapse = " | "),
    Pharmacy_Code = paste(unique(Pharmacy_Code), collapse = " | "),
    Pharmacy_Address1 = paste(unique(Pharmacy_Address1), collapse = " | "),
    Pharmacy_Address2 = paste(unique(Pharmacy_Address2), collapse = " | "),
    Pharmacy_City = paste(unique(Pharmacy_City), collapse = " | "),
    Pharmacy_State = paste(unique(Pharmacy_State), collapse = " | "),
    Pharmacy_Zip = paste(unique(Pharmacy_Zip), collapse = " | "),
    Pharmacy_Phone = paste(unique(Pharmacy_Phone), collapse = " | "),
    Mail_Order = paste(unique(Mail_Order), collapse = " | "),
    hra_count = max(hra_count),
    unique_member_count = max(unique_member_count),
    member_prescription_count = max(member_prescription_count),
    projected_refill_count_Oct04 = max(projected_refill_count_Oct04),
    projected_refill_count_Nov29 = max(projected_refill_count_Nov29)
  )

no_dup <- rbind(HRA_Pharma_clean %>%
                  filter(!Pharmacy_NPI %in% HRA_Pharma_clean[duplicated(HRA_Pharma_clean$Pharmacy_NPI),]$Pharmacy_NPI), dist_dup)

# Setting >=60 thresholds for projected reffil
no_dup <- no_dup %>% 
  arrange(desc(projected_refill_count_Nov29))


morethan60_projrefNOV29 <- no_dup %>% 
  filter(projected_refill_count_Nov29>=60)


# SIMULATION --------------------------------------------------------------
# Initializing number cores to use for parallel proceccing
numcores <- detectCores()
registerDoParallel(numcores)


# The functions below is a simulation that calculates the power of multilevel method. 
# Also included in the function is the combination of samples for treatment and control. 

powersamp <- function(data, effsize, nsims){
  prop <- seq(0.2, 0.6, 0.01)
  glmeres2 <- data.frame()
  
  for (h  in 1:length(prop)){
    n1 <- round(nrow(data) * prop[h],0)
    n2 <- nrow(data) - n1
    runs <- nsims
    p_value <- data.frame()
    
    # Parallel processing
    p_value <- foreach(l = 1:runs, .combine=rbind) %dopar% {
      # hra_count is the number of members who have an HRA gap
      # unique_member_count  is total number of members for a specific pharmacy
      
      trt_dat <- sample_n(morethan60_projrefNOV29, 10) %>%
        mutate(group = "trt") %>% 
        mutate(HRA = unique_member_count - hra_count,
               incHRA = HRA + round(projected_refill_count_Nov29*0.5, 0))
      
      ctrl_dat <- morethan60_projrefNOV29 %>%
        filter(!Pharmacy_NPI %in% trt_dat$Pharmacy_NPI) %>%
        sample_n(10) %>%
        mutate(group = "ctrl") %>% 
        mutate(HRA = unique_member_count - hra_count,
               incHRA = HRA)
    
      mod_dat <- rbind(trt_dat, ctrl_dat) %>% 
        mutate(Perc_HRA = incHRA/unique_member_count)
      
      mod <- glmer(cbind(incHRA,unique_member_count-incHRA) ~ group + (1|Pharmacy_Name),
                   data=mod_dat,family=binomial("logit"))
      
      mod_output <- data.frame(p_value = summary(mod)$coefficients[2,4], Rejection = ifelse(summary(mod)$coefficients[2,4] <= 0.05, 1,0),
                        MeanTrtInc = mean(mod_dat[mod_dat$group=="trt",]$Perc_HRA),
                        MeanCtrlInc = mean(mod_dat[mod_dat$group=="ctrl",]$Perc_HRA))
      mod_output
    }
    
    # saving the data
    mod_output <- data.frame(pwr = sum(p_value$Rejection)/runs,
                       TrtHRA = mean(p_value$MeanTrtInc),
                       CtrlHRA = mean(p_value$MeanCtrlInc),
                       n1 = n1, n2 = n2)
    glmeres2 <- rbind(glmeres2, tmp2)
    if(h%%1 == 0){
      print(h)
    }
  }
  return(glmeres2)
}


tresh60efs20 <- powersamp(morethan60_projrefNOV29, 0.2, nsims = 1000)
tresh60efs30 <- powersamp(morethan60_projrefNOV29, 0.3, nsims = 1000)
tresh60efs40 <- powersamp(morethan60_projrefNOV29, 0.4, nsims = 1000)
tresh60efs50 <- powersamp(morethan60_projrefNOV29, 0.5, nsims = 1000)
saveRDS(tresh60efs20, file = "Data/tresh60efs20_1k.rds")
saveRDS(tresh60efs30, file = "Data/tresh60efs30_1k.rds")
saveRDS(tresh60efs40, file = "Data/tresh60efs40_1k.rds")
saveRDS(tresh60efs50, file = "Data/tresh60efs50_1k.rds")

# Stop Parallel preocessing
stopImplicitCluster()


pre_data = mod_dat %>% 
  select(PH_name, group , unique_member_count, incHRA) %>% 
  mutate(noHRA = unique_member_count-incHRA) %>% 
  arrange(desc(incHRA)) %>% 
  mutate(id = seq(1,nrow(mod_dat)))

# ggplot(aes(x=reorder(OpTypeVis,-TotalVisit), TotalVisit, fill=LOB)) + 
#   geom_bar( stat = "identity", position = "stack", width = 0.5) +
#   facet_grid(~ Clinics, scales = "free_x", space = "free_x") +
#   scale_fill_discrete(name = "Line of Business") +
#   theme_bw() + 
#   theme(panel.spacing = unit(0,"lines"),
#         strip.background = element_blank(),
#         plot.title = element_text(size = 10, face = "bold", 
#                                   lineheight=1,hjust = 0), 
#         # axis.text.x = element_text( size = rel(1.1), angle = 90),
#         legend.position = "bottom", 
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank())

g1 = ggplot(data = pre_data, aes(y = incHRA, x = reorder(as.character(id), -incHRA))) + ylim(0,4200)+
  geom_bar(stat = "identity", fill = 'green') + ylab("HRA") + xlab("Pharmacy ID") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

g2 = ggplot(data = pre_data, aes(y = noHRA, x = reorder(as.character(id), -incHRA))) + ylim(0,4200)+
  geom_bar(stat = "identity", fill = 'red') + ylab("HRA") + xlab("Pharmacy ID") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

g3 = ggplot(data = pre_data, aes(y = unique_member_count , x = reorder(as.character(id), -incHRA))) + ylim(0,4200)+
  geom_bar(stat = "identity", fill = 'blue') + ylab("HRA") + xlab("Pharmacy ID") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

g4 = gridExtra::grid.arrange(g1,g2, ncol = 2)

gridExtra::grid.arrange(g4, g3, nrow = 2)



