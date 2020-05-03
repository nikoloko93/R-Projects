library(rvest)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

# GETTING INTO THE PAGE
WebMD <- "https://www.webmd.com/drugs/2/index"
readWebMD <- read_html(WebMD)

# GETTING LIST OF LETTERS
alpha <- readWebMD %>% 
  html_nodes(".drugs-browse-box") %>%
  html_nodes("a")  

alpha <- html_text(alpha)[-1] # REMOVING Find Off-Market Drugs

# GETTING THE LINKS OF EVERY LETTERS
linkAlpha <- readWebMD %>% 
  html_nodes(".drugs-browse-box") %>% html_nodes("a") %>% 
  html_attr("href")

linkAlpha <- paste0("https://www.webmd.com", as.character(linkAlpha[-1]))

# GETTING ALL SUBLETTERS WITH LINKS
linkSubAlpha <- data.frame()
for(i in 1:length(linkAlpha)){
  if (i == length(linkAlpha)){
    tmp_dat <- data.frame(SubLetter = "0-9", Link = linkAlpha[i])
    linkSubAlpha <- rbind(linkSubAlpha, tmp_dat)
  }
  else{
    webpage <- linkAlpha[i]
    readwebpage <- read_html(webpage)
    
    subalp <- readwebpage %>% html_nodes(".drugs-browse-subbox") %>% html_nodes("a") %>% html_text()
    link <- readwebpage %>% html_nodes(".drugs-browse-subbox") %>% html_nodes("a") %>% html_attr("href")
    nolink <- readwebpage %>% html_nodes(".drugs-browse-subbox") %>% html_nodes("a") %>% html_attr("class")
    
    tmp_dat <- data.frame(SubLetter = subalp, Link = paste0("https://www.webmd.com",link), noLink = nolink)
    tmp_dat <- tmp_dat %>% 
      mutate(noLink = ifelse(is.na(noLink), "a", noLink)) %>% 
      filter(noLink != "2")
    tmp_dat <- tmp_dat[,-3]
    linkSubAlpha <- rbind(linkSubAlpha, tmp_dat)
    
  }
  if(i%%1 == 0){
    print(i)
  }
}

# GETTING ALL THE DRUG NAMES WITH LINKS
druglist <- data.frame()
for (j in 1:nrow(linkSubAlpha)){
  webpage <- varhandle::unfactor(linkSubAlpha[j,2]) 
  readwebpage <- read_html(webpage)
  
  drug_tmp <- readwebpage %>% html_nodes(".drug-list-container") %>% html_nodes("a") %>% html_text()
  druglink_tmp <- paste0("https://www.webmd.com", readwebpage %>% html_nodes(".drug-list-container") %>% html_nodes("a") %>% html_attr("href"))
  tmp_dat <- data.frame(SubLetter = linkSubAlpha[j,1], DrugName = drug_tmp, DrugLink = druglink_tmp)
  druglist <- rbind(druglist, tmp_dat)
  
  if(j%%1 == 0){
    print(j)
  }
}

# CHECKING DUPLICATES
tmp_2 <- druglist %>%
  filter(DrugName %in% druglist[duplicated(druglist$DrugName),]$DrugName)

# GETTING THE GENERIC NAME OF DRUGS
WebMDDrugstoGen <- data.frame()

for(l in 1:nrow(druglist)){
  webpage <- varhandle::unfactor(druglist[l,3])
  readwebpage <- read_html(webpage)
  
  druGen_tmp <- readwebpage %>% html_nodes(".drug-information") %>% html_nodes("p") %>% html_text()
  if (rlang::is_empty(druGen_tmp)){
    tmp_dat <- cbind(druglist[l,], Generic = NA)
    WebMDDrugstoGen <- rbind(WebMDDrugstoGen, tmp_dat)
  }else{
    generic <- gsub("^\\s+", "", strsplit(druGen_tmp[grepl("GENERIC", druGen_tmp)], split = ":")[[1]][2])
    tmp_dat <- cbind(druglist[l,], Generic = generic)
    WebMDDrugstoGen <- rbind(WebMDDrugstoGen, tmp_dat)
  }
  
}

# Parallel Computation ----------------------------------------------------


numcores <- detectCores()
registerDoParallel(numcores)

system.time({
  WebMDDrugstoGen <- foreach(l = 1:18225, .combine=rbind) %dopar% {
    webpage <- varhandle::unfactor(druglist[l,3])
    readwebpage <- read_html(webpage)
    
    druGen_tmp <- readwebpage %>% html_nodes(".drug-information") %>% html_nodes("p") %>% html_text()
    if (rlang::is_empty(druGen_tmp)){
      tmp_dat <- cbind(druglist[l,], Generic = NA)
    }else{
      generic <- gsub("^\\s+", "", strsplit(druGen_tmp[grepl("GENERIC", druGen_tmp)], split = ":")[[1]][2])
      tmp_dat <- cbind(druglist[l,], Generic = generic)
    }
  }
})

tmp_1 <- data.frame()
tmp_1 <- rbind(tmp_1, WebMDDrugstoGen)
stopImplicitCluster()
