library(rvest)
library(tidyverse)

medURL <- "https://www.drugs.com/drug_information.html"
webpage <- read_html(medURL)

Alphabetical <- webpage %>% html_nodes(".ddc-paging") %>% html_nodes("a")
alphas <- html_text(Alphabetical)
alp <- tolower(alphas)


DrugTablesAtoZ <- data.frame()



for (i in 1:length(alp)){
  urlalp <- paste0("https://www.drugs.com/alpha/", alp[i], ".html")
  alpwebpage <- read_html(urlalp)
  tmpnodes <- html_nodes(alpwebpage, ".ddc-paging")
  tmpnodes <- tmpnodes[[1]] %>% 
    html_nodes("a") 
  tmpchar <- html_text(tmpnodes)
  tmpchar <- tolower(tmpchar)
  for (j in 1:length(tmpchar)){
    urltmp <- paste0("https://www.drugs.com/alpha/", tmpchar[j], ".html")
    if (tmpchar[j] == "0-9"){
      urltmp <- paste0("https://www.drugs.com/alpha/", alp[i], tmpchar[j], ".html")
    }
    tmpwebpage <- read_html(urltmp)
    tmpnodes1 <- tmpwebpage %>% html_nodes(".ddc-list-column-2") %>% html_nodes("a")
    tmpnodes2 <- tmpwebpage %>% html_nodes(".ddc-list-column-2") %>% html_nodes("a") %>% 
      html_attr("href") %>% as.character()
    if (rlang::is_empty(as.character(tmpnodes1))){
      tmpnodes1 <- tmpwebpage %>% html_nodes(".ddc-list-unstyled")
      tmpnodes1 <- tmpnodes1[1] %>% html_nodes("a")
      tmpnodes2 <- tmpwebpage %>% html_nodes(".ddc-list-unstyled")
      tmpnodes2 <- tmpnodes2[1] %>% html_nodes("a") %>% 
        html_attr("href") %>% as.character()
    }
    druglist <- data.frame(drugs = html_text(tmpnodes1), href = tmpnodes2)
    drug_gen <- data.frame()
    # DrugTablesAtoZ <- rbind(DrugTablesAtoZ,druglist)
    for(k in 1:nrow(druglist)){
      if(k%%1 == 0) cat(k, sep="\n")
      urldruggen <- paste0("https://www.drugs.com", druglist[k,2])
      webpagedruggen <- read_html(urldruggen)
      nodesgnrc <- webpagedruggen %>% html_node(".drug-subtitle")
      generic <- html_text(nodesgnrc)
      generic <- strsplit(strsplit(generic, split = "Brand|Dosage")[[1]][1], split = "Generic Name: ")[[1]][2]
      generic <- strsplit(generic, split = " \\(")[[1]][1]
      
      try(if (is.na(generic)){
        nodesdruggen <- webpagedruggen %>% html_nodes(".vmig")
        nodesdruggen <- nodesdruggen[[2]] %>% html_nodes("a") %>% 
          html_attr("href") %>% as.character()
        urlgnrc <- paste0("https://www.drugs.com", nodesdruggen[1])
        webpagegnrc <- read_html(urlgnrc)
        nodesgnrc <- webpagegnrc %>% html_nodes(".drug-subtitle")
        generic <- html_text(nodesgnrc)
        generic <- strsplit(strsplit(generic, split = "Brand|Dosage")[[1]][1], split = "Generic Name: ")[[1]][2]
        generic <- strsplit(generic, split = " \\(")[[1]][1]
        # generic
      })
      if (rlang::is_empty(generic)){
        drug_gen <- rbind(drug_gen,data.frame(Drugname = druglist[k,1], GenericName = NA, 
                                              Link = paste0("https://www.drugs.com", druglist[k,2])))
      }else{
        drug_gen <- rbind(drug_gen,data.frame(Drugname = druglist[k,1], GenericName = generic,
                                              Link = paste0("https://www.drugs.com", druglist[k,2])))
      }
      
    }
    DrugTablesAtoZ <- rbind(DrugTablesAtoZ,drug_gen)
  }
}


# saveRDS(DrugTablesAtoZ, file = "DrugTableAtoZ.rds")

CleanDrugTableAtoZ <- DrugTablesAtoZ %>% filter(!is.na(GenericName))
NAGenericDat <- DrugTablesAtoZ %>% filter(is.na(GenericName))


NAGeneric <- data.frame()

for (t in 1:nrow(NAGenericDat)){
  if(t%%1 == 0) cat(t, sep="\n")
  nagenericURL <- varhandle::unfactor(NAGenericDat[t,3])
  wpNAgenericURL <- read_html(nagenericURL)
  nodesNAgen <- wpNAgenericURL %>% html_nodes("p")
  generic <- html_text(nodesNAgen[[1]])
  
  try(generic <- strsplit(strsplit(generic, split = "\n|Brand")[[1]][1], split = "Generic Name: ")[[1]][2])

  NAGeneric <- rbind(NAGeneric, data.frame(Drugname = NAGenericDat[t,1], GenericName = generic,
                                         Link = NAGenericDat[t,3]))
}

tempgen <- NAGeneric %>% filter(!is.na(GenericName))
CleanDrugTableAtoZ <- rbind(CleanDrugTableAtoZ, tempgen)

saveRDS(CleanDrugTableAtoZ, file = "CleanDrugTableAtoZ.rds")


