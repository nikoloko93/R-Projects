library(rvest)
library(foreach)
library(doParallel)

companies = data.frame(company = tolower(unique(company_data$company_name))) %>% 
  mutate(company2 = gsub(" [[:punct:]] |[[:punct:]]", " ", company)) %>% 
  mutate(nchar=nchar(company2)) %>% 
  mutate(company2 = gsub("[^\x01-\x7F]+", "", company2)) %>% 
  mutate(company2 = gsub(" ", "-", company2))


# Set URL
baseurl <- "https://www.glassdoor.com/Reviews/"
company <- "govstrive"
rev <- paste0("-reviews-SRCH_KE0,",9,".htm")

             
            
             
             
             
             
             
# How many total number of reviews? It will determine the maximum page results to iterate over.
totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
  html_nodes(".info, .h2") %>% 
  html_attr("href") %>% as.data.frame() %>% 
  filter(grepl("/Overview", .)) %>% 
  slice(1)

comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
  html_nodes(".info, .h2") %>% 
  html_text()
comp_name = trimws(comp_name[2])

url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
  

info = read_html(url2) %>% 
  html_nodes(".infoEntity, .value") %>% 
  html_text() %>% trimws()

info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
glassdoor = data.frame(comp_name = search_name = comp_name, 
                       Website = trimws(gsub("Website","",info[grepl("Website", info)])),
                       Headquarters = trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])),
                       Size = trimws(gsub("Size","",info[grepl("Size", info)])), 
                       Founded = trimws(gsub("Founded","",info[grepl("Founded", info)])),
                       Type = trimws(gsub("Type","",info[grepl("Type", info)])),
                       Industry = trimws(gsub("Industry","",info[grepl("Industry", info)])),
                       Revenue = trimws(gsub("Revenue","",info[grepl("Revenue", info)])))



for (l in 1:40){
  baseurl <- "https://www.glassdoor.com/Reviews/"
  company <- companies[l,1]
  rev <- paste0("-reviews-SRCH_KE0,",companies[l,2],".htm")
  
  totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
    html_nodes(".info, .h2") %>% 
    html_attr("href") %>% as.data.frame() %>% 
    filter(grepl("/Overview", .)) %>% 
    slice(1)
  
  if(nrow(totalreviews)==0){
    glassdoor = data.frame(comp_name = company, search_name = NA, 
                           Website = NA,
                           Headquarters = NA,
                           Size = NA, 
                           Founded = NA,
                           Type = NA,
                           Industry = NA,
                           Revenue = NA)
  }else{
    comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
      html_nodes(".info , .margBotXs") %>% 
      html_text()
    comp_name = trimws(comp_name[2])
    
    url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
    
    
    info = read_html(url2) %>% 
      html_nodes(".infoEntity, .value") %>% 
      html_text() %>% trimws()
    
    #info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
    # website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
    Website = try(trimws(gsub("Website","",info[grepl("Website", info)])))
    Headquarters = try(trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])))
    Size = try(trimws(gsub("Size","",info[grepl("Size", info)])))
    Founded = try(trimws(gsub("Founded","",info[grepl("Founded", info)])))
    Type = try(trimws(gsub("Type","",info[grepl("Type", info)])))
    Industry = try(trimws(gsub("Industry","",info[grepl("Industry", info)])))
    Revenue = try(trimws(gsub("Revenue","",info[grepl("Revenue", info)])))
    
    glassdoor = data.frame(comp_name = company, search_name = comp_name, 
                           Website = ifelse(length(Website)==0, NA, Website),
                           Headquarters = ifelse(length(Headquarters)==0, NA, Headquarters),
                           Size = ifelse(length(Size)==0, NA, Size), 
                           Founded = ifelse(length(Founded)==0, NA, Founded),
                           Type = ifelse(length(Type)==0, NA, Type),
                           Industry = ifelse(length(Industry)==0, NA, Industry),
                           Revenue = ifelse(length(Revenue)==0, NA, Revenue))
  }
  
  
  glass_data = rbind(glass_data, glassdoor)
  
}

numcores = detectCores()
registerDoParallel(numcores)

glass_data = data.frame()
unknown = foreach( l = 4001:4635, .combine = rbind) %dopar% {
  library(tidyverse)
  library(rvest)
  baseurl <- "https://www.glassdoor.com/Reviews/"
  company <- companies[l,1]
  rev <- paste0("-reviews-SRCH_KE0,",companies[l,2],".htm")
  
  totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
    html_nodes(".info, .h2") %>% 
    html_attr("href") %>% as.data.frame() %>% 
    filter(grepl("/Overview", .)) %>% 
    slice(1)
  
  if(nrow(totalreviews)==0){
    glassdoor = data.frame(comp_name = company, search_name = NA, 
                           Website = NA,
                           Headquarters = NA,
                           Size = NA, 
                           Founded = NA,
                           Type = NA,
                           Industry = NA,
                           Revenue = NA)
  }else{
    comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
      html_nodes(".info , .margBotXs") %>% 
      html_text()
    comp_name = trimws(comp_name[2])
    
    url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
    
    
    info = read_html(url2) %>% 
      html_nodes(".infoEntity, .value") %>% 
      html_text() %>% trimws()
    
    #info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
    # website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
    Website = try(trimws(gsub("Website","",info[grepl("Website", info)])))
    Headquarters = try(trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])))
    Size = try(trimws(gsub("Size","",info[grepl("Size", info)])))
    Founded = try(trimws(gsub("Founded","",info[grepl("Founded", info)])))
    Type = try(trimws(gsub("Type","",info[grepl("Type", info)])))
    Industry = try(trimws(gsub("Industry","",info[grepl("Industry", info)])))
    Revenue = try(trimws(gsub("Revenue","",info[grepl("Revenue", info)])))
    
    glassdoor = data.frame(comp_name = company, search_name = comp_name, 
                           Website = ifelse(length(Website)==0, NA, Website),
                           Headquarters = ifelse(length(Headquarters)==0, NA, Headquarters),
                           Size = ifelse(length(Size)==0, NA, Size), 
                           Founded = ifelse(length(Founded)==0, NA, Founded),
                           Type = ifelse(length(Type)==0, NA, Type),
                           Industry = ifelse(length(Industry)==0, NA, Industry),
                           Revenue = ifelse(length(Revenue)==0, NA, Revenue))
  }
  
}

glass_data = rbind(glass_data, unknown)



gg = glass_data %>% filter(is.na(Industry))
companies = companies %>% filter(company %in% gg$comp_name)

glass_data2 = data.frame()
unknown = foreach( l = 101:1606, .combine = rbind) %dopar% {
  library(tidyverse)
  library(rvest)
  baseurl <- "https://www.glassdoor.com/Reviews/"
  company <- companies[l,1]
  rev <- paste0("-reviews-SRCH_KE0,",companies[l,2],".htm")
  
  totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
    html_nodes(".info, .h2") %>% 
    html_attr("href") %>% as.data.frame() %>% 
    filter(grepl("/Overview", .)) %>% 
    slice(1)
  
  if(nrow(totalreviews)==0){
    glassdoor = data.frame(comp_name = company, search_name = NA, 
                           Website = NA,
                           Headquarters = NA,
                           Size = NA, 
                           Founded = NA,
                           Type = NA,
                           Industry = NA,
                           Revenue = NA)
  }else{
    comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
      html_nodes(".info , .margBotXs") %>% 
      html_text()
    comp_name = trimws(comp_name[2])
    
    url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
    
    
    info = read_html(url2) %>% 
      html_nodes(".infoEntity, .value") %>% 
      html_text() %>% trimws()
    
    #info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
    # website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
    Website = try(trimws(gsub("Website","",info[grepl("Website", info)])))
    Headquarters = try(trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])))
    Size = try(trimws(gsub("Size","",info[grepl("Size", info)])))
    Founded = try(trimws(gsub("Founded","",info[grepl("Founded", info)])))
    Type = try(trimws(gsub("Type","",info[grepl("Type", info)])))
    Industry = try(trimws(gsub("Industry","",info[grepl("Industry", info)])))
    Revenue = try(trimws(gsub("Revenue","",info[grepl("Revenue", info)])))
    
    glassdoor = data.frame(comp_name = company, search_name = comp_name, 
                           Website = ifelse(length(Website)==0, NA, Website),
                           Headquarters = ifelse(length(Headquarters)==0, NA, Headquarters),
                           Size = ifelse(length(Size)==0, NA, Size), 
                           Founded = ifelse(length(Founded)==0, NA, Founded),
                           Type = ifelse(length(Type)==0, NA, Type),
                           Industry = ifelse(length(Industry)==0, NA, Industry),
                           Revenue = ifelse(length(Revenue)==0, NA, Revenue))
  }
  
}

glass_data2 = rbind(glass_data2, unknown)

gg = glass_data2 %>% filter(is.na(Industry))
companies = companies %>% filter(company %in% gg$comp_name)

glass_data3 = data.frame()
unknown = foreach( l = 1:918, .combine = rbind) %dopar% {
  library(tidyverse)
  library(rvest)
  baseurl <- "https://www.glassdoor.com/Reviews/"
  company <- companies[l,1]
  rev <- paste0("-reviews-SRCH_KE0,",companies[l,2],".htm")
  
  totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
    html_nodes(".info, .h2") %>% 
    html_attr("href") %>% as.data.frame() %>% 
    filter(grepl("/Overview", .)) %>% 
    slice(1)
  
  if(nrow(totalreviews)==0){
    glassdoor = data.frame(comp_name = company, search_name = NA, 
                           Website = NA,
                           Headquarters = NA,
                           Size = NA, 
                           Founded = NA,
                           Type = NA,
                           Industry = NA,
                           Revenue = NA)
  }else{
    comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
      html_nodes(".info , .margBotXs") %>% 
      html_text()
    comp_name = trimws(comp_name[2])
    
    url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
    
    
    info = read_html(url2) %>% 
      html_nodes(".infoEntity, .value") %>% 
      html_text() %>% trimws()
    
    #info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
    # website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
    Website = try(trimws(gsub("Website","",info[grepl("Website", info)])))
    Headquarters = try(trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])))
    Size = try(trimws(gsub("Size","",info[grepl("Size", info)])))
    Founded = try(trimws(gsub("Founded","",info[grepl("Founded", info)])))
    Type = try(trimws(gsub("Type","",info[grepl("Type", info)])))
    Industry = try(trimws(gsub("Industry","",info[grepl("Industry", info)])))
    Revenue = try(trimws(gsub("Revenue","",info[grepl("Revenue", info)])))
    
    glassdoor = data.frame(comp_name = company, search_name = comp_name, 
                           Website = ifelse(length(Website)==0, NA, Website),
                           Headquarters = ifelse(length(Headquarters)==0, NA, Headquarters),
                           Size = ifelse(length(Size)==0, NA, Size), 
                           Founded = ifelse(length(Founded)==0, NA, Founded),
                           Type = ifelse(length(Type)==0, NA, Type),
                           Industry = ifelse(length(Industry)==0, NA, Industry),
                           Revenue = ifelse(length(Revenue)==0, NA, Revenue))
  }
  
}

glass_data3 = rbind(glass_data3, unknown)

gg = glass_data3 %>% filter(is.na(Industry))
companies = companies %>% filter(company %in% gg$comp_name)

glass_data4 = data.frame()
unknown = foreach( l = 1:742, .combine = rbind) %dopar% {
  library(tidyverse)
  library(rvest)
  baseurl <- "https://www.glassdoor.com/Reviews/"
  company <- companies[l,1]
  rev <- paste0("-reviews-SRCH_KE0,",companies[l,2],".htm")
  
  totalreviews <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
    html_nodes(".info, .h2") %>% 
    html_attr("href") %>% as.data.frame() %>% 
    filter(grepl("/Overview", .)) %>% 
    slice(1)
  
  if(nrow(totalreviews)==0){
    glassdoor = data.frame(comp_name = company, search_name = NA, 
                           Website = NA,
                           Headquarters = NA,
                           Size = NA, 
                           Founded = NA,
                           Type = NA,
                           Industry = NA,
                           Revenue = NA)
  }else{
    comp_name <- read_html(paste0(baseurl, company, rev, sep="")) %>% 
      html_nodes(".info , .margBotXs") %>% 
      html_text()
    comp_name = trimws(comp_name[2])
    
    url2 = paste0("https://www.glassdoor.com", totalreviews[1,1][[1]])
    
    
    info = read_html(url2) %>% 
      html_nodes(".infoEntity, .value") %>% 
      html_text() %>% trimws()
    
    #info[grepl("Website|Headquarters|Size|Founded|Type|Industry|Revenue", info)] 
    # website = trimws(gsub("Revenue","",info[grepl("Revenue", info)]))
    Website = try(trimws(gsub("Website","",info[grepl("Website", info)])))
    Headquarters = try(trimws(gsub("Headquarters","",info[grepl("Headquarters", info)])))
    Size = try(trimws(gsub("Size","",info[grepl("Size", info)])))
    Founded = try(trimws(gsub("Founded","",info[grepl("Founded", info)])))
    Type = try(trimws(gsub("Type","",info[grepl("Type", info)])))
    Industry = try(trimws(gsub("Industry","",info[grepl("Industry", info)])))
    Revenue = try(trimws(gsub("Revenue","",info[grepl("Revenue", info)])))
    
    glassdoor = data.frame(comp_name = company, search_name = comp_name, 
                           Website = ifelse(length(Website)==0, NA, Website),
                           Headquarters = ifelse(length(Headquarters)==0, NA, Headquarters),
                           Size = ifelse(length(Size)==0, NA, Size), 
                           Founded = ifelse(length(Founded)==0, NA, Founded),
                           Type = ifelse(length(Type)==0, NA, Type),
                           Industry = ifelse(length(Industry)==0, NA, Industry),
                           Revenue = ifelse(length(Revenue)==0, NA, Revenue))
  }
  
}

glass_data4 = rbind(glass_data4, unknown)


glass_data4 = glass_data4 %>% filter(!is.na(Industry))
glass_data3 = glass_data3 %>% filter(!is.na(Industry))
glass_data2 = glass_data2 %>% filter(!is.na(Industry))
glass_data = glass_data %>% filter(!is.na(Industry))

glassdoor_data = rbind(glass_data, glass_data2)
glassdoor_data = rbind(glassdoor_data, glass_data3)
glassdoor_data = rbind(glassdoor_data, glass_data4)

glassdoor_data = left_join(glassdoor_data, companies[,1:2], by = c("comp_name"="company2"))


init_url = "https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1.htm"

company = read_html(init_url) %>% 
  html_nodes(".tightAll.h2") %>% 
  html_text() 

links = read_html(init_url) %>% 
  html_nodes(".tightAll.h2") %>% 
  html_attr("href")

glass_data = data.frame(company = trimws(company), links = links)



for(i in 6:20){
  init_url = paste0("https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1_IP", i, ".htm")
  
  company = read_html(init_url) %>% 
    html_nodes(".tightAll.h2") %>% 
    html_text() 
  while(is_empty(company)){
    company = read_html(init_url) %>% 
      html_nodes(".tightAll.h2") %>% 
      html_text()
  }
  
  links = read_html(init_url) %>% 
    html_nodes(".tightAll.h2") %>% 
    html_attr("href")
  while(is_empty(links)){
    links = read_html(init_url) %>% 
      html_nodes(".tightAll.h2") %>% 
      html_attr("href")
  }
  
  glass = data.frame(company = trimws(company), links = links)
  glass_data = rbind(glass_data, glass)
}

