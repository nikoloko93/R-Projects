padding <- function(pad) paste0("padding-left: ", pad, "em; padding-right: ", pad, "em;")

theme_update(text = element_text(size = 15))

theme_diri = theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

html_summary <- function (mod, names = NULL, caption = NULL, ...) 
{
  tab <- summary(mod)$coefficients
  if (is.null(caption)) 
    caption = paste("<b>Response variable:</b>", as.character(mod$call[[2]])[2])
  if (!is.null(names)) 
    row.names(tab) <- names
  colnames(tab)[4] <- "P-value"
  tab[, 1] <- round(tab[, 1], digits = 3)
  tab[, 2] <- signif(tab[, 2], digits = 4)
  tab[, 3] <- round(tab[, 3], digits = 2)
  tab[, 4] <- signif(tab[, 4], digits = 2)
  tab[, 4] <- ifelse(tab[, 4] < 1e-04, "&lt;0.0001", tab[, 
                                                         4])
  htmlTable::htmlTable(tab, caption = caption, ...)
}

html_dropdown <- function (id, divs, labels = NULL, onchange = paste0(id, "_function(value);"), onload = "") {
  if (is.null(labels)) {
    if (any(names(divs) == "")) 
      stop("Must supply labels or a named vector for divs.")
    else labels <- names(divs)
  }
  str <- paste("<script>\nfunction ", id, "_function (value) {\n", 
               paste(divs, ".hidden = true;", sep = "", collapse = "\n"), 
               "\nconsole.log(value);\n", "document.getElementById(value).hidden = false;\n}\n</script>", 
               "\n<select id = \"", id, "\" onchange = \"", onchange, 
               "\" onload = \"", onload, "\">", paste("\n<option value = ", 
                                                      divs, ">", labels, "</option>", sep = "", collapse = ""), 
               "\n</select>", "<div id = \"", id, "_disp\"></div>", 
               sep = "")
  str
}

matpaste <- function (..., sep = " ", collapse = NULL, dim = NULL) {
  lst <- list(...)
  if (is.null(dim)) 
    dim <- dim(lst[[min(which(sapply(lst, is.matrix)))]])
  mat <- matrix(paste(..., sep = sep, collapse = collapse), 
                nrow = dim[1], ncol = dim[2])
  mat
}

kableone <- function(x, ...) {
  capture.output(x <- print(x))
  knitr::kable(x, ...)
}

medianq1q3_out <- function(x, weights=rep(1, length(x)), ...){
  x = x[!is.na(x)]
  as = scores(x, prob = 0.995, type = "t")
  fd = data.frame(cbind(x,as))
  fd = fd %>% filter(as==FALSE)
  medianq1q3fd = round(medianq1q3(fd$x),2)
  return(medianq1q3fd)
}

n_outliers_999 <- function(x, weights=rep(1, length(x)), ...){
  sum(scores(na.omit(x), prob = 0.999, type = "t"))
}
mean_out_999 <- function(x, weights=rep(1, length(x)), ...){
  x = x[!is.na(x)]
  as = scores(x, prob = 0.999, type = "t")
  fd = data.frame(cbind(x,as))
  fd = fd %>% filter(as==FALSE)
  meansdfd = round(meansd(fd$x), 3)

  return(meansdfd)
}

medianq1q3_out_999 <- function(x, weights=rep(1, length(x)), ...){
  x = x[!is.na(x)]
  as = scores(x, prob = 0.999, type = "t")
  fd = data.frame(cbind(x,as))
  fd = fd %>% filter(as==FALSE)
  medianq1q3fd = round(medianq1q3(fd$x),2)
  return(medianq1q3fd)
}

range_out_999 <- function(x, weights=rep(1, length(x)), ...){
  x = x[!is.na(x)]
  as = scores(x, prob = 0.999, type = "t")
  fd = data.frame(cbind(x,as))
  fd = fd %>% filter(as==FALSE)
  maxfd = round(max(fd$x),2)
  minfd = round(min(fd$x),2)
  com = paste0(minfd, " - ", maxfd)
  return(com)
}


aovout_func_joy <- function(data,columns=NULL,strata=NULL,pertle =0.995){
  p_val_tab <- tibble()
  for(i in 1:length(columns)){
    a1 = data %>% 
      filter(Year_Mo_=="July 2017") %>% 
      select(columns[i],strata) %>% 
      drop_na()
    a1$out = scores(a1[,1], type="t", prob = pertle)
    a1 = a1 %>% filter(out==FALSE)
    a2 = data %>% 
      filter(Year_Mo_=="July 2018") %>% 
      select(columns[i],strata) %>% 
      drop_na()
    a2$out = scores(a2[,1], type="t", prob = pertle)
    a2 = a2 %>% filter(out==FALSE)
    aa = rbind(a1,a2)
    az =aov(formula(paste0(columns[i], "~", strata)), data = aa)
    az1 = summary(az)
    az2 = tibble(Var = columns[i], p_value=az1[[1]]$`Pr(>F)`[1])
    p_val_tab = rbind(p_val_tab, az2)
  }
  return(p_val_tab)
}


# Function to create new data of spending without outliers
new_pop_spend <- function(data, enroll_flag = "N") {
  
  if(enroll_flag == "Y") {
    data = data[Enroll_Flag == 1]
    
    new_pop <- wo_extreme_func(data = data, columns = c("IP_Allw_Amt", "OP_Allw_Amt", "DR_Allw_Amt","Rx_Allw_Amt",  "ER_Allw_Amt", "Total_Allw_Amt"), strata = "Year_Mo_", pertle = 0.995)
    
    new_pop_ip = data.table(new_pop[[1]])
    new_pop_op = data.table(new_pop[[2]])
    new_pop_dr = data.table(new_pop[[3]])
    new_pop_rx = data.table(new_pop[[4]])
    new_pop_er = data.table(new_pop[[5]])
    
    IP_cat = new_pop_ip[,.(IP = mean_ci(IP_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    OP_cat = new_pop_op[,.(OP = mean_ci(OP_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    DR_cat = new_pop_dr[,.(DR = mean_ci(DR_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    Rx_cat = new_pop_rx[,.(Rx = mean_ci(Rx_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    ER_cat = new_pop_er[,.(ER = mean_ci(ER_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    
    Spend_cat = cbind(IP_cat, OP_cat, DR_cat, Rx_cat, ER_cat)
    Spend_cat = Spend_cat[,.(Year_Mo_, IP, OP, DR, Rx, ER, stat)]
    Spend_cat_mean = Spend_cat[stat == "Mean"]
    Spend_cat_lower = Spend_cat[stat == "Lower"]
    Spend_cat_upper = Spend_cat[stat == "Upper"]
    Spend_cat_mean_melt = melt(Spend_cat_mean, id.vars = c('Year_Mo_'),
                               measure.vars = c('IP','OP','DR','Rx','ER'),
                               variable.name = "Category",
                               value.name = "Mean")
    Spend_cat_lower_melt = melt(Spend_cat_lower, id.vars = c('Year_Mo_'),
                                measure.vars = c('IP','OP','DR','Rx','ER'),
                                variable.name = "Category",
                                value.name = "Lower")
    Spend_cat_upper_melt = melt(Spend_cat_upper, id.vars = c('Year_Mo_'),
                                measure.vars = c('IP','OP','DR','Rx','ER'),
                                variable.name = "Category",
                                value.name = "Upper")
    setkeyv(Spend_cat_mean_melt, c("Year_Mo_", "Category"))
    setkeyv(Spend_cat_lower_melt, c("Year_Mo_", "Category"))
    setkeyv(Spend_cat_upper_melt, c("Year_Mo_", "Category"))
    Spend_cat_melt = Spend_cat_mean_melt[Spend_cat_lower_melt, nomatch = 0]
    Spend_cat_melt = Spend_cat_melt[Spend_cat_upper_melt, nomatch = 0]
    
    return(Spend_cat_melt)
    
  }
  else if(enroll_flag == "N") {
    
    new_pop <- wo_extreme_func(data = data, columns = c("IP_Allw_Amt", "OP_Allw_Amt", "DR_Allw_Amt","Rx_Allw_Amt",  "ER_Allw_Amt", "Total_Allw_Amt"), strata = "Year_Mo_", pertle = 0.995)
    
    new_pop_ip = data.table(new_pop[[1]])
    new_pop_op = data.table(new_pop[[2]])
    new_pop_dr = data.table(new_pop[[3]])
    new_pop_rx = data.table(new_pop[[4]])
    new_pop_er = data.table(new_pop[[5]])
    
    IP_cat = new_pop_ip[,.(IP = mean_ci(IP_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    OP_cat = new_pop_op[,.(OP = mean_ci(OP_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    DR_cat = new_pop_dr[,.(DR = mean_ci(DR_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    Rx_cat = new_pop_rx[,.(Rx = mean_ci(Rx_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    ER_cat = new_pop_er[,.(ER = mean_ci(ER_Allw_Amt), stat = c('Mean', 'Lower', 'Upper')), keyby = Year_Mo_]
    
    Spend_cat = cbind(IP_cat, OP_cat, DR_cat, Rx_cat, ER_cat)
    Spend_cat = Spend_cat[,.(Year_Mo_, IP, OP, DR, Rx, ER, stat)]
    Spend_cat_mean = Spend_cat[stat == "Mean"]
    Spend_cat_lower = Spend_cat[stat == "Lower"]
    Spend_cat_upper = Spend_cat[stat == "Upper"]
    Spend_cat_mean_melt = melt(Spend_cat_mean, id.vars = c('Year_Mo_'),
                               measure.vars = c('IP','OP','DR','Rx','ER'),
                               variable.name = "Category",
                               value.name = "Mean")
    Spend_cat_lower_melt = melt(Spend_cat_lower, id.vars = c('Year_Mo_'),
                                measure.vars = c('IP','OP','DR','Rx','ER'),
                                variable.name = "Category",
                                value.name = "Lower")
    Spend_cat_upper_melt = melt(Spend_cat_upper, id.vars = c('Year_Mo_'),
                                measure.vars = c('IP','OP','DR','Rx','ER'),
                                variable.name = "Category",
                                value.name = "Upper")
    setkeyv(Spend_cat_mean_melt, c("Year_Mo_", "Category"))
    setkeyv(Spend_cat_lower_melt, c("Year_Mo_", "Category"))
    setkeyv(Spend_cat_upper_melt, c("Year_Mo_", "Category"))
    Spend_cat_melt = Spend_cat_mean_melt[Spend_cat_lower_melt, nomatch = 0]
    Spend_cat_melt = Spend_cat_melt[Spend_cat_upper_melt, nomatch = 0]
  
    return(Spend_cat_melt)
  }
}
# sample
# Spend_cat_melt = new_pop_spend(data = pop, enroll_flag = "Y")

# Function to create new data of RAF scores without outliers 
new_pop_raf <- function(data, enroll_flag = "N") {
  
  if(enroll_flag == "Y") {
    data = data[Enroll_Flag == 1]
    
    tmp_pop1 = data[Population == "July 2017",.(RAF_Score, Population)]
    tmp_pop2 = data[Population == "July 2018" & !is.na(RAF_Score),.(RAF_Score, Population)]
    tmp_pop1$out <- scores(tmp_pop1[,1], type="t", prob = 0.999)
    tmp_pop2$out <- scores(tmp_pop2[,1], type="t", prob = 0.999)
    tmp_pop = rbind(tmp_pop1, tmp_pop2)
    tmp_pop = tmp_pop[out == FALSE,]
    
    return(tmp_pop)
  }
  else if(enroll_flag == "N") {
    
    
    tmp_pop1 = data[Population == "July 2017",.(RAF_Score, Population)]
    tmp_pop2 = data[Population == "July 2018" & !is.na(RAF_Score),.(RAF_Score, Population)]
    tmp_pop1$out <- scores(tmp_pop1[,1], type="t", prob = 0.999)
    tmp_pop2$out <- scores(tmp_pop2[,1], type="t", prob = 0.999)
    tmp_pop = rbind(tmp_pop1, tmp_pop2)
    tmp_pop = tmp_pop[out == FALSE,]
    
    return(tmp_pop)
  }
  
  
}
# sample 
# new_pop_raf(pop, enroll_flag = "N")

# Function to create new data without outliers and plot RAF scores
plot_out <- function(data, enroll_flag = "N") {
  
  if(enroll_flag == "Y") {
    data = data[Enroll_Flag == 1]
    title = "Among enrolled (excluding 0.1% extreme normalized values)"
    
    tmp_pop1 = data[Population == "July 2017",.(RAF_Score, Population)]
    tmp_pop2 = data[Population == "July 2018" & !is.na(RAF_Score),.(RAF_Score, Population)]
    tmp_pop1$out <- scores(tmp_pop1[,1], type="t", prob = 0.999)
    tmp_pop2$out <- scores(tmp_pop2[,1], type="t", prob = 0.999)
    tmp_pop = rbind(tmp_pop1, tmp_pop2)
    tmp_pop = tmp_pop[out == FALSE,]
    
  }
  else if(enroll_flag == "N") {
    title = "Among eligibles (excluding 0.1% extreme normalized values)"
    
    tmp_pop1 = data[Population == "July 2017",.(RAF_Score, Population)]
    tmp_pop2 = data[Population == "July 2018" & !is.na(RAF_Score),.(RAF_Score, Population)]
    tmp_pop1$out <- scores(tmp_pop1[,1], type="t", prob = 0.999)
    tmp_pop2$out <- scores(tmp_pop2[,1], type="t", prob = 0.999)
    tmp_pop = rbind(tmp_pop1, tmp_pop2)
    tmp_pop = tmp_pop[out == FALSE,]
  }
  
  ggplot(tmp_pop[out == FALSE], aes(x = RAF_Score, fill = Population)) + 
    geom_histogram(bins = 30, 
                   aes(y = stat(width*density)), 
                   position = "identity", alpha = 0.6) +  
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    ggtitle(title) + 
    ylab("Percentage") + xlab("RAF Score") +
    scale_fill_manual(values = c(uhg1, uhg2)) +
    theme_joy_single + guides(fill = guide_legend(title = NULL))
  
}

# Function to create new list of dataframes for the spending variables

wo_extreme_func <- function(data, columns = NULL, strata = NULL, pertle = 1) {
  new_data <- list()
  for(i in 1:length(columns)) {
    a1 = data %>% 
      filter(Year_Mo_ ==  "July 2017") %>% 
      select(columns[i], strata) %>% 
      drop_na()
    a1$out = scores(a1[,1], type= "t", prob = pertle)
    a1 = a1 %>% filter(out == FALSE)
    
    a2 = data %>% 
      filter(Year_Mo_ == "July 2018") %>% 
      select(columns[i], strata) %>% 
      drop_na()
    a2$out = scores(a2[,1], type = "t", prob = pertle)
    a2 = a2 %>% filter(out == FALSE)
    new_dat = rbind(a1 ,a2)
    new_dat[,2] = as.factor(new_dat[,2])
    new_data[[i]] = new_dat
  }
  names(new_data) = columns
  return(new_data)
  
}

# Function to provide effect size for numerical variables. 
# It has the options to remove extreme values or not. 
effectsize_func <- function(data, columns = NULL, strata = NULL, pertle = 1, remove_outlier = "Y") {
  effectsize_tab <- data.table()
  
  if(remove_outlier == "Y") {
    new_pop <- wo_extreme_func(data = data, columns = columns, strata = strata, 
                               pertle = pertle)
    for(i in 1:length(new_pop)) {
      
      effect_size = cohen.d(new_pop[[i]][,1], new_pop[[i]][,2], hedges.correction = TRUE, na.rm = TRUE)
      effect_size_estimate = tibble(Variable = names(new_pop[i]), 
                                    Estimate = abs(effect_size$estimate), 
                                    Magnitude = effect_size$magnitude)
      
      effectsize_tab = rbind(effectsize_tab, effect_size_estimate)
    }
    return(effectsize_tab)
  }
  
  else if(remove_outlier == "N") {
    for(i in 1:length(columns)) {
      
      a = data %>% 
        select(strata, columns[i]) %>% 
        drop_na() 
      a[,Year_Mo_ := as.factor(Year_Mo_)]
      a = data.frame(a)
      effect_size = cohen.d(a[,2], a[,1], hedges.correction = TRUE, na.rm = TRUE)
      effect_size_estimate = tibble(Variable = columns[i], 
                                    Estimate = abs(effect_size$estimate), 
                                    Magnitude = effect_size$magnitude)
      
      effectsize_tab = rbind(effectsize_tab, effect_size_estimate)
    }
    return(effectsize_tab)
  }
  
  
  
}
# sample
# effectsize_func(pop, columns = c("IP_Allw_Amt", "OP_Allw_Amt", "DR_Allw_Amt", "Rx_Allw_Amt", "ER_Allw_Amt",
#                                  "Total_Allw_Amt"), strata = "Year_Mo_")


# Function for cohen's h
cohen.h_func = function(data, factor=NULL, columns=NULL, tofactor="Y"){
  cohen.h.tib = data.table()
  for (i in 1:length(columns)){
    a = data %>% 
      select(factor,columns[i]) %>% 
      drop_na() 
    a[,Year_Mo_ := as.factor(Year_Mo_)]
    a = data.frame(a)
    factors_me = unique(a[,1])
    
    a1 = a %>% filter(Year_Mo_=="Jul 2017")
    a2 = a %>% filter(Year_Mo_=="Jul 2018")
    c1 =  nrow(a1[a1[,2]==tofactor,])
    c2 =  nrow(a2[a2[,2]==tofactor,])
    prop1 = nrow(a1[a1[,2]==tofactor,])/nrow(a1)
    prop2 = nrow(a2[a2[,2]==tofactor,])/nrow(a2)
    H = abs(ES.h(prop1, prop2))
    magnitude = case_when(abs(H)<0.20~"negligible",
                          abs(H)<0.5 ~ "small",
                          abs(H)<0.8 ~ "medium",
                          abs(H)<=1 ~ "large")
    a3 = data.frame(Variable=columns[i], t1 = paste0(c1, " (", round(prop1*100,1), "%)"),
                    t1 = paste0(c2, " (", round(prop2*100,1), "%)"), Cohen.s.H.Estimate=H, Magnitude=magnitude)
    colnames(a3)[2] = paste0(factors_me[1], " (N=", nrow(a1),")")
    colnames(a3)[3] = paste0(factors_me[2], " (N=", nrow(a2),")")
    cohen.h.tib = rbind(cohen.h.tib, a3)
  }
  return(cohen.h.tib)
  
}

# cohen.h_func(pop, factor = "Year_Mo_", columns = "T2D_Flag", tofactor = "1")



create_chronic_tab_func = function(data, year_mo_included = 1){
  
  if (year_mo_included == 1){
    chron_dat1 <- data %>% 
      filter(Year_Mo >= 201701) %>% 
      filter(Year_Mo <= 201707) %>% 
      select(Indv_Sys_ID,Year_Mo_,31:37)
  }
  else if (year_mo_included == 2){
    chron_dat1 <- data %>% 
      filter(Year_Mo >= 201801) %>% 
      select(Indv_Sys_ID,Year_Mo_,31:37)
  }
  
  chron_names = names(chron_dat1[-c(1,2)])
  chronic_table = data.frame(Indv_Sys_ID=unique(chron_dat1$Indv_Sys_ID))
  
  for (i in 1:length(chron_names)){
    a = chron_dat1 %>% 
      select(1,2,chron_names[i]) %>% 
      spread(2,3)
    a[is.na(a)] <-0
    a[2:8][a[2:8]=="Y"] = 1 
    a[2:8][a[2:8]=="N"] = 0 
    a[, c(2:8)] <- sapply(a[, c(2:8)], as.numeric)
    a$i <- ifelse(apply(a[2:8], 1, sum)>0,1,0)
    colnames(a)[9] <- chron_names[i]
    a = a[,c(1,9)]
    a[,2][a[,2]==1]= "Y"
    a[,2][a[,2]==0]= "N"
    chronic_table = left_join(chronic_table, a, by = "Indv_Sys_ID")
  }
  return(chronic_table)
}
# sample 
# conditions1 = data.table(create_chronic_tab_func(mm[Group1_Flag == 1], year_mo_included = 1))
