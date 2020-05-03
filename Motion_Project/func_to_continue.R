# colSums(is.na(subset(df1, str_detect(Year_mo_str, "Jul 2018")))) #count nas


# df1a %>%
#   group_by(Year_Mo_Motion_Enr, Indv_Sys_ID) %>%
#   summarise(n=n()) %>%
#   group_by(Year_Mo_Motion_Enr) %>%
#   summarise(n=n()) %>% data.frame()

# demogs_vars <- names(df1a[c(7,8,10,17:19)])
# # creating table function
# create_table <- function(data, strata, variables){
#   varnames <- tibble(Varname=names(data), ind=seq(names(data)))
#   data1 <- deparse(substitute(data))
#   arg1 <- paste0(data1, "$", strata)
#   stratum <- unique(eval(parse(text = arg1)))
#   str_ind = filter(varnames, Varname==strata )[,2][[1]]
#   dat_me <- tibble()
#
#   for (i in 1:length(variables)){
#     var_ind = filter(varnames, Varname==variables[i])[,2][[1]]
#     if (class(data[,var_ind]) %in% c("integer","numeric")){
#       m1 =round(mean(data[which(data[str_ind]==stratum[1]),var_ind], na.rm = TRUE),3)
#       m2 =round(mean(data[which(data[str_ind]==stratum[2]),var_ind], na.rm = TRUE),3)
#       s1 =round(sd(data[which(data[str_ind]==stratum[1]),var_ind], na.rm = TRUE),3)
#       s2 =round(sd(data[which(data[str_ind]==stratum[2]),var_ind], na.rm = TRUE),3)
#
#       dfa <- tibble(Variables=variables[i],` ` = "", v1=paste0(m1, " (", s1, ")"),v2=paste0(m2, " (", s2, ")"))
#       dat_me <- rbind(dat_me, dfa)
#     }
#     else if (class(data[,var_ind]) %in% c("factor", "ordered", "logical", "character",
# 			"labelled")){
#
#     }
#   }
#   colnames(dat_me)[3] <- paste(as.character(stratum[1]), "(n =", length(data[which(data[str_ind]==stratum[1]),strata]), ")")
#   colnames(dat_me)[4] <- paste(as.character(stratum[2]), "(n =", length(data[which(data[str_ind]==stratum[2]),strata]), ")")
# 	return(dat_me)
# }
#
#
#
# create_table(data = df1a, strata = "Groups", variables = c("Age", "Total_Allw_Amt"))
