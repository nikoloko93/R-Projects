library(tidyverse)

##-------------------------------- PERMUTATION TEST ----------------------------------#

# creating function for randomization test
# can be used in simple arithmetic mean

permu.test.unweighted <- function(data, var_of_inter, group.me, iter=1000){
  library(mosaic)
  #calculate the observed difference in means
  observed <- mean(var_of_inter~group.me, data=data) %>% diff()
  # simulating fake data using shuffle function in mosaic package
  
  # create a randomization distribution
  # do loop
  data_null <- do(iter)*mean(var_of_inter~shuffle(group.me),data=data) %>% diff()
  
  # visualize the null data
  fig <- ggplot(data=data_null, aes(x=data_null[,1], y=..density..))+
    geom_histogram()+
    xlab("mean difference")+
    geom_vline(xintercept = observed, linetype=2, colour="blue")
  
  p_value <- prop(~ data_null[,1]<=observed, data=data)
  
  list(plot=fig, mean = observed, pvalue=p_value)
  
}

# since observations are not in equal sizes, 
# we create randomizartion test for difference in weightred means
# values should be enter in string format

permu.test.weighted <- function(data,voi,group.me, weight.me, iter=1000){
  # eval & parse can perform arguments in string format
  library(mosaic)
  arg1 <- paste0(data,"$", group.me)
  arg2 <- paste0(data,"$", weight.me)
  arg3 <- paste0(data,"$", voi)
  group.out <- unique(eval(parse(text = arg1)))
  arg4 <- paste0(data,"[eval(parse(text = arg1))==group.out[2]",",]$",voi)
  arg5 <- paste0(data,"[eval(parse(text = arg1))==group.out[1]",",]$",voi)
  arg6 <- paste0(data,"[eval(parse(text = arg1))==group.out[2]",",]$",weight.me)
  arg7 <- paste0(data,"[eval(parse(text = arg1))==group.out[1]",",]$",weight.me)
  observed_means <- weighted.mean(eval(parse(text = arg4)),eval(parse(text=arg6))) -
    weighted.mean(eval(parse(text = arg5)),eval(parse(text=arg7)))
  
  # null data
  null.test.stat <- rep(0, iter)
  for (i in 1:iter){
    arg8 <- paste0(data, "$new_group <- shuffle(", arg1, ")")
    eval(parse(text = arg8))
    arg9 <- paste0(data,"$new_group")
    group.out <- unique(eval(parse(text = arg9)))
    arg4 <- paste0(data,"[eval(parse(text = arg9))==group.out[2]",",]$",voi)
    arg5 <- paste0(data,"[eval(parse(text = arg9))==group.out[1]",",]$",voi)
    arg6 <- paste0(data,"[eval(parse(text = arg9))==group.out[2]",",]$",weight.me)
    arg7 <- paste0(data,"[eval(parse(text = arg9))==group.out[1]",",]$",weight.me)
    null.test.stat[i]<- weighted.mean(eval(parse(text = arg4)),eval(parse(text=arg6))) -
      weighted.mean(eval(parse(text = arg5)),eval(parse(text=arg7)))
  }
  data_null <- data.frame(a=as.numeric(null.test.stat))
  fig <- ggplot(data=data_null, aes(x=a, y=..density..))+
    geom_histogram()+
    xlab("mean difference")+
    geom_vline(xintercept = observed_means, linetype=2, colour="blue")
  # 
  p_value <- sum(data_null$a <=-abs(observed_means)|data_null$a >=abs(observed_means))/iter
  
  
  list(mean=observed_means, pvalue=p_value, plot=fig)
}

##-------------------------------- BOOTSTRAPPING ----------------------------------#
diff_fn <- function(data, index) {
  data <- data[index, ]
  
  diff <- mean(data$post_mean) - mean(data$pre_mean)
  return(diff)
}
diff_wt <- function(data,  index) {
  data <- data[index,  ]
  
  diff <- weighted.mean(data$post_mean, data$post_weight) - weighted.mean(data$pre_mean, data$pre_weight)
  return(diff)
}

'%not_in%' <- function(x, y)!('%in%'(x, y))

round_up <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]] - 100
}