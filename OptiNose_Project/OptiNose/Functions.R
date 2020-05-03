blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
plot_bar = function(data, x, y, fill, stats = "identity", ylab = NULL, xlab = NULL,
                    title = NULL, legend.title=NULL, plot.size=16,
                    width=1, lab.size=5, legend.pos="bottom"){
  x = enquo(x)
  y = enquo(y)
  fill = enquo(fill)

  fig1 =ggplot(data, aes(x=!!x, y=!!y, fill = !!fill)) +
    geom_bar(stat = stats , position = "dodge", width = width) +
    ylab(ylab) + xlab(xlab) + ggtitle(title)  +
    guides(fill=guide_legend(title = legend.title)) +
    theme(axis.text.x=element_text(size = 10, face = "bold"),
          # axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = plot.size, face = "bold"),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          legend.position = legend.pos) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1))

  return(fig1)
}
# plot_pie(prop1, x="", y=n,fill = Gender, label_map = c(5364,1540))

plot_hist <- function(datax, xs, y, title=NULL, xlab=NULL, ylab=NULL, binwidth = NULL,
                      by=NULL, alpha=0.5, fill=NULL, color=NULL, legend.title=NULL,
                      legend.position=NULL ,plot.size=10){
  xs = enquo(xs)
  y = enquo(y)
  fill = enquo(fill)
  color = enquo(color)
  ggplot(data = datax, aes(x=!!xs, fill=!!fill, alpha=alpha))+
    geom_histogram(aes(y=stat(width*density)), bins = binwidth,position = "identity",na.rm = TRUE)+
    ggtitle(title)+ ylab(ylab) + xlab(xlab) +
    guides(alpha=FALSE, color=FALSE, fill=guide_legend(title =legend.title))+
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme(axis.text.x=element_text(size = 10, face = "bold"),
          # axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = plot.size, face = "bold"),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
}
library(magrittr)

add_title <- function(g, title, padding = unit(2,"mm"), lpos=1, ...){
  tg <- grid::textGrob(title, ...)
  g %>%
    gtable::gtable_add_rows(heights = grobHeight(tg) + padding, pos = 0L) %>%
    gtable::gtable_add_grob(tg, t=1,b=1,l=lpos,r=ncol(g))

}
prettyround = function(x, digits=3){
  if (x<=0.001){
    y = "<0.001"
  }
  else{
    y = round(x, digits = digits)
  }
  return(y)
}
# functions that will create table
createtableanalysis = function(data,variable, strata, main.cat=NULL, varcat.totest=NULL){
  locvar = grep(variable, names(data))
  locstrata = grep(strata, names(data))
  vars <-  rlang::parse_expr(variable)
  groups <-  rlang::parse_expr(strata)

  g1 = unfactor(unique(data[,locstrata]))
  g1 = g1[g1!=main.cat]

  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1)),
    colhead = list(fg_params=list(cex = 1)),
    rowhead = list(fg_params=list(cex = 1)))

  if (class(data[,locvar]) %in% c("character", "factor")){
    # getting the first category
    t0 = list()
    if (is.null(varcat.totest)){
      v1 = unique(data[,locvar])[1]
    }
    else{
      v1 = varcat.totest
    }
    for(i in 1:length(g1)){
      dat0 = data %>%
        filter(!!groups %in% c(main.cat, g1[i])) %>%
        group_by(!!groups) %>%
        summarise(n=n())
      dat1 = data %>%
        filter(!!groups %in% c(main.cat, g1[i])) %>%
        select(!!vars, !!groups) %>%
        group_by(!!groups,!!vars) %>%
        summarise(cnt=n())
      dat2 = left_join(dat1, dat0)
      dat2 = dat2 %>%
        filter(!!vars==v1)
      a = prop.test(x=dat2$cnt, n=dat2$n, correct = TRUE)
      p_val = round(a$p.value, 3)

      tib1 = data.frame(v1=paste0(dat2[1,3], " (", percent(a$estimate[[1]]),
                                  ")"), v2 =paste0(dat2[2,3],
                                                   " (", percent(a$estimate[[2]]),")") ,`p-value`=prettyround(p_val))
      colnames(tib1)[1] = unfactor(dat2[1,1][[1]])
      colnames(tib1)[2] = unfactor(dat2[2,1][[1]])
      t0[[i]] = tableGrob(tib1, rows = NULL, theme = mytheme) %>%
        add_title(paste0("Difference in Proportion \n(",variable," " ,v1, ")"), gp=gpar(fontsize=12, fontface=2),
                  hjust=0, x=0, lpos=1)
    }
    return(t0)
  }
  else if (class(data[,locvar]) %in% c("numeric", "integer")) {
    t0 = list()
    for (i in 1:length(g1)){
      dat1 = data %>%
        filter(!!groups %in% c(main.cat, g1[i])) %>%
        drop_na()
      dat2 = dat1 %>%
        group_by(!!groups) %>%
        summarise(MEANSD=paste0(round(mean(!!vars),2), " (", round(sd(!!vars),2), ")" ))
      ano = aov(formula(paste0(variable, "~", strata)), data = dat1)
      az = summary(ano)
      tib1 = data.frame(v1=dat2[1,2], v2=dat2[2,2],
                        `p.value`=prettyround(az[[1]]$`Pr(>F)`[1]))
      colnames(tib1)[1] = unfactor(dat2[1,1][[1]])
      colnames(tib1)[2] = unfactor(dat2[2,1][[1]])
      t0[[i]] = tableGrob(tib1, rows = NULL, theme = mytheme) %>%
        add_title(paste("Difference in Mean",variable), gp=gpar(fontsize=12, fontface=2),
                  hjust=0, x=0, lpos=1)

    }
    return(t0)
  }

}
prop.tib = function(data,Groups="Invited", var1, var2){
  var1 = enquo(var1)
  var2 = enquo(var2)
  cnt1 = data %>%
    filter(!!var1 %in% Groups) %>%
    group_by(!!var1) %>%
    summarise(cnt=n())
  prop5 = data %>%
    filter(!!var1 %in% Groups) %>%
    group_by(!!var1, !!var2) %>%
    summarise(n=n())
  propme = left_join(prop5, cnt1, by="Jail_Type") %>% data.frame()
  propme = propme %>%
    mutate(Proportion = n/cnt, Grp = paste0(!!var1,"\n (N=", cnt, ")")) %>%
    select(Grp, !!var2, Proportion)
  return(propme)
}

themenikko = theme(axis.text.x=element_text(size = 10, face = "bold"),
      # axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom")





