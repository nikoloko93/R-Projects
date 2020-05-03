## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(flexdashboard)
library(RPostgres)
library(varhandle)
library(tidyverse)
#source("../UHG_Palette.R")

connection <- dbConnect(RPostgres::Postgres(), dbname='research1', host='research1.c1nqtd6s9v6b.us-east-1.rds.amazonaws.com',port=5432, password='AF5B6D68-3958-4CED-AE87-8D060F6BE0A2', user="nramal")


Query1 <- paste("SELECt c.CatgID, c.CatgDesc, e.StatID ,e.StatDesc
  ,sum(case when v.statdate between '2019-03-24' and '2019-03-30' then v.StatValue end) as Week_20190324",
               " ,sum(case when v.statdate between '2019-03-31' and '2019-04-06' then v.StatValue end) as Week_20190331",
               " ,sum(case when v.statdate between '2019-04-07' and '2019-04-13' then v.StatValue end) as Week_20190407",
               " ,sum(case when v.statdate between '2019-04-14' and '2019-04-20' then v.StatValue end) as Week_20190414",
               " ,sum(case when v.statdate between '2019-04-21' and '2019-04-27' then v.StatValue end) as Week_20190421",
               " ,sum(case when v.statdate between '2019-04-28' and '2019-05-04' then v.StatValue end) as Week_20190428",
               " ,sum(case when v.statdate between '2019-05-05' and '2019-05-11' then v.StatValue end) as Week_20190505",
               " ,sum(case when v.statdate between '2019-05-12' and '2019-05-18' then v.StatValue end) as Week_20190512",
               " ,sum(case when v.statdate between '2019-05-19' and '2019-05-25' then v.StatValue end) as Week_20190519",
               " ,sum(case when v.statdate between '2019-05-26' and '2019-06-01' then v.StatValue end) as Week_20190526",
               " ,sum(case when v.statdate between '2019-06-02' and '2019-06-08' then v.StatValue end) as Week_20190602",
               " ,sum(case when v.statdate between '2019-06-09' and '2019-06-15' then v.StatValue end) as Week_20190609",
               " ,sum(case when v.statdate between '2019-06-16' and '2019-06-22' then v.StatValue end) as Week_20190616",
               " ,sum(case when v.statdate between '2019-06-23' and '2019-06-29' then v.StatValue end) as Week_20190623",
               " ,sum(case when v.statdate between '2019-06-30' and '2019-07-06' then v.StatValue end) as Week_20190630",
               " ,sum(case when v.statdate between '2019-07-07' and '2019-07-13' then v.StatValue end) as Week_20190707",
               " ,sum(case when v.statdate between '2019-07-14' and '2019-07-20' then v.StatValue end) as Week_20190714",
               " ,sum(case when v.statdate between '2019-07-21' and '2019-07-27' then v.StatValue end) as Week_20190721",
               " FROM spe_tt_tdt_sma.ops_stat_values as v
   JOIN spe_tt_tdt_sma.ops_stat_catg as c on v.CatgId=c.CatgID
   JOIN spe_tt_tdt_sma.ops_stat_event as e on v.CatgId=e.CatgID and v.StatId=e.StatID
GROUP BY
   c.CatgID
  ,c.CatgDesc
  ,e.StatID
  ,e.StatDesc
ORDER BY
   c.CatgID
   ,e.StatID;")

Query2 <- paste("SELECT c.CatgID, c.CatgDesc, e.StatID, e.StatDesc, v.providerid, v.statdate, v.statvalue
                FROM spe_tt_tdt_sma.ops_stat_values as v
                JOIN spe_tt_tdt_sma.ops_stat_catg as c on v.CatgId=c.CatgID
                JOIN spe_tt_tdt_sma.ops_stat_event as e on v.CatgId=e.CatgID and v.StatId=e.StatID
                 ORDER BY
                  c.CatgID
                  ,e.StatID
                  ,v.statdate;")



Weeklydat <- dbGetQuery(connection, Query1)
dlydat <- dbGetQuery(connection, Query2)
dlydat$statdate <- as.character(dlydat$statdate)
dlydat <- unique(dlydat)
# 
# Weeklydat <- read.csv("Weekly.csv")
# 

Weeklydat <- Weeklydat %>%
  gather(key = "Week", value = "Counts", 5:ncol(Weeklydat)) %>%
  mutate(weekstr = paste0(str_sub(Week, -8, -5),"-",
                          str_sub(Week, -4, -3), "-",
                          str_sub(Week, -2, -1)))
Weeklydat[is.na(Weeklydat)] <- 0
weekme <-unique(Weeklydat$weekstr)

daily <- data.frame()

for (i in 1:length(weekme)){
  dd = as.Date(weekme[i], "%Y-%m-%d")
  ddd = dd + seq(0,6)
  ddd = as.character(ddd)
  yymmdd = data.frame(weekstr=weekme[i], dd = ddd)
  daily = rbind(daily, yymmdd)
}


cat <- unique(dlydat$catgdesc)
dd <- dlydat

for (j in 1:length(cat)){
  ddcat <- dlydat %>% filter(catgdesc==cat[j])
  datenodata <- setdiff(daily$dd, ddcat[,5])
  dd1 <- data.frame()
  for (i in 1:length(datenodata)){
    dat <- ddcat %>% 
      group_by(catgid, catgdesc, statid, statdesc, providerid) %>% 
      summarise(n=n()) %>% 
      select(-n) %>%  
      mutate(statdate=datenodata[i], statvalue=0) %>% data.frame() 
    dd1 <- rbind(dd1, dat)
  }
  dd <- rbind(dd, dd1)
}

dd <- dd %>% 
  group_by(catgid, catgdesc, statid, statdesc, statdate) %>% 
  summarise(statvalue = sum(statvalue)) %>% data.frame()

dayme <- sort(unique(dd$statdate)) 

today <- Sys.Date() %>% as.character()
      if ( today %in% daily$dd ){
      curweek  <- daily %>% 
        filter(dd == today)
      curwek <- curweek[1,1]
      curday <- curweek[1,2]
      }else{
        curweek = "2019-03-31"
        curday <- "2019-03-31"
      }

# Weeklydat <- read.csv("Weekly.csv")
ui <- dashboardPage(
  dashboardHeader(title = "Weekly Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItem(tabName = "dashboard",
    tabBox(width = 12,
      tabPanel("Weekly", 
               fluidRow(
                 box(width = 3, collapsible = TRUE,
                     selectInput(inputId = "fil0", "Week",
                                 choices = c(unique(Weeklydat$weekstr)), selected = curwek)
                 ),
                 box(width=9, height = 150, status = "warning", collapsible = TRUE,
                     shinydashboard::valueBoxOutput("boxIDA", width=4),
                     shinydashboard::valueBoxOutput("boxFUA", width=4),
                     shinydashboard::valueBoxOutput("boxSDA", width=4)
                 )
                 # box(width = 6, status = "primary", collapsible = TRUE,
                 #   shinydashboard::valueBoxOutput("box1", width=6),
                 #   shinydashboard::valueBoxOutput("box2", width=6),
                 #   shinydashboard::valueBoxOutput("box3", width=6),  
                 #   shinydashboard::valueBoxOutput("box4", width=6)
                 # )
                 
               ),
               fluidRow(
                 box(width=12, height = 150,status = "primary", collapsible = TRUE,
                 shinydashboard::valueBoxOutput("box1", width=3),
                 shinydashboard::valueBoxOutput("box2", width=3),
                 shinydashboard::valueBoxOutput("box3", width=3),
                 shinydashboard::valueBoxOutput("box4", width=3)
                 )
               ),
               fluidRow(
                 column(width = 6,
                        fluidRow(
                          box(width = 12, title = "Referrals", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab1")
                          )
                        ), 
                        fluidRow(
                          box(width = 12, title = "Appointments", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab2")
                          )
                        )
                 ),
                 column(width = 6,
                        fluidRow(
                          box(width = 12, title = "Encounters", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab3")
                          )
                        ), 
                        fluidRow(
                          box(width = 12, title = "Patients", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab4")
                          )
                        )
                 )
               ) 
      ),
      tabPanel("Daily",
               fluidRow(
                 box(width = 3, collapsible = TRUE,
                     selectInput(inputId = "fil1", "Day",
                                 choices = "")
                 ),
                 box(width=9, height = 150, status = "warning", collapsible = TRUE,
                     shinydashboard::valueBoxOutput("boxIDAdd", width=4),
                     shinydashboard::valueBoxOutput("boxFUAdd", width=4),
                     shinydashboard::valueBoxOutput("boxSDAdd", width=4)
                 )
                 # box(width = 6, status = "primary", collapsible = TRUE,
                 #     shinydashboard::valueBoxOutput("box5", width=6),
                 #     shinydashboard::valueBoxOutput("box6", width=6),
                 #     shinydashboard::valueBoxOutput("box7", width=6),  
                 #     shinydashboard::valueBoxOutput("box8", width=6)
                 # )
                 
               ),
               fluidRow(
                 box(width = 12, height = 150, status = "primary", collapsible = TRUE,
                     shinydashboard::valueBoxOutput("box5", width=3),
                     shinydashboard::valueBoxOutput("box6", width=3),
                     shinydashboard::valueBoxOutput("box7", width=3),  
                     shinydashboard::valueBoxOutput("box8", width=3)
                 )
               ),
               
               fluidRow(
                 column(width = 6,
                        fluidRow(
                          box(width = 12, title = "Referrals", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab5")
                          )
                        ), 
                        fluidRow(
                          box(width = 12, title = "Appointments", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab6")
                          )
                        )
                 ),
                 column(width = 6,
                        fluidRow(
                          box(width = 12, title = "Encounters", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab7")
                          )
                        ), 
                        fluidRow(
                          box(width = 12, title = "Patients", solidHeader = TRUE, status = "primary",
                              dataTableOutput("tab8")
                          )
                        )
                 )
               )
      )
    )
    
   )
  )
)

server <- function(input, output, session) {
  d1 <- reactive({
    daily %>% filter(weekstr==input$fil0)
  })
  observeEvent(d1(),{
    updateSelectInput(session, "fil1", "Daily", choices = unique(d1()$dd))
  })
  output$boxIDA <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-IDA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total IDA Scheduled", icon = icon("user"), color = "teal")
  })
  output$boxFUA <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-FUA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total FUA Scheduled", icon = icon("user"), color = "teal")
  })
  output$boxSDA <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-SDA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total SDA Scheduled", icon = icon("user"), color = "teal")
  })
  output$boxIDAdd <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-IDA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total IDA Scheduled", icon = icon("user"), color = "teal")
  })
  output$boxFUAdd <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-FUA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total FUA Scheduled", icon = icon("user"), color = "teal")
  })
  output$boxSDAdd <- shinydashboard::renderValueBox({
    tmp2 <- dd %>% 
      filter(statdesc == "Future-SDA") %>% 
      select(statdesc,statdate, statvalue) %>% 
      group_by(statdesc) %>% 
      summarise(v2=sum(statvalue)) %>% data.frame()
    shinydashboard::valueBox(tmp2[1,2], "Total SDA Scheduled", icon = icon("user"), color = "teal")
  })
  output$box1 <- shinydashboard::renderValueBox({
    tmp1 <- Weeklydat %>% 
      filter(weekstr == input$fil0, catgdesc=="Referrals")
    ref = sum(tmp1$Counts)
    shinydashboard::valueBox(ref, "Referrals this week", icon = icon("user"), color = "teal")
  })
  output$box2 <- shinydashboard::renderValueBox({
    tmp1 <- Weeklydat %>% 
      filter(weekstr == input$fil0, catgdesc=="Schedule")
    ref = sum(tmp1$Counts)
    shinydashboard::valueBox(ref, "Appointments this week", icon = icon("user"), color = "teal")
  })
  output$box3 <- shinydashboard::renderValueBox({
    tmp1 <- Weeklydat %>% 
      filter(weekstr == input$fil0, catgdesc=="Encounters")
    ref = sum(tmp1$Counts)
    shinydashboard::valueBox(ref, "Encounters this week", icon = icon("user"), color = "teal")
  })
  output$box4 <- shinydashboard::renderValueBox({
    tmp1 <- Weeklydat %>% 
      filter(weekstr == input$fil0, catgdesc=="Patients")
    ref = sum(tmp1$Counts)
    shinydashboard::valueBox(ref, "Patients this week", icon = icon("user"), color = "teal")
  })
  output$tab1 <- renderDataTable({
    if (input$fil0== "2019-03-24"){
      tmp3 <- Weeklydat %>% 
        filter(weekstr==input$fil0, catgdesc == "Referrals") %>% 
        mutate(v="--No Data--", v2 = Counts) %>% 
        select(statdesc,v, Counts, v2)  
    }
    else{
      indx = match(input$fil0, weekme)
      wks = weekme[c(indx-1, indx)]
      tmp <- Weeklydat %>% 
        filter(weekstr %in% wks, catgdesc == "Referrals") %>% 
        select(statdesc,weekstr, Counts)  %>% 
        spread(key = weekstr, value = Counts)
      wks1 = weekme[seq(1,indx)]
      tmp2 <- Weeklydat %>% 
        filter(weekstr %in% wks1, catgdesc == "Referrals") %>% 
        select(statdesc,weekstr, Counts) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(Counts)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
        
    }
    datatable(tmp3,colnames = c("Events","Last Week", "Current Week", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab2 <- renderDataTable({
    if (input$fil0== "2019-03-24"){
      tmp3 <- Weeklydat %>% 
        filter(weekstr==input$fil0, catgdesc == "Schedule") %>% 
        mutate(v="--No Data--", v2 = Counts) %>% 
        select(statdesc,v, Counts, v2) %>% 
        arrange(match(statdesc, c("Future-IDA", "Future-FUA", "Future-SDA", "Arrival", "Bumped", "Cancelled", "Noshow", "Pending"))) %>% 
        mutate(statdesc = str_replace(statdesc, "Future-", ""))
    }
    else{
      indx = match(input$fil0, weekme)
      wks = weekme[c(indx-1, indx)]
      tmp <- Weeklydat %>% 
        filter(weekstr %in% wks, catgdesc == "Schedule") %>% 
        select(statdesc,weekstr, Counts)  %>% 
        spread(key = weekstr, value = Counts)
      wks1 = weekme[seq(1,indx)]
      tmp2 <- Weeklydat %>% 
        filter(weekstr %in% wks1, catgdesc == "Schedule") %>% 
        select(statdesc,weekstr, Counts) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(Counts)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2) %>% 
       arrange(match(statdesc, c("Future-IDA", "Future-FUA", "Future-SDA", "Arrival", "Bumped", "Cancelled", "Noshow", "Pending"))) %>% 
        mutate(statdesc = str_replace(statdesc, "Future-", ""))
        
    
      
    }
    datatable(tmp3,colnames = c("Events","Last Week", "Current Week", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab3 <- renderDataTable({
    if (input$fil0== "2019-03-24"){
      tmp3 <- Weeklydat %>% 
        filter(weekstr==input$fil0, catgdesc == "Encounters") %>% 
        mutate(v="--No Data--", v2 = Counts) %>% 
        select(statdesc,v, Counts, v2)  
    }
    else{
      indx = match(input$fil0, weekme)
      wks = weekme[c(indx-1, indx)]
      tmp <- Weeklydat %>% 
        filter(weekstr %in% wks, catgdesc == "Encounters") %>% 
        select(statdesc,weekstr, Counts)  %>% 
        spread(key = weekstr, value = Counts)
      wks1 = weekme[seq(1,indx)]
      tmp2 <- Weeklydat %>% 
        filter(weekstr %in% wks1, catgdesc == "Encounters") %>% 
        select(statdesc,weekstr, Counts) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(Counts)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
      
    }
    datatable(tmp3,colnames = c("Events","Last Week", "Current Week", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab4 <- renderDataTable({
    if (input$fil0== "2019-03-24"){
      tmp3 <- Weeklydat %>% 
        filter(weekstr==input$fil0, catgdesc == "Patients") %>% 
        mutate(v="--No Data--", v2 = Counts) %>% 
        select(statdesc,v, Counts, v2)  
    }
    else{
      indx = match(input$fil0, weekme)
      wks = weekme[c(indx-1, indx)]
      tmp <- Weeklydat %>% 
        filter(weekstr %in% wks, catgdesc == "Patients") %>% 
        select(statdesc,weekstr, Counts)  %>% 
        spread(key = weekstr, value = Counts)
      wks1 = weekme[seq(1,indx)]
      tmp2 <- Weeklydat %>% 
        filter(weekstr %in% wks1, catgdesc == "Patients") %>% 
        select(statdesc,weekstr, Counts) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(Counts)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
      
    }
    datatable(tmp3,colnames = c("Events","Last Week", "Current Week", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab5 <- renderDataTable({
    if (as.Date(input$fil1) < as.Date("2019-03-30")){
      tmp3 <- dd %>% 
        filter(catgdesc == "Referrals") 
      tmp3 <- data.frame(v0=unique(tmp3$statdesc),v="--No Data--", v2 = "--No Data--", v3 = "--No Data--") 
        
    }
    else if (input$fil1== "2019-03-30"){
      tmp3 <- dd %>% 
        filter(statdate==input$fil1, catgdesc == "Referrals") %>% 
        mutate(v="--No Data--", v2 = statvalue) %>% 
        select(statdesc,v, statvalue, v2)  
    }
    else if (as.Date(input$fil1) > as.Date("2019-03-30")){
      
      indx = match(input$fil1, dayme)
      wks = dayme[c(indx-1, indx)]
      tmp <- dd %>% 
        filter(statdate %in% wks, catgdesc == "Referrals") %>% 
        select(statdesc,statdate, statvalue)  %>% 
        spread(key = statdate, value = statvalue)
      wks1 = dayme[seq(1,indx)]
      tmp2 <- dd %>% 
        filter(statdate %in% wks1, catgdesc == "Referrals") %>% 
        select(statdesc,statdate, statvalue) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(statvalue)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
      
    }
    datatable(tmp3,colnames = c("Events","Yesterday", "Today", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab6 <- renderDataTable({
    if (as.Date(input$fil1) < as.Date("2019-03-30")){
      tmp3 <- dd %>% 
        filter(catgdesc == "Schedule") 
      tmp3 <- data.frame(statdesc=unique(tmp3$statdesc),v="--No Data--", v2 = "--No Data--", v3 = "--No Data--") %>% 
        arrange(match(statdesc, c("Future-IDA", "Future-FUA", "Future-SDA", "Arrival", "Bumped", "Cancelled", "Noshow", "Pending"))) %>% 
        mutate(statdesc = str_replace(statdesc, "Future-", ""))
        
      
    }
    else if (input$fil1== "2019-03-30"){
      tmp3 <- dd %>% 
        filter(statdate==input$fil1, catgdesc == "Schedule") %>% 
        mutate(v="--No Data--", v2 = statvalue) %>% 
        select(statdesc,v, statvalue, v2) %>% 
        arrange(match(statdesc, c("Future-IDA", "Future-FUA", "Future-SDA", "Arrival", "Bumped", "Cancelled", "Noshow", "Pending"))) %>% 
        mutate(statdesc = str_replace(statdesc, "Future-", ""))
    }
    else if (as.Date(input$fil1) > as.Date("2019-03-30")){
      
      indx = match(input$fil1, dayme)
      wks = dayme[c(indx-1, indx)]
      tmp <- dd %>% 
        filter(statdate %in% wks, catgdesc == "Schedule") %>% 
        select(statdesc,statdate, statvalue)  %>% 
        spread(key = statdate, value = statvalue)
      wks1 = dayme[seq(1,indx)]
      tmp2 <- dd %>% 
        filter(statdate %in% wks1, catgdesc == "Schedule") %>% 
        select(statdesc,statdate, statvalue) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(statvalue)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2) %>% 
        arrange(match(statdesc, c("Future-IDA", "Future-FUA", "Arrival", "Bumped", "Cancelled", "Noshow", "Pending"))) %>% 
        mutate(statdesc = str_replace(statdesc, "Future-", ""))
      
    }
    datatable(tmp3,colnames = c("Events","Yesterday", "Today", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab7 <- renderDataTable({
    if (as.Date(input$fil1) < as.Date("2019-03-30")){
      tmp3 <- dd %>% 
        filter(catgdesc == "Encounters") 
      tmp3 <- data.frame(v0=unique(tmp3$statdesc),v="--No Data--", v2 = "--No Data--", v3 = "--No Data--") 
      
    }
    else if (input$fil1== "2019-03-30"){
      tmp3 <- dd %>% 
        filter(statdate==input$fil1, catgdesc == "Encounters") %>% 
        mutate(v="--No Data--", v2 = statvalue) %>% 
        select(statdesc,v, statvalue, v2)  
    }
    else if (as.Date(input$fil1) > as.Date("2019-03-30")){
      
      indx = match(input$fil1, dayme)
      wks = dayme[c(indx-1, indx)]
      tmp <- dd %>% 
        filter(statdate %in% wks, catgdesc == "Encounters") %>% 
        select(statdesc,statdate, statvalue)  %>% 
        spread(key = statdate, value = statvalue)
      wks1 = dayme[seq(1,indx)]
      tmp2 <- dd %>% 
        filter(statdate %in% wks1, catgdesc == "Encounters") %>% 
        select(statdesc,statdate, statvalue) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(statvalue)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
      
    }
    datatable(tmp3,colnames = c("Events","Yesterday", "Today", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$tab8 <- renderDataTable({
    if (as.Date(input$fil1) < as.Date("2019-03-30")){
      tmp3 <- dd %>% 
        filter(catgdesc == "Patients") 
      tmp3 <- data.frame(v0=unique(tmp3$statdesc),v="--No Data--", v2 = "--No Data--", v3 = "--No Data--") 
      
    }
    else if (input$fil1== "2019-03-30"){
      tmp3 <- dd %>% 
        filter(statdate==input$fil1, catgdesc == "Patients") %>% 
        mutate(v="--No Data--", v2 = statvalue) %>% 
        select(statdesc,v, statvalue, v2)  
    }
    else if (as.Date(input$fil1) > as.Date("2019-03-30")){
      
      indx = match(input$fil1, dayme)
      wks = dayme[c(indx-1, indx)]
      tmp <- dd %>% 
        filter(statdate %in% wks, catgdesc == "Patients") %>% 
        select(statdesc,statdate, statvalue)  %>% 
        spread(key = statdate, value = statvalue)
      wks1 = dayme[seq(1,indx)]
      tmp2 <- dd %>% 
        filter(statdate %in% wks1, catgdesc == "Patients") %>% 
        select(statdesc,statdate, statvalue) %>% 
        group_by(statdesc) %>% 
        summarise(v2=sum(statvalue)) %>% data.frame()
      tmp3 <- left_join(tmp, tmp2)
      
    }
    datatable(tmp3,colnames = c("Events","Yesterday", "Today", "Running Total"),
              options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    
  })
  output$box5 <- shinydashboard::renderValueBox({
    tmp1 <- dd %>% 
      filter(statdate == input$fil1, catgdesc=="Referrals")
    ref = sum(tmp1$statvalue)
    shinydashboard::valueBox(ref, "Referrals today", icon = icon("user"), color = "teal")
  })
  output$box6 <- shinydashboard::renderValueBox({
    tmp1 <- dd %>% 
      filter(statdate == input$fil1, catgdesc=="Schedule")
    ref = sum(tmp1$statvalue)
    shinydashboard::valueBox(ref, "Appointments today", icon = icon("user"), color = "teal")
  })
  output$box7 <- shinydashboard::renderValueBox({
    tmp1 <- dd %>% 
      filter(statdate == input$fil1, catgdesc=="Encounters")
    ref = sum(tmp1$statvalue)
    shinydashboard::valueBox(ref, "Encounters today", icon = icon("user"), color = "teal")
  })
  output$box8 <- shinydashboard::renderValueBox({
    tmp1 <- dd %>% 
      filter(statdate == input$fil1, catgdesc=="Patients")
    ref = sum(tmp1$statvalue)
    shinydashboard::valueBox(ref, "Patients today", icon = icon("user"), color = "teal")
  })
}

shinyApp(ui, server)
