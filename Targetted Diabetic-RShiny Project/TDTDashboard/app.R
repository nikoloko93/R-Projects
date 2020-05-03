## app.R ##
library(shiny)
library(DT)
library(shinydashboard)
library(varhandle)
#install.packages("flexdashboard")
library(flexdashboard)
library(tidyverse)
source("../UHG_Palette.R", local = T)
TDTdat <- read.csv("../pseudo_data.csv")
TDTdat <- TDTdat %>% 
  mutate(Yr_Mo = paste(month.name[as.numeric(str_sub(as.character(Year_Mo), -2,-1))],
                       str_sub(as.character(Year_Mo),1,4)))
yr_mo <- unique(TDTdat$Year_Mo)
month <- month.name
ex_sorted <- sort(yr_mo)
month_names <- month[as.numeric(str_sub(as.character(ex_sorted), -2,-1))]
yr_mo <- paste(month_names,str_sub(as.character(ex_sorted),1,4))

# Checking current month and previous month
curMo <- paste(as.POSIXlt(as.character(Sys.Date())) %>% format("%B"), as.POSIXlt(as.character(Sys.Date())) %>% format("%Y"))

numMo <- as.POSIXlt(as.character(Sys.Date())) %>% format("%m") %>% as.numeric()
if (numMo==1){
  yearNo <- as.POSIXlt(as.character(Sys.Date())) %>% format("%Y") %>% as.numeric() - 1
  preMO <- month.name[12]
  prevYrMO <- paste(preMO,yearNo)
}
if (numMo>1){
  yearNo <- as.POSIXlt(as.character(Sys.Date())) %>% format("%Y") %>% as.numeric() 
  preMO <- month.name[numMo-1]
  prevYrMO <- paste(preMO,yearNo)
}


css1 <- "text-align: left;
         font-weight: bold;
         color: #ffffff"

ui <- dashboardPage(
  
  dashboardHeader(title = "TDT Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody( 
    tags$head(tags$style(
      type="text/css",
      "#img img {max-width: 100%; width: 77%; height: auto; }"
    )),
    #first tab content
    tabItem(tabName = "dashboard", 
    fluidRow(
      box(width = 3,
          selectInput(inputId = "fil0", "Year Month",
                      choices = c("All", unique(yr_mo)), selected = curMo)
      ),
      box(width = 3,
          selectInput(inputId = "fil1", "Line of Business",
                      choices = c("All",unfactor(unique(TDTdat$LOB))))
      ),
      box(width = 3,
          checkboxGroupInput(inputId = "fil2","Clinic(s)",
                             choices = unique(TDTdat$Clinics) %>% unfactor(), 
                             selected = unfactor(unique(TDTdat$Clinics)))
      )
    ),
    fluidRow(
    column(width = 6,
      fluidRow(
      box(width = 12, title = "Member Engagement", solidHeader = TRUE, status = "primary",
          column(width = 8, dataTableOutput("tab1")),
          column(width = 4, imageOutput("img"), style = "height: 120px; display: inline-block; padding-left: 110px; position: absolute; left: 72%; top: 52%; margin-top: -49px; margin-left: -30px;"),
          column(width = 2, htmlOutput("txt"), style = "display: inline-block; padding-left: 10px; padding-top: -20px; position: absolute; top: 35%; left: 65%; width: 34%; height: 37%; z-index: 99;background-color: rgba(45, 95, 167,0.8); border-style: solid; border-color: rgb(45, 95, 167)")
          #column(width =5, shinydashboard::valueBoxOutput("vbox"), style = "position: absolute; top: 38%; left: 64%; width: 100%; height: 100%; z-index: 10;")
      )),
      fluidRow(
      box(width = 12, title = "Net Promoter Score", solidHeader = TRUE, status = "primary",
          column(width = 8, plotOutput("plt2",height = 120), style = "display: inline-block; height: 200px; padding-top: 35px;"),
          column(width = 4, gaugeOutput("txt2"), style = "display: inline-block; padding: 15px; height: 200px;") 
      )),
      fluidRow(
        box(width = 12, title = "Visits per Clinic", solidHeader = TRUE, status = "primary",
          plotOutput("plt3")
      )),
      fluidRow(
        box(width = 12, title = "Finance Metrics", solidHeader = TRUE, status = "primary",
            dataTableOutput("tab3")
      ))
      ),
    column(width = 6,
      fluidRow(
      box(width = 12, title = "Visits per Month", solidHeader = TRUE, status = "primary",
          column(width=12, plotOutput("plt1", height = 360)),
          column(width=10, offset = 1, dataTableOutput("tab2"))
        
      )),
      fluidRow(
        box(width = 12, title = "Timeline", solidHeader = TRUE, status = "primary",
          plotOutput("plt4")
      )),
      fluidRow(
        box(width = 12, title = "Glycemic Control Data", solidHeader = TRUE, status = "primary",
            column(width=6, shinydashboard::valueBoxOutput("box_GMI", width=NULL)),
            column(width=6, shinydashboard::valueBoxOutput("box_MedAd", width=NULL)),
            column(width=6, offset = 0, shinydashboard::valueBoxOutput("box_A1c", width=NULL)),
            column(width=6, offset = 0, shinydashboard::valueBoxOutput("box_TIR", width=NULL))
        ))
    ))

    
    )
  )
)


server <- function(input, output) {
  
  data1 <- eventReactive(c(input$fil0, input$fil1, input$fil2), {
    if (input$fil0!="All"){
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo==input$fil0)
    }
    else{
      TDTdat <- TDTdat
    }
    if (input$fil1=="All"){
      memEngage <- TDTdat %>% 
        filter(Clinics %in% input$fil2) %>% # input$fil2
        gather(key = "Mem_engage", value = "Value", 4:9)
      prtWtrFll<- data.frame(Mem_engage1 = c("Referrals", "Visits with consent completed", "AGP reviewed visits", 
                                             "Visits with changes (med or lifestyle)", "2nd visit scheduled",
                                             "2nd AGP reviewed"), Mem_engage = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                                 "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                                 "AGP_review_visit2"))
      memEngage <- merge(memEngage, prtWtrFll, by="Mem_engage")
      memEngage$Mem_engage <- ordered(memEngage$Mem_engage, levels = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                       "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                       "AGP_review_visit2"))
      memEngage <- memEngage %>% 
        group_by(Mem_engage,Mem_engage1) %>% 
        summarise(Count = sum(Value)) %>% data.frame()
      
      colnames(memEngage)[2] <- "Participation Waterfall"
      memEngage[,-1]
    }
    else{
      memEngage <- TDTdat %>% 
        filter(Clinics %in% input$fil2, LOB == input$fil1) %>% # input$fil2
        gather(key = "Mem_engage", value = "Value", 4:9)
      prtWtrFll<- data.frame(Mem_engage1 = c("Referrals", "Visits with consent completed", "AGP reviewed visits", 
                                             "Visits with changes (med or lifestyle)", "2nd visit scheduled",
                                             "2nd AGP reviewed"), Mem_engage = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                                 "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                                 "AGP_review_visit2"))
      memEngage <- merge(memEngage, prtWtrFll, by="Mem_engage")
      memEngage$Mem_engage <- ordered(memEngage$Mem_engage, levels = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                       "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                       "AGP_review_visit2"))
      memEngage <- memEngage %>% 
        group_by(Mem_engage,Mem_engage1) %>% 
        summarise(Count = sum(Value)) %>% data.frame()
      
      colnames(memEngage)[2] <- "Participation Waterfall"
      memEngage[,-1]
    }
    
  })
  prevdata <- eventReactive(c(input$fil0, input$fil1, input$fil2), {
    yearMo <- unique(yr_mo)
    if (!input$fil0 %in% c("April 2019", "All")){
      indPrevMo <- match(input$fil0, yearMo) - 1
      filt = yearMo[indPrevMo]
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo==filt)
    }
    
    if (input$fil1=="All"){
      memEngage <- TDTdat %>% 
        filter(Clinics %in% input$fil2) %>% # input$fil2
        gather(key = "Mem_engage", value = "Value", 4:9)
      prtWtrFll<- data.frame(Mem_engage1 = c("Referrals", "Visits with consent completed", "AGP reviewed visits", 
                                             "Visits with changes (med or lifestyle)", "2nd visit scheduled",
                                             "2nd AGP reviewed"), Mem_engage = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                                 "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                                 "AGP_review_visit2"))
      memEngage <- merge(memEngage, prtWtrFll, by="Mem_engage")
      memEngage$Mem_engage <- ordered(memEngage$Mem_engage, levels = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                       "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                       "AGP_review_visit2"))
      memEngage <- memEngage %>% 
        group_by(Mem_engage,Mem_engage1) %>% 
        summarise(Count = sum(Value)) %>% data.frame()
      
      colnames(memEngage)[2] <- "Participation Waterfall"
      memEngage[,-1]
    }
    else{
      memEngage <- TDTdat %>% 
        filter(Clinics %in% input$fil2, LOB == input$fil1) %>% # input$fil2
        gather(key = "Mem_engage", value = "Value", 4:9)
      prtWtrFll<- data.frame(Mem_engage1 = c("Referrals", "Visits with consent completed", "AGP reviewed visits", 
                                             "Visits with changes (med or lifestyle)", "2nd visit scheduled",
                                             "2nd AGP reviewed"), Mem_engage = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                                 "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                                 "AGP_review_visit2"))
      memEngage <- merge(memEngage, prtWtrFll, by="Mem_engage")
      memEngage$Mem_engage <- ordered(memEngage$Mem_engage, levels = c("Referred", "Dev_Applic_Visit1", "AGP_review_visit1",
                                                                       "AGP_revChange_visit", "Dev_Applic_Visit2",
                                                                       "AGP_review_visit2"))
      memEngage <- memEngage %>% 
        group_by(Mem_engage,Mem_engage1) %>% 
        summarise(Count = sum(Value)) %>% data.frame()
      
      colnames(memEngage)[2] <- "Participation Waterfall"
      memEngage[,-1]
    }
    
  })
  output$tab1 <- renderDataTable(data1(), options = list(dom = 't'), 
                                 selection = list(mode = 'single', selected = 1, target = 'row'), 
                                 rownames = FALSE) #selection = 'single',
  
  # output$tab2 <- renderDataTable(data1(), options = list(dom = 't'), 
  #                                selection = list(mode = 'single', selected = 1, target = 'row'), 
  #                                rownames = FALSE) #selection = 'single',
  
  
  Clicked <- eventReactive(input$tab1_rows_selected,{
    input$tab1_rows_selected
  })

  
  
  output$img <- renderImage({
    if (Clicked()==1){
      return(list(
        src = "../images/grp.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
    if (Clicked()==2){
      return(list(
        src = "../images/hospital.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
    if (Clicked()==3){
      return(list(
        src = "../images/agp.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
    if (Clicked()==4){
      return(list(
        src = "../images/vis2.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
    if (Clicked()==5){
      return(list(
        src = "../images/hospital.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
    if (Clicked()==6){
      return(list(
        src = "../images/agp.png",
        contentType = "image/png",
        width = 100, height = 100,
        alt = "Face"
      ))
    }
  }, deleteFile = FALSE)
  output$txt <- renderUI({
    cont1 <- "<style>
              p{
              font-size : 8px;
              padding : 0;
              font-weight: bold;
              margin : 0;
              line-height : 0px;
              color:#ffffff;
                }
              </style>
              <table style='width:90%'>
              <tr>
              <th style = 'text-align:left'>"
    
    if (!input$fil0 %in% c("April 2019", "All")){
      if (Clicked() == 1){
        HTML(paste(cont1,  
          h1(paste(data1()[1,2]), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
          "<p>Current Month</p></th>
          <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
          <th style = 'text-align: left'>",
          h3(paste(prevdata()[1,2]), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
          "<p>Previous Month</p></th> 
          </tr>
          </table>", 
          h5("Members Referred", style = css1)
        ))
    }
      else if (Clicked() == 2){
        HTML(paste(cont1,  
                   h1(paste(scales::percent(data1()[2,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Current Month</p></th>
                   <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
                   <th style = 'text-align: left'>",
                   h3(paste(scales::percent(prevdata()[2,2]/prevdata()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Previous Month</p></th> 
                   </tr>
                   </table>", 
                   h5("Initial Device Application", style = css1)
        ))
        
      }
      else if (Clicked() == 3){
        HTML(paste(cont1,  
                   h1(paste(scales::percent(data1()[3,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Current Month</p></th>
                   <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
                   <th style = 'text-align: left'>",
                   h3(paste(scales::percent(prevdata()[3,2]/prevdata()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Previous Month</p></th> 
                   </tr>
                   </table>", 
                   h5("FUA-AGP Review", style = css1)
        ))
      }
      else if (Clicked() == 4){
        HTML(paste(cont1,  
                   h1(paste(scales::percent(data1()[4,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Current Month</p></th>
                   <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
                   <th style = 'text-align: left'>",
                   h3(paste(scales::percent(prevdata()[4,2]/prevdata()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Previous Month</p></th> 
                   </tr>
                   </table>", 
                   h5("FUA - AGP Review with Changes", style = css1)
        ))
      }
      else if (Clicked() == 5){
        HTML(paste(cont1,  
                   h1(paste(scales::percent(data1()[5,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Current Month</p></th>
                   <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
                   <th style = 'text-align: left'>",
                   h3(paste(scales::percent(prevdata()[4,2]/prevdata()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Previous Month</p></th> 
                   </tr>
                   </table>", 
                   h5("Subsequent Device Application", style = css1)
        ))
      }
      else if (Clicked() == 6){
        HTML(paste(cont1,  
                   h1(paste(scales::percent(data1()[6,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Current Month</p></th>
                   <th style = 'text-align: center; width: 20%'>", h5("VS", style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),"</th>
                   <th style = 'text-align: left'>",
                   h3(paste(scales::percent(prevdata()[6,2]/prevdata()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff; margin-bottom : 0;"),
                   "<p>Previous Month</p></th> 
                   </tr>
                   </table>", 
                   h5("2nd FUA-AGP Review", style = css1)
        ))
      }
    }
    else{
    if (Clicked() == 1){
      HTML(paste(
        h1(paste(data1()[1,2]), style = "text-align: left; font-weight: bold; color:#ffffff;line-height: 0.8"),
        h5("Members Referred", style = css1)
      ))
    }
    else if (Clicked() == 2){
      HTML(paste(
        h1(paste(scales::percent(data1()[2,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color: #ffffff"),
        h5("Initial Device Application", style = css1)
      ))
    }
    else if (Clicked() == 3){
      HTML(paste(
        h1(paste(scales::percent(data1()[3,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color: #ffffff"),
        h5("FUA-AGP Review", style = css1)
      ))
    }
    else if (Clicked() == 4){
      HTML(paste(
        h1(paste(scales::percent(data1()[4,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color:#ffffff"),
        h5("FUA - AGP Review with Changes", style = css1)
      ))
    }
    else if (Clicked() == 5){
      HTML(paste(
        h1(paste(scales::percent(data1()[5,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color: #ffffff"),
        h5("Subsequent Device Application", style = css1)
      ))
    }
    else if (Clicked() == 6){
      HTML(paste(
        h1(paste(scales::percent(data1()[6,2]/data1()[1,2], accuracy = 1)), style = "text-align: left; font-weight: bold; color: #ffffff"),
        h5("2nd FUA-AGP Review", style = css1)
      ))
    }
    }
  })
  
 
  output$plt1 <- renderPlot({
    targetperMonth1 <- data.frame(month = c("04","05","06","07","08","09","10","11","12"), year = 2019) %>% 
      mutate(Year_Mo = as.numeric(paste0(year, month)), targetNoVisit = seq(10,18)) %>% 
      select(Year_Mo, targetNoVisit)
    if (input$fil1=="All" & length(input$fil2)>1) {
      targetperMonth <- targetperMonth1 %>% full_join(
        TDTdat %>% 
          group_by(Year_Mo,Clinics) %>% 
          summarise(Visits = round(mean(Total_OP_Visit))))
    }
    else if(input$fil1 != "All" & (length(input$fil2)<=1 | length(input$fil2)>1 )) {
      targetperMonth <- targetperMonth1 %>% full_join(
        TDTdat %>% 
          filter(LOB == input$fil1, Clinics %in% input$fil2) %>% 
          group_by(Year_Mo,Clinics) %>% 
          summarise(Visits = round(mean(Total_OP_Visit))))
    }
    else if(input$fil1 == "All" & (length(input$fil2)<=1 | length(input$fil2)>1 )) {
      targetperMonth <- targetperMonth1 %>% full_join(
        TDTdat %>% 
          filter(Clinics %in% input$fil2) %>% 
          group_by(Year_Mo,Clinics) %>% 
          summarise(Visits = round(mean(Total_OP_Visit))))
    }
    
    
    
     
      ggplot(data = targetperMonth, aes(x=Year_Mo)) + geom_line(aes(y=Visits,color = Clinics), na.rm = TRUE, size = 1) + 
      geom_line(data= targetperMonth1, aes(y=targetNoVisit), size=1, linetype = "dashed") + ylim(1,25) +
      scale_color_discrete(na.translate = F) +
      scale_x_continuous(breaks = unique(targetperMonth$Year_Mo), labels = paste0(month.abb[4:12], "2019")) +
      theme(axis.text.x = element_text(size = 10, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom")
  })
  data2 <- eventReactive(c(input$fil0, input$fil1, input$fil2),{
    if (input$fil0!="All"){
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo==input$fil0)
    }
    else{
      TDTdat <- TDTdat
    }
    if (input$fil1=="All"){
      datNPScat <- TDTdat %>%
        filter(Clinics %in% input$fil2) %>% 
        mutate(NPSCat = case_when(NPS>=9 ~ "Promoter",
                                  NPS<=6 ~ "Detractor",
                                  TRUE ~ "Passive"))
      datNPScat
    }
    else {
      datNPScat <- TDTdat %>%
        filter(LOB == input$fil1, Clinics %in% input$fil2) %>% 
        mutate(NPSCat = case_when(NPS>=9 ~ "Promoter",
                                  NPS<=6 ~ "Detractor",
                                  TRUE ~ "Passive"))
      datNPScat
    }
  })

  output$plt2 <- renderPlot({
    #NPS <- ((sum(datNPScat$NPSCat=="Promoter") - sum(datNPScat$NPSCat=="Detractor"))/nrow(NPS))*100
    if (input$fil0!="All"){
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo==input$fil0)
    }
    else{
      TDTdat <- TDTdat
    }
    if (input$fil1=="All"){
      datNPScat <- TDTdat %>%
        filter(Clinics %in% input$fil2) %>% 
        mutate(NPSCat = case_when(NPS>=9 ~ "Promoter",
                                  NPS<=6 ~ "Detractor",
                                  TRUE ~ "Passive"))
    }
    else {
      datNPScat <- TDTdat %>%
        filter(LOB == input$fil1, Clinics %in% input$fil2) %>% 
        mutate(NPSCat = case_when(NPS>=9 ~ "Promoter",
                                  NPS<=6 ~ "Detractor",
                                  TRUE ~ "Passive"))
    }
    datNPScat %>% 
      group_by(NPSCat) %>% 
      summarise(prop = round((n()/nrow(datNPScat))*100)) %>% 
      mutate(NPS = "NPS") %>% 
      mutate(NPSCat = factor(NPSCat, levels = c("Promoter","Passive", "Detractor"))) %>% 
      ggplot(aes(x=NPS, y=prop, fill=NPSCat)) + geom_bar(stat = "identity",position = "fill") +
      geom_text(aes(y = cumsum(prop/100) - ((prop/2)/100), label = scales::percent(prop/100, accuracy = 1)), color = "black", fontface = "bold", size = 8) +
      coord_flip() +
      scale_fill_manual("",values = c("Detractor" = '#ea4b4d', "Passive" = '#f79837', "Promoter" = '#72c060')) +
      theme(axis.text.x = element_text(size = 14, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size=12, face = "bold")) +
      scale_y_continuous(labels = scales::percent) 
  })
  
  output$txt2 <- renderGauge({
    NPSdat = data2()
    NPS <- round(((sum(NPSdat$NPSCat=="Promoter") - sum(NPSdat$NPSCat=="Detractor"))/nrow(NPSdat))*100)
    gauge(NPS, label = "Net Promoter Score", 
          min = -100, 
          max = 100, 
          sectors = gaugeSectors(success = c(50, 100), 
                                 warning = c(-30, 50),
                                 danger = c(-100, -30)))
    
  })
  output$tab2 <- renderDataTable({
    yearMo <- unique(yr_mo)
    if (input$fil0 == "All"){
      if (curMo=="April 2019"){
        TDTdat <- TDTdat %>% 
          filter(Yr_Mo==curMo)
      }
      else{
        indPrevMo <- match(curMo, yearMo) - 1
        indCurMo <- match(curMo, yearMo)
        filt = yearMo[c(indCurMo,indPrevMo)]
        TDTdat <- TDTdat %>% 
          filter(Yr_Mo %in% filt)
      }
    }
    
    else if (input$fil0 == "April 2019"){
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo==input$fil0)
    }
    
    else if(!input$fil0 %in% c("All", "April 2019")){
      indPrevMo <- match(input$fil0, yearMo) - 1
      indCurMo <- match(input$fil0, yearMo)
      filt = yearMo[c(indCurMo,indPrevMo)]
      TDTdat <- TDTdat %>% 
        filter(Yr_Mo %in% filt)
    }
    
    
    if (input$fil1=="All" & length(input$fil2)>1) {
      
      tibVisit <- TDTdat %>% 
          group_by(Year_Mo,Clinics) %>% 
          summarise(Visits = round(mean(Total_OP_Visit))) %>% 
        spread(key = Year_Mo, value = Visits) %>% data.frame()
      tibVisit
    }
    else if(input$fil1 != "All" & (length(input$fil2)<=1 | length(input$fil2)>1 )) {
      
      tibVisit <- TDTdat %>% 
        filter(LOB == input$fil1, Clinics %in% input$fil2) %>% 
        group_by(Year_Mo,Clinics) %>% 
        summarise(Visits = round(mean(Total_OP_Visit))) %>% 
      spread(key = Year_Mo, value = Visits) %>% data.frame()
      tibVisit
    }
    
    else if(input$fil1 == "All" & (length(input$fil2)<=1 | length(input$fil2)>1 )) {
    
      tibVisit <- TDTdat %>% 
          filter(Clinics %in% input$fil2) %>% 
          group_by(Year_Mo,Clinics) %>% 
          summarise(Visits = round(mean(Total_OP_Visit))) %>% 
        spread(key = Year_Mo, value = Visits) %>% data.frame()
      tibVisit
    }
    if (length(names(tibVisit))>2){
      datatable(tibVisit,colnames = c("Clinics", "Last Month", "Current Month"),
                options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    }
    else{
      datatable(tibVisit,colnames = c("Clinics", "Current Month"),
                options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    }
    
  })
  
  output$plt3 <- renderPlot({
    if (input$fil0=="All" & length(input$fil2)>1){
    TDTdat %>% 
      gather(key = "OPType", value = "visit", 20:21) %>% 
      mutate(OpTypeVis = ifelse(OPType=="OP_IDA", "Initial Device Application", "AGP Review")) %>% 
      group_by(LOB, Clinics, OpTypeVis) %>% 
      summarise(TotalVisit = sum(visit)) %>% data.frame() %>%
      ggplot(aes(x=reorder(OpTypeVis,-TotalVisit), TotalVisit, fill=LOB)) + 
      geom_bar( stat = "identity", position = "stack", width = 0.5) +
      facet_grid(~ Clinics, scales = "free_x", space = "free_x") +
      scale_fill_discrete(name = "Line of Business") +
      theme_bw() + 
      theme(panel.spacing = unit(0,"lines"),
            strip.background = element_blank(),
            plot.title = element_text(size = 10, face = "bold", 
                                      lineheight=1,hjust = 0), 
            # axis.text.x = element_text( size = rel(1.1), angle = 90),
            legend.position = "bottom", 
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) 
    }
    else{
      TDTdat %>% 
        filter(Yr_Mo==input$fil0, Clinics==input$fil2) %>% 
        gather(key = "OPType", value = "visit", 20:21) %>% 
        mutate(OpTypeVis = ifelse(OPType=="OP_IDA", "Initial Device Application", "AGP Review")) %>% 
        group_by(LOB, Clinics, OpTypeVis) %>% 
        summarise(TotalVisit = sum(visit)) %>% data.frame() %>%
        ggplot(aes(x=reorder(OpTypeVis,-TotalVisit), TotalVisit, fill=LOB)) + 
        geom_bar( stat = "identity", position = "stack", width = 0.5) +
        facet_grid(~ Clinics, scales = "free_x", space = "free_x") +
        scale_fill_discrete(name = "Line of Business") +
        theme_bw() + 
        theme(panel.spacing = unit(0,"lines"),
              strip.background = element_blank(),
              plot.title = element_text(size = 10, face = "bold", 
                                        lineheight=1,hjust = 0), 
              # axis.text.x = element_text( size = rel(1.1), angle = 90),
              legend.position = "bottom", 
              panel.grid = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank())
    }
    
  })
  
  output$plt4 <- renderPlot({
    yer_mo <- paste(month.name, 2019)
    if (input$fil0=="All"){
      indMo = match(curMo, yer_mo)
    }
    else{
      indMo = match(input$fil0, yer_mo)
    }
    predCount <- data.frame(Quarter = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                            prjCount = c(100, 1000, 2000, 2600, 3500, 4000, 6000, 8000, 11000, 18000, 25000, 32000))
    predCntQrtr <- data.frame(Qrtr=c(3, 10, 12), 
                              Cnt = c(2000, 18000, 32000),cntstr = c("2k", "18k", "32k")) 
    textMap <- data.frame(y =c(35000, 35000, 35000), x=c(2.5, 7, 11.5))
    
    curCnt <- cbind(predCount[indMo,], data.frame(Today = "TODAY"))
    
    
    ggplot(data=predCount,aes(y=prjCount, x=Quarter)) + geom_line(size=1, color = "#2d5fa7") +
      geom_point(data=predCntQrtr,aes(x= Qrtr, y= Cnt), size=2) +
      geom_text(data=predCntQrtr, aes(x= Qrtr, y= Cnt, label = cntstr), hjust = -0.3, fontface = "bold") +
      geom_vline(xintercept = c(4, 10), linetype = "dashed" , color="gray", size = 1.5) + 
      geom_point(data=curCnt, aes(y=prjCount, x=Quarter), size = 4, shape = 17, color="#72c060") +
      geom_text(data = curCnt, aes(y=prjCount, x=Quarter, label = Today), vjust = -0.9, fontface = "bold") +
      geom_text(data = textMap, aes(y = y, x=x, label = c("Ramp Up", "Operationalize", "Scale")), size = 6, fontface = "bold") +
      scale_x_continuous(breaks = c(3,6,9,12),labels = c("Q1", "Q2", "Q3", "Q4")) +
      ylab("Projected Patient Count") + xlab("") +
      theme(panel.grid = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold")) 
    
  })
  
  output$tab3 <- renderDataTable({
    yer_mo <- paste(month.name, 2019)
    if (!input$fil0 %in% c("All", "April 2019")){
      current <- match(input$fil0,yer_mo)
      last <- match(input$fil0,yer_mo) - 1
      if(input$fil1!="All"){
        finCur <- TDTdat %>%
          filter(Yr_Mo==yer_mo[current],LOB ==input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v2=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v2)
        finlast <- TDTdat %>%
          filter(Yr_Mo==yer_mo[last], LOB == input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v1=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v1)
        progAve <- TDTdat %>%
          filter(Yr_Mo %in% yer_mo[1:current], LOB == input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v1=round(mean(Cost),1)) 
        finTab <- left_join(finlast, finCur, by="CostType")
        finTab <- left_join(finTab, progAve, by="CostType")
      }
      else{
        finCur <- TDTdat %>%
          filter(Yr_Mo==yer_mo[current], Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v2=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v2)
        finlast <- TDTdat %>%
          filter(Yr_Mo==yer_mo[last], Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v1=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v1)
        progAve <- TDTdat %>%
          filter(Yr_Mo %in% yer_mo[1:current], Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v1=round(mean(Cost),1)) 
        finTab <- left_join(finlast, finCur, by="CostType")
        finTab <- left_join(finTab, progAve, by="CostType")
      }
      datatable(finTab,colnames = c("", "Last Month", "Current Month", "Program Average"),
                options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    }
    else if(input$fil0 == "April 2019"){
      if(input$fil1!="All"){
        finTab <- TDTdat %>%
          filter(Yr_Mo==input$fil0, LOB == input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v2=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v2)
        progAve <- TDTdat %>%
          filter(Yr_Mo==input$fil0, LOB == input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v1=round(mean(Cost),1))
        finTab <- left_join(finTab, progAve, by="CostType")
      }
      else{
        finTab <- TDTdat %>%
          filter(Yr_Mo==input$fil0, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType, Yr_Mo) %>% 
          summarise(v2=round(mean(Cost),1)) %>% 
          spread(key = Yr_Mo, value = v2)
        progAve <- TDTdat %>%
          filter(Yr_Mo==input$fil0,  Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v1=round(mean(Cost),1))
        finTab <- left_join(finTab, progAve, by="CostType")
      }
      datatable(finTab,colnames = c("", "Current Month", "Program Average"),
                options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    }
    else if(input$fil0 == "All"){
      if(input$fil1!="All"){
        finTab <- TDTdat %>%
          filter(LOB == input$fil1, Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v2=round(mean(Cost),1))
      }
      else{
        finTab <- TDTdat %>%
          filter(Clinics %in% c(input$fil2)) %>%
          gather(key="CostType", value = "Cost", c(22:25)) %>% 
          mutate(CostType = case_when(CostType=="CostperVis" ~ "Cost Per Visit",
                                      CostType=="RVU" ~ "Relative Value Unit",
                                      CostType=="PerAllowance" ~ "Percentage Allowance",
                                      TRUE~ "Provider Visit per hr")) %>% 
          group_by(CostType) %>% 
          summarise(v2=round(mean(Cost),1)) 
      }
      datatable(finTab,colnames = c("", "Program Average"),
                options = list(dom = 't'), selection = list(mode = 'single'), rownames = FALSE)
    }
      
  })
  
  output$box_GMI <- shinydashboard::renderValueBox({
    if (input$fil0=="All" & input$fil1 == "All" ){
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2)
    }
    else if(input$fil0=="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1)
    }
    else if(input$fil0!="All" & input$fil1 == "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, Yr_Mo==input$fil0)
    }
    else if(input$fil0!="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1, Yr_Mo==input$fil0)
    }
    GMI = round(mean(TDTdat$GMI1),1)
    shinydashboard::valueBox(GMI, "GMI", icon = icon("bar-chart-o"), color = "yellow")
    
  })
  output$box_MedAd <- shinydashboard::renderValueBox({
    if (input$fil0=="All" & input$fil1 == "All" ){
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2)
    }
    else if(input$fil0=="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1)
    }
    else if(input$fil0!="All" & input$fil1 == "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, Yr_Mo==input$fil0)
    }
    else if(input$fil0!="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1, Yr_Mo==input$fil0)
    }
    MedAd = scales::percent(round(mean(TDTdat$Med_Adher),1), accuracy = 1)
    shinydashboard::valueBox(MedAd, "Medication Adherence", icon("stats",lib='glyphicon'), color = "blue")
    
  })
  output$box_A1c <- shinydashboard::renderValueBox({
    if (input$fil0=="All" & input$fil1 == "All" ){
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2)
    }
    else if(input$fil0=="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1)
    }
    else if(input$fil0!="All" & input$fil1 == "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, Yr_Mo==input$fil0)
    }
    else if(input$fil0!="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1, Yr_Mo==input$fil0)
    }
    A1c = round(mean(TDTdat$A1C1),1)
    shinydashboard::valueBox(A1c, "A1C Level", icon = icon("stats",lib='glyphicon'), color = "green")
  })
  output$box_TIR <- shinydashboard::renderValueBox({
    if (input$fil0=="All" & input$fil1 == "All" ){
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2)
    }
    else if(input$fil0=="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1)
    }
    else if(input$fil0!="All" & input$fil1 == "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, Yr_Mo==input$fil0)
    }
    else if(input$fil0!="All" & input$fil1 != "All") {
      TDTdat <- TDTdat %>% filter(Clinics %in% input$fil2, LOB==input$fil1, Yr_Mo==input$fil0)
    }
    TIR = scales::percent(round(mean(TDTdat$TIR1),1), accuracy = 1)
    shinydashboard::valueBox(TIR, "Time in Range", icon = icon("area-chart"), color = "teal")
  })
  
  
}

shinyApp(ui, server)









