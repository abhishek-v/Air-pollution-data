library("shinydashboard")
library(ggplot2)
library(leaflet)
library("reshape2")
library(DT)

curr_state <- "Alabama"
curr_county <- ""
curr_ip <- ""

#------------GLOBAL VARIABLES FOR COMPARSION MODE - - - - - -- - -  - - - - #

curr_state1 <- "Alabama"
curr_county1 <- ""

curr_state2 <- "Alabama"
curr_county2 <- ""

curr_state3 <- "Alabama"
curr_county3 <- ""

# - -- - - - - - - -- -- -  -- -- --- - -- - -- --- -- -- - - - - - - - -- -#

#Read in data from files
setwd("./data")
file_list = list.files(pattern="*.csv")
df_list = lapply(file_list, read.csv)

#Get list of states from latest file to be used in selectinput
df <- df_list[[39]] #year 2018
states <- unique(df$State)

master_df <- data.frame()
for(year_df in df_list)
{
  master_df = merge(master_df,year_df,all=TRUE)
}

master_states <- list()
i <- 1
for(state in states)
{
  temp <- subset(master_df,State==state & State!="<NA>")
  temp2 <- as.character(unique(temp$County))
  master_states[[i]] <- sort(temp2)
  
  i <- i + 1
}
names(master_states) <- states

#get data for longitudes and latitudes
map_df <- read.csv("../aqs_sites.csv")
# names(map_df) <- sub(" ", ".", names(map_df))
reqd <- c("Latitude","Longitude","County.Name","State.Name")
#drop all columns except these
map_df <- map_df[,reqd]

print(names(master_df))





ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Air pollution stats in the US",titleWidth = 550),
  dashboardSidebar(
    tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 30pt; }")),
    width=550,
    sidebarMenu(
      id="menu",
      menuItem("Single county mode",icon= icon("calendar"),
               div(style="width: 900px;",sliderInput("year", "Select year:", min=1980, max=2018, value=2018,sep="")),
               div(style="width: 700px;",selectInput(inputId = "state","Select State:",choices=states)),
               div(style="width: 700px;",selectInput(inputId = "county","Select County:",choices=NULL)),
               div(style="width: 700px;",textInput(inputId="search","Search county:")))
    ),
    menuItem("Comparison mode",icon= icon("th"),
             sliderInput("year1", "County 1:", min=1980, max=2018, value=2018,sep=""),
             selectInput(inputId = "state1","Select State",choices=states),
             selectInput(inputId = "county1",NULL,choices=NULL),
             sliderInput("year2", "County 2:", min=1980, max=2018, value=2018,sep=""),
             selectInput(inputId = "state2","Select State",choices=states),
             selectInput(inputId = "county2",NULL,choices=NULL),
             sliderInput("year3", "County 3:", min=1980, max=2018, value=2018,sep=""),
             selectInput(inputId = "state3","Select State",choices=states),
             selectInput(inputId = "county3",NULL,choices=NULL)
    )
  ),
  dashboardBody(
    tags$head(includeCSS(path ="../www/style.css")),
    tabsetPanel(type="tabs",
                tabPanel("Single county mode",
                         tabsetPanel(type="tabs",
                          tabPanel("AQI details",
                         fluidRow(
                           box(
                             width = 2, height = 650,
                             div(tableOutput(outputId = "aqi_table"), style = "font-size:40px")
                           ),
                           box(
                             width = 3, height = 1200,
                             plotOutput(outputId = "aqi_pie",width = "1000", height = "1200")
                           ),
                           box(
                             width = 3, height = 1200,
                             plotOutput(outputId = "aqi_bar",width = "1000", height = "1200")
                           ),
                           box(
                             width = 4, height = 1200,
                             plotOutput(outputId = "mpa_graph",width = "1400", height = "1200") #median, 90th percentile and max AQI
                           )
                         
                           
                         )#end of first row
                          ),
                         tabPanel("Pollutants overview",
                                  fluidRow(
                                    box(
                                      width = 5, height = 1200,
                                      div(dataTableOutput(outputId = "pollutants_table"), style = "font-size:40px") #number of days for each pollutant
                                       ),
                                    box(
                                      width = 3, height = 1200,
                                      plotOutput(outputId = "pollutants_bar",width = "1000", height = "1200") #bar chart
                                    )
                                    ,
                                    box(
                                      width = 4, height = 1200,
                                      plotOutput(outputId = "pollutants_graph",width = "1400", height = "1200") #6 line graph
                                    )
                                  )
                                  ),
                         tabPanel("Individual Pollutants",
                                  fluidRow(
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "so2",width = "1500", height = "600") #median, SO2 as main pollutant
                                    )
                                    ,
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "no2",width = "1500", height = "600") #median, SO2 as main pollutant
                                    ),
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "co",width = "1500", height = "600") #median, CO as main pollutant
                                    )
                                  ),
                                  fluidRow(
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "ozone",width = "1500", height = "600") #median, Ozone as main pollutant
                                    )
                                    ,
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "pm25",width = "1500", height = "600") #median, PM2.5 as main pollutant
                                    )
                                    ,
                                    box(
                                      width = 4, height = 600,
                                      plotOutput(outputId = "pm10",width = "1500", height = "600") #median, PM10 as main pollutant
                                    )
                                  )
                                  
                         ),
                         tabPanel("County info",
                                  #insert county info here
                                  box(
                                    width=4,
                                    htmlOutput(outputId = "county_info")
                                  ),
     
                                  box(
                                    width = 5, height = 1200,
                                    leafletOutput("map",width = "1900", height = "1200")
                                  ))
                         
                         
                  )#end of main tabset panel
                         
                         
                ),
                
                ###UNUSED GRAPHS####
                # box(
                #   width = 3, height = 1100,
                #   plotOutput(outputId = "aqi_pie_1",width = "2000", height = "1100")
                # ),
                # box(
                #   width = 3, height = 1100,
                #   plotOutput(outputId = "aqi_pie_2",width = "2000", height = "1100")
                # ),
                # box(
                #   width = 3, height = 1100,
                #   plotOutput(outputId = "aqi_pie_3",width = "2000", height = "1100")
                # )
                tabPanel("comparison",
                         tabsetPanel(type="tabs",
                                     tabPanel("AQI index",
                         fluidRow(
                           box(
                             width = 3, height = 500,
                             div(tableOutput(outputId = "aqi_table_1"),style = "font-size:40px")
                           ),
                           box(
                             width = 3, height = 500,
                             div(tableOutput(outputId = "aqi_table_2"), style = "font-size:40px")
                           ),
                           box(
                             width = 3, height = 500,
                             div(tableOutput(outputId = "aqi_table_3"), style = "font-size:40px")
                           )
                           
                         ),
                         fluidRow(
                           box(
                             width = 3, height = 700,
                             plotOutput(outputId = "aqi_bar_1",width = "1000", height = "700")
                           ),
                           box(
                             width = 3, height = 700,
                             plotOutput(outputId = "aqi_bar_2",width = "1000", height = "700")
                           ),
                           box(
                             width = 3, height = 700,
                             plotOutput(outputId = "aqi_bar_3",width = "1000", height = "700")
                           )

                         )
                         ),
                         tabPanel("Pollutants overview",
                         fluidRow(
                           
                           selectInput(inputId = "graph_choice","Select time series",choices=c("Pollutants","AQI"))
                           
                         ),
                         fluidRow(
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county1_graph",width = "1300", height = "1000")
                           ),
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county2_graph",width = "1300", height = "1000")
                           ),
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county3_graph",width = "1300", height = "1000")
                           )


                         )
                         ),
                         tabPanel("Individual pollutants",
                         fluidRow(
                           selectInput(inputId = "pollutant_choice","Select pollutant",choices=c("CO","NO2","SO2","PM2.5","PM10","Ozone"))
                         ),
                         fluidRow(
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county1_pollutant",width = "1300", height = "700")
                           ),
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county2_pollutant",width = "1300", height = "700")
                           ),
                           box(
                             width = 4, height = 900,
                             plotOutput(outputId = "county3_pollutant",width = "1300", height = "700")
                           )
                         )
                         )
                         )
                         
                ),
                tabPanel("About app",
                         includeHTML("../info.txt")
                )
    )
  )
)




#SERVER SIDE CODE


server <- function(input,output,session){
  
  
  counties <- master_states[[curr_state]][grepl(paste("",sep=""), master_states[[curr_state]], ignore.case=T)]
  updateSelectInput(session,inputId="county","Select County:",choices=counties,selected=counties[1])
  print("Initialized data")
  curr_county<<-counties[1]
  updateSelectInput(session,inputId="county1",NULL,choices=counties,selected=counties[1])
  updateSelectInput(session,inputId="county2",NULL,choices=counties,selected=counties[1])
  updateSelectInput(session,inputId="county3",NULL,choices=counties,selected=counties[1])
  curr_county1<<-counties[1]
  curr_county2<<-counties[1]
  curr_county3<<-counties[1]
  
  
  observe({
    print(input$county)
    print(input$state)
    print(input$search)
    if(input$state != curr_state)
    {
      counties <- master_states[[input$state]][grepl(paste("^",trimws(input$search),sep=""), master_states[[input$state]], ignore.case=T)]
      curr_state <<- input$state
      curr_county <<- counties[1]
      updateSelectInput(session,inputId="county","Select County:",choices=counties,selected=counties[1])
      req(FALSE, cancelOutput=TRUE)
    }
    else if(input$search != curr_ip)
    {
      counties <- master_states[[input$state]][grepl(paste("^",input$search,sep=""), master_states[[input$state]], ignore.case=T)]
      curr_ip <<- input$search
      curr_county <<- counties[1]
      updateSelectInput(session,inputId="county","Select County:",choices=counties,selected=counties[1])
      
    }
    else if(input$county != curr_county)
    {
      
      curr_county <<- input$county
      print(paste("changed county to :",curr_county))
    }
    
  })
  
  
  
  getTableData <- reactive({
    
    z <- input$state
    zz <- input$county
    df <- df_list[[(input$year+38-2018)+1]]
    if(input$search != "")
      counties <- master_states[[input$state]][grepl(paste("^",trimws(input$search),sep=""), master_states[[input$state]], ignore.case=T)]
    else
      counties <- master_states[[input$state]][grepl(paste("^",trimws(""),sep=""), master_states[[input$state]], ignore.case=T)]
    
    print(counties)
    
    if(identical(counties, character(0)))
    {
      emp <- data.frame()
      "empty"
    }
    
    else
    {
      temp1 <- subset(master_df,State==input$state & County==input$county)
      d <- temp1[,c("Year","Days.CO","Days.NO2","Days.Ozone","Days.SO2","Days.PM2.5","Days.PM10")]
      d
    }
    
  })
  
  getLineData <- reactive({
    
    z <- input$state
    zz <- input$county
    zzz <- input$search
    
    df <- df_list[[(input$year+38-2018)+1]]
    
    if(input$search != "")
      counties <- master_states[[input$state]][grepl(paste("^",trimws(input$search),sep=""), master_states[[input$state]], ignore.case=T)]
    else
      counties <- master_states[[input$state]][grepl(paste("^",trimws(""),sep=""), master_states[[input$state]], ignore.case=T)]
    
    print(counties)
    
    if(identical(counties, character(0)))
    {
      emp <- data.frame()
      "empty"
    }
    
    else
    {
      temp1 <- subset(master_df,State==input$state & County==input$county)
      p901 <- temp1[,c("Year","X90th.Percentile.AQI")]

      max_aqi1 <- temp1[,c("Year","Max.AQI")]
      
      median1 <- temp1[,c("Year","Median.AQI")]

      
      
      # temp1 <- subset(master_df,State==input$state & County==input$county)
      # median <- temp1$Median.AQI
      # years = 1980:2018
      # median1 <- rep(NA, 2018-1980)
      # i <- 1
      # for(year in years){
      #   temp <- (year-1980) + 1
      #   median1[temp] <- median[i]
      #   i <- i + 1
      # }
      # 
      # p90 <- temp1$X90th.Percentile.AQI
      # years = 1980:2018
      # p901 <- rep(NA, 2018-1980)
      # i <- 1
      # for(year in years){
      #   temp <- (year-1980) + 1
      #   p901[temp] <- p90[i]
      #   i <- i + 1
      # }
      # 
      # max_aqi <- temp1$Max.AQI
      # years = 1980:2018
      # max_aqi1 <- rep(NA, 2018-1980)
      # i <- 1
      # for(year in years){
      #   temp <- (year-1980) + 1
      #   max_aqi1[temp] <- max_aqi[i]
      #   i <- i + 1
      # }
      # 
      # 
      # p901 <- data.frame(years,p901)
      # median1 <- data.frame(years,median1)
      # max_aqi1 <- data.frame(years,max_aqi1)
      
      names(median1) = c("years", "median AQI")
      names(p901) = c("years", "90th.percentile")
      names(max_aqi1) = c("years", "Max AQI")

      line1<-merge(p901,median1,by="years")
      line1<-merge(line1,max_aqi1,by="years")
      line1_melted <- reshape2::melt(line1, id.var='years')
      
      print(line1_melted)
      print(median1$years)
      df2 <- data.frame(median1$years, line1_melted)
      
      #----------------------POLLUTANTS LINE CHART--------------------------#
      
      #---------SO2 -----#
      line_so2 <- temp1$Days.SO2
      line_so2_1 <- temp1[,c("Year","Days.SO2")]

      #---------NO2 -----#
      line_no2 <- temp1$Days.NO2
      line_no2_1 <- temp1[,c("Year","Days.NO2")]
      
      #---------Ozone -----#
      line_ozone <- temp1$Days.Ozone
      line_ozone_1 <- temp1[,c("Year","Days.Ozone")]

      #---------CO -----#
      line_co <- temp1$Days.CO
      line_co_1 <- temp1[,c("Year","Days.CO")]

      #---------PM2.5 -----#
      line_pm25 <- temp1$"Days.PM2.5"
      line_pm25_1 <- temp1[,c("Year","Days.PM2.5")]

      #---------PM10-----#
      line_pm10 <- temp1$"Days.PM10"
      line_pm10_1 <- temp1[,c("Year","Days.PM10")]
      
      names(line_so2_1) = c("years", "SO2")
      names(line_no2_1) = c("years", "NO2")
      names(line_ozone_1) = c("years", "Ozone")
      names(line_co_1) = c("years", "CO")
      names(line_pm25_1) = c("years", "PM2.5")
      names(line_pm10_1) = c("years", "PM10")
      
      line2<-merge(line_co_1,line_no2_1,by="years")
      line2<-merge(line2,line_ozone_1,by="years")
      line2<-merge(line2,line_pm10_1,by="years")
      line2<-merge(line2,line_pm25_1,by="years")
      line2<-merge(line2,line_so2_1,by="years")
      
      line2_melted <- reshape2::melt(line2, id.var='years')
      
      df3 <- data.frame(line_so2_1$years, line2_melted)
      
      l <- list(df2,df3)
      l
    }
  })
  
  ##COUNTY 1
  observe({
    a <- curr_county1
    b <- curr_state1
    print(paste("hello",input$state1,input$county1))
    
    if(input$state1 != curr_state1)
    {
      counties <- master_states[[input$state1]][grepl(paste("",sep=""), master_states[[input$state1]], ignore.case=T)]
      print(paste("counties",counties))
      curr_state1 <<- input$state1
      curr_county1 <<- counties[1]
      updateSelectInput(session,inputId="county1",NULL,choices=counties,selected=counties[1])
      req(FALSE, cancelOutput=TRUE)
    }
    else if(input$county1 != curr_county1)
    {
      curr_county1 <<- input$county1
      print(paste("changed county to :",curr_county1))
    }
    
  })
  
  
  getLineData1 <- reactive({
    
    z <- input$state1
    zz <- input$county1
    
    
    df <- df_list[[(input$year1+38-2018)+1]]
    
    counties <- master_states[[input$state1]][grepl(paste("^",trimws(""),sep=""), master_states[[input$state1]], ignore.case=T)]
    
    if(identical(counties, character(0)))
    {
      emp <- data.frame()
      "empty"
    }
    
    else
    {
      temp1 <- subset(master_df,State==input$state1 & County==input$county1)
      if(input$graph_choice == "AQI")
      {
        p901 <- temp1[,c("Year","X90th.Percentile.AQI")]
        
        max_aqi1 <- temp1[,c("Year","Max.AQI")]
        
        median1 <- temp1[,c("Year","Median.AQI")]
        
        
        names(median1) = c("years", "median AQI")
        names(p901) = c("years", "90th.percentile")
        names(max_aqi1) = c("years", "Max AQI")
        
        line1<-merge(p901,median1,by="years")
        line1<-merge(line1,max_aqi1,by="years")
        line1_melted <- reshape2::melt(line1, id.var='years')
        
        print(line1_melted)
        print(median1$years)
        df_final <- data.frame(median1$years, line1_melted)
      }
      
      #----------------------POLLUTANTS LINE CHART--------------------------#
      else
      {
        #---------SO2 -----#
        line_so2 <- temp1$Days.SO2
        line_so2_1 <- temp1[,c("Year","Days.SO2")]
        
        #---------NO2 -----#
        line_no2 <- temp1$Days.NO2
        line_no2_1 <- temp1[,c("Year","Days.NO2")]
        
        #---------Ozone -----#
        line_ozone <- temp1$Days.Ozone
        line_ozone_1 <- temp1[,c("Year","Days.Ozone")]
        
        #---------CO -----#
        line_co <- temp1$Days.CO
        line_co_1 <- temp1[,c("Year","Days.CO")]
        
        #---------PM2.5 -----#
        line_pm25 <- temp1$"Days.PM2.5"
        line_pm25_1 <- temp1[,c("Year","Days.PM2.5")]
        
        #---------PM10-----#
        line_pm10 <- temp1$"Days.PM10"
        line_pm10_1 <- temp1[,c("Year","Days.PM10")]
        
        names(line_so2_1) = c("years", "SO2")
        names(line_no2_1) = c("years", "NO2")
        names(line_ozone_1) = c("years", "Ozone")
        names(line_co_1) = c("years", "CO")
        names(line_pm25_1) = c("years", "PM2.5")
        names(line_pm10_1) = c("years", "PM10")
        
        line2<-merge(line_so2_1,line_no2_1,by="years")
        line2<-merge(line2,line_co_1,by="years")
        line2<-merge(line2,line_pm25_1,by="years")
        line2<-merge(line2,line_pm10_1,by="years")
        line2<-merge(line2,line_ozone_1,by="years")
        
        line2_melted <- reshape2::melt(line2, id.var='years')
        
        df_final <- data.frame(line_so2_1$years, line2_melted)
      }
      df_final
    }
  })
  
  getData1 <- reactive({
    zz1 <- input$county1
    df <- df_list[[(input$year1+38-2018)+1]]
    temp_df <- subset(df,County==input$county1 & State==input$state1)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
      value = value1/temp_df[["Days.with.AQI"]]
      df1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value1
      )
      df1$group <- factor(df1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(df1) <- c("Air.Quality","Number.of.days")
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      value1$group <- factor(value1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(value1) <- c("Air.Quality","Percentage.Number.of.days")
      list(value1,df1)
    }
  })
  
  getPollutant1 <- reactive({
    zz1 <- input$county1
    zz <- input$state1
    df <- df_list[[(input$year1+38-2018)+1]]
    temp_df <- subset(df,County==input$county1 & State==input$state1)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      #1) Calculating percentage of days
      
      #---------------#
      
      #beginning of percentage calculations for pollutants for particular year
      total_days_df <- subset(df,County==input$county1 & State==input$state1)
      total_days <- total_days_df$Days.with.AQI
      
      if(input$pollutant_choice=="SO2")
      {
        # #SO2
        a0 <- total_days_df$Days.SO2
        poll_so2 <- a0*100/total_days
        # 
        final <- tryCatch({
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(poll_so2,100-poll_so2))
        },warning=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        },error=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        }
        )
        names(final) <- c("pollutants", "percentage")
      }
      #CO
      if(input$pollutant_choice=="CO")
      {
        a1 <- total_days_df$Days.CO
        poll_co <- a1*100/total_days
        final <- data.frame(pollutants=c("CO as main pollutant"," "),percentage=c(poll_co,100-poll_co))
        
        names(final) <- c("pollutants", "percentage")
      }
      
      #Ozone
      if(input$pollutant_choice=="Ozone")
      {
        a2 <- total_days_df$Days.Ozone
        poll_ozone <- a2*100/total_days
        
        final <- data.frame(pollutants=c("Ozone as main pollutant"," "),percentage=c(poll_ozone,100-poll_ozone))
        names(final) <- c("pollutants", "percentage")
      }
      #NO2
      if(input$pollutant_choice=="NO2")
      {
        a3 <- total_days_df$Days.NO2
        poll_no2 <- a3*100/total_days
        
        final <- data.frame(pollutants=c("NO2 as main pollutant"," "),percentage=c(poll_no2,100-poll_no2))
        names(final) <- c("pollutants", "percentage")
      }
      #PM2.5
      if(input$pollutant_choice=="PM2.5")
      {
        a4 <- total_days_df$"Days.PM2.5"
        poll_pm25 <- (a4)*100/total_days
        
        final <- data.frame(pollutants=c("PM2.5 as main pollutant"," "),percentage=c(poll_pm25,100-poll_pm25))
        names(final) <- c("pollutants", "percentage")
      }
      #PM10
      if(input$pollutant_choice=="PM10")
      {
        a5 <- total_days_df$Days.PM10
        poll_pm10 <- a5*100/total_days
        
        final <- data.frame(pollutants=c("PM10 as main pollutant"," "),percentage=c(poll_pm10,100-poll_pm10))
        names(final) <- c("pollutants", "percentage")
      }
      
      final
      }
    #end of else
    
    
    
  })
  
  ##### END OF COUNTY1
  
  ##COUNTY 2
  observe({
    a <- curr_county2
    b <- curr_state2
    print(paste("hello",input$state2,input$county2))
    
    if(input$state2 != curr_state2)
    {
      counties <- master_states[[input$state2]][grepl(paste("",sep=""), master_states[[input$state2]], ignore.case=T)]
      print(paste("counties",counties))
      curr_state2 <<- input$state2
      curr_county2 <<- counties[1]
      updateSelectInput(session,inputId="county2",NULL,choices=counties,selected=counties[1])
      req(FALSE, cancelOutput=TRUE)
    }
    else if(input$county2 != curr_county2)
    {
      curr_county2 <<- input$county2
      print(paste("changed county to :",curr_county2))
    }
    
  })
  
  getLineData2 <- reactive({
    
    z <- input$state2
    zz <- input$county2
    
    
    df <- df_list[[(input$year2+38-2018)+1]]
    
    counties <- master_states[[input$state2]][grepl(paste("^",trimws(""),sep=""), master_states[[input$state2]], ignore.case=T)]
    
    if(identical(counties, character(0)))
    {
      emp <- data.frame()
      "empty"
    }
    
    else
    {
      temp1 <- subset(master_df,State==input$state2 & County==input$county2)
      if(input$graph_choice == "AQI")
      {
        p901 <- temp1[,c("Year","X90th.Percentile.AQI")]
        
        max_aqi1 <- temp1[,c("Year","Max.AQI")]
        
        median1 <- temp1[,c("Year","Median.AQI")]
        
        
        names(median1) = c("years", "median AQI")
        names(p901) = c("years", "90th.percentile")
        names(max_aqi1) = c("years", "Max AQI")
        
        line1<-merge(p901,median1,by="years")
        line1<-merge(line1,max_aqi1,by="years")
        line1_melted <- reshape2::melt(line1, id.var='years')
        
        print(line1_melted)
        print(median1$years)
        df_final <- data.frame(median1$years, line1_melted)
      }
      
      #----------------------POLLUTANTS LINE CHART--------------------------#
      else
      {
        #---------SO2 -----#
        line_so2 <- temp1$Days.SO2
        line_so2_1 <- temp1[,c("Year","Days.SO2")]
        
        #---------NO2 -----#
        line_no2 <- temp1$Days.NO2
        line_no2_1 <- temp1[,c("Year","Days.NO2")]
        
        #---------Ozone -----#
        line_ozone <- temp1$Days.Ozone
        line_ozone_1 <- temp1[,c("Year","Days.Ozone")]
        
        #---------CO -----#
        line_co <- temp1$Days.CO
        line_co_1 <- temp1[,c("Year","Days.CO")]
        
        #---------PM2.5 -----#
        line_pm25 <- temp1$"Days.PM2.5"
        line_pm25_1 <- temp1[,c("Year","Days.PM2.5")]
        
        #---------PM10-----#
        line_pm10 <- temp1$"Days.PM10"
        line_pm10_1 <- temp1[,c("Year","Days.PM10")]
        
        names(line_so2_1) = c("years", "SO2")
        names(line_no2_1) = c("years", "NO2")
        names(line_ozone_1) = c("years", "Ozone")
        names(line_co_1) = c("years", "CO")
        names(line_pm25_1) = c("years", "PM2.5")
        names(line_pm10_1) = c("years", "PM10")
        
        line2<-merge(line_so2_1,line_no2_1,by="years")
        line2<-merge(line2,line_co_1,by="years")
        line2<-merge(line2,line_pm25_1,by="years")
        line2<-merge(line2,line_pm10_1,by="years")
        line2<-merge(line2,line_ozone_1,by="years")
        
        line2_melted <- reshape2::melt(line2, id.var='years')
        
        df_final <- data.frame(line_so2_1$years, line2_melted)
      }
      df_final
    }
  })
  
  getData2 <- reactive({
    zz2 <- input$county2
    df <- df_list[[(input$year2+38-2018)+1]]
    temp_df <- subset(df,County==input$county2 & State==input$state2)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
      value = value1/temp_df[["Days.with.AQI"]]
      df1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value1
      )
      df1$group <- factor(df1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(df1) <- c("Air.Quality","Number.of.days")
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      value1$group <- factor(value1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(value1) <- c("Air.Quality","Percentage.Number.of.days")
      list(value1,df1)
    }
  })
  
  getPollutant2 <- reactive({
    zz1 <- input$county2
    zz <- input$state2
    df <- df_list[[(input$year2+38-2018)+1]]
    temp_df <- subset(df,County==input$county2 & State==input$state2)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      #1) Calculating percentage of days
      
      #---------------#
      
      #beginning of percentage calculations for pollutants for particular year
      total_days_df <- subset(df,County==input$county2 & State==input$state2)
      total_days <- total_days_df$Days.with.AQI
      
      if(input$pollutant_choice=="SO2")
      {
        # #SO2
        a0 <- total_days_df$Days.SO2
        poll_so2 <- a0*100/total_days
        # 
        final <- tryCatch({
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(poll_so2,100-poll_so2))
        },warning=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        },error=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        }
        )
        names(final) <- c("pollutants", "percentage")
      }
      #CO
      if(input$pollutant_choice=="CO")
      {
        a1 <- total_days_df$Days.CO
        poll_co <- a1*100/total_days
        final <- data.frame(pollutants=c("CO as main pollutant"," "),percentage=c(poll_co,100-poll_co))
        
        names(final) <- c("pollutants", "percentage")
      }
      
      #Ozone
      if(input$pollutant_choice=="Ozone")
      {
        a2 <- total_days_df$Days.Ozone
        poll_ozone <- a2*100/total_days
        
        final <- data.frame(pollutants=c("Ozone as main pollutant"," "),percentage=c(poll_ozone,100-poll_ozone))
        names(final) <- c("pollutants", "percentage")
      }
      #NO2
      if(input$pollutant_choice=="NO2")
      {
        a3 <- total_days_df$Days.NO2
        poll_no2 <- a3*100/total_days
        
        final <- data.frame(pollutants=c("NO2 as main pollutant"," "),percentage=c(poll_no2,100-poll_no2))
        names(final) <- c("pollutants", "percentage")
      }
      #PM2.5
      if(input$pollutant_choice=="PM2.5")
      {
        a4 <- total_days_df$"Days.PM2.5"
        poll_pm25 <- (a4)*100/total_days
        
        final <- data.frame(pollutants=c("PM2.5 as main pollutant"," "),percentage=c(poll_pm25,100-poll_pm25))
        names(final) <- c("pollutants", "percentage")
      }
      #PM10
      if(input$pollutant_choice=="PM10")
      {
        a5 <- total_days_df$Days.PM10
        poll_pm10 <- a5*100/total_days
        
        final <- data.frame(pollutants=c("PM10 as main pollutant"," "),percentage=c(poll_pm10,100-poll_pm10))
        names(final) <- c("pollutants", "percentage")
      }
      
      final
    }
    #end of else
    
    
    
  })
  ##### END OF COUNTY2
  
  
  ##COUNTY 3
  observe({
    a <- curr_county3
    b <- curr_state3
    print(paste("hello",input$state3,input$county3))
    
    if(input$state3 != curr_state3)
    {
      counties <- master_states[[input$state3]][grepl(paste("",sep=""), master_states[[input$state3]], ignore.case=T)]
      print(paste("counties",counties))
      curr_state3 <<- input$state3
      curr_county3 <<- counties[1]
      updateSelectInput(session,inputId="county3",NULL,choices=counties,selected=counties[1])
      req(FALSE, cancelOutput=TRUE)
    }
    else if(input$county3 != curr_county3)
    {
      curr_county3 <<- input$county3
      print(paste("changed county to :",curr_county3))
    }
    
  })
  
  getLineData3 <- reactive({
    
    z <- input$state3
    zz <- input$county3
    
    
    df <- df_list[[(input$year3+38-2018)+1]]
    
    counties <- master_states[[input$state3]][grepl(paste("^",trimws(""),sep=""), master_states[[input$state3]], ignore.case=T)]
    
    if(identical(counties, character(0)))
    {
      emp <- data.frame()
      "empty"
    }
    
    else
    {
      temp1 <- subset(master_df,State==input$state3 & County==input$county3)
      if(input$graph_choice == "AQI")
      {
        p901 <- temp1[,c("Year","X90th.Percentile.AQI")]
        
        max_aqi1 <- temp1[,c("Year","Max.AQI")]
        
        median1 <- temp1[,c("Year","Median.AQI")]
        
        
        names(median1) = c("years", "median AQI")
        names(p901) = c("years", "90th.percentile")
        names(max_aqi1) = c("years", "Max AQI")
        
        line1<-merge(p901,median1,by="years")
        line1<-merge(line1,max_aqi1,by="years")
        line1_melted <- reshape2::melt(line1, id.var='years')
        
        print(line1_melted)
        print(median1$years)
        df_final <- data.frame(median1$years, line1_melted)
      }
      
      #----------------------POLLUTANTS LINE CHART--------------------------#
      else
      {
        #---------SO2 -----#
        line_so2 <- temp1$Days.SO2
        line_so2_1 <- temp1[,c("Year","Days.SO2")]
        
        #---------NO2 -----#
        line_no2 <- temp1$Days.NO2
        line_no2_1 <- temp1[,c("Year","Days.NO2")]
        
        #---------Ozone -----#
        line_ozone <- temp1$Days.Ozone
        line_ozone_1 <- temp1[,c("Year","Days.Ozone")]
        
        #---------CO -----#
        line_co <- temp1$Days.CO
        line_co_1 <- temp1[,c("Year","Days.CO")]
        
        #---------PM2.5 -----#
        line_pm25 <- temp1$"Days.PM2.5"
        line_pm25_1 <- temp1[,c("Year","Days.PM2.5")]
        
        #---------PM10-----#
        line_pm10 <- temp1$"Days.PM10"
        line_pm10_1 <- temp1[,c("Year","Days.PM10")]
        
        names(line_so2_1) = c("years", "SO2")
        names(line_no2_1) = c("years", "NO2")
        names(line_ozone_1) = c("years", "Ozone")
        names(line_co_1) = c("years", "CO")
        names(line_pm25_1) = c("years", "PM2.5")
        names(line_pm10_1) = c("years", "PM10")
        
        line2<-merge(line_so2_1,line_no2_1,by="years")
        line2<-merge(line2,line_co_1,by="years")
        line2<-merge(line2,line_pm25_1,by="years")
        line2<-merge(line2,line_pm10_1,by="years")
        line2<-merge(line2,line_ozone_1,by="years")
        
        line2_melted <- reshape2::melt(line2, id.var='years')
        
        df_final <- data.frame(line_so2_1$years, line2_melted)
      }
      df_final
    }
  })
  
  getData3 <- reactive({
    zz3 <- input$county3
    df <- df_list[[(input$year3+38-2018)+1]]
    temp_df <- subset(df,County==input$county3 & State==input$state3)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
      value = value1/temp_df[["Days.with.AQI"]]
      df1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value1
      )
      df1$group <- factor(df1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      
      names(df1) <- c("Air.Quality","Number.of.days")
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      value1$group <- factor(value1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      
      names(value1) <- c("Air.Quality","Percentage.Number.of.days")
      list(value1,df1)
    }
  })
  
  getPollutant3 <- reactive({
    zz1 <- input$county3
    zz <- input$state3
    df <- df_list[[(input$year3+38-2018)+1]]
    temp_df <- subset(df,County==input$county3 & State==input$state3)
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      #1) Calculating percentage of days
      
      #---------------#
      
      #beginning of percentage calculations for pollutants for particular year
      total_days_df <- subset(df,County==input$county3 & State==input$state3)
      total_days <- total_days_df$Days.with.AQI
      
      if(input$pollutant_choice=="SO2")
      {
        # #SO2
        a0 <- total_days_df$Days.SO2
        poll_so2 <- a0*100/total_days
        # 
        final <- tryCatch({
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(poll_so2,100-poll_so2))
        },warning=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        },error=function(cond) {
          data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
        }
        )
        names(final) <- c("pollutants", "percentage")
      }
      #CO
      if(input$pollutant_choice=="CO")
      {
        a1 <- total_days_df$Days.CO
        poll_co <- a1*100/total_days
        final <- data.frame(pollutants=c("CO as main pollutant"," "),percentage=c(poll_co,100-poll_co))
        
        names(final) <- c("pollutants", "percentage")
      }
      
      #Ozone
      if(input$pollutant_choice=="Ozone")
      {
        a2 <- total_days_df$Days.Ozone
        poll_ozone <- a2*100/total_days
        
        final <- data.frame(pollutants=c("Ozone as main pollutant"," "),percentage=c(poll_ozone,100-poll_ozone))
        names(final) <- c("pollutants", "percentage")
      }
      #NO2
      if(input$pollutant_choice=="NO2")
      {
        a3 <- total_days_df$Days.NO2
        poll_no2 <- a3*100/total_days
        
        final <- data.frame(pollutants=c("NO2 as main pollutant"," "),percentage=c(poll_no2,100-poll_no2))
        names(final) <- c("pollutants", "percentage")
      }
      #PM2.5
      if(input$pollutant_choice=="PM2.5")
      {
        a4 <- total_days_df$"Days.PM2.5"
        poll_pm25 <- (a4)*100/total_days
        
        final <- data.frame(pollutants=c("PM2.5 as main pollutant"," "),percentage=c(poll_pm25,100-poll_pm25))
        names(final) <- c("pollutants", "percentage")
      }
      #PM10
      if(input$pollutant_choice=="PM10")
      {
        a5 <- total_days_df$Days.PM10
        poll_pm10 <- a5*100/total_days
        
        final <- data.frame(pollutants=c("PM10 as main pollutant"," "),percentage=c(poll_pm10,100-poll_pm10))
        names(final) <- c("pollutants", "percentage")
      }
      
      final
    }
    #end of else
    
    
    
  })
  ##### END OF COUNTY3
  
  getData <- reactive({
    
    zz <- input$search
    zz1 <- input$county
    
    df <- df_list[[(input$year+38-2018)+1]]
    
    if(input$search != "")
      temp_df <- subset(df,County==curr_county & State==input$state)
    else
      temp_df <- subset(df,County==input$county & State==input$state)
    
    
    if(is.data.frame(temp_df) && nrow(temp_df)==0)
    {
      emp <- data.frame()
      "empty"
    }
    else
    {
      #1) Calculating percentage of days
      value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
      value = value1/temp_df[["Days.with.AQI"]]
      df1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value1
      )
      print(df1)
      print(levels(df1))
      df1$group <- factor(df1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(df1) <- c("Air.Quality","Number.of.days")
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      value1$group <- factor(value1$group, levels = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"))
      names(value1) <- c("Air.Quality","Percentage.Number.of.days")

      
      
      #---------------#
      
      #beginning of percentage calculations for pollutants for particular year
      total_days_df <- subset(df,County==input$county & State==input$state)
      total_days <- total_days_df$Days.with.AQI
      
      # #SO2
      a0 <- total_days_df$Days.SO2
      poll_so2 <- a0*100/total_days
      # 
      poll_so2 <- tryCatch({
        data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(poll_so2,100-poll_so2))
      },warning=function(cond) {
        data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
      },error=function(cond) {
        data.frame(pollutants=c("SO2 as main pollutant"," "),percentage=c(0,0))
      }
      )
      names(poll_so2) <- c("pollutants", "percentage")
      #CO
      a1 <- total_days_df$Days.CO
      poll_co <- a1*100/total_days
      poll_co <- data.frame(pollutants=c("CO as main pollutant"," "),percentage=c(poll_co,100-poll_co))
      
      names(poll_co) <- c("pollutants", "percentage")
      
      #Ozone
      a2 <- total_days_df$Days.Ozone
      poll_ozone <- a2*100/total_days
      
      poll_ozone <- data.frame(pollutants=c("Ozone as main pollutant"," "),percentage=c(poll_ozone,100-poll_ozone))
      names(poll_ozone) <- c("pollutants", "percentage")
      
      #NO2
      a3 <- total_days_df$Days.NO2
      poll_no2 <- a3*100/total_days
      
      poll_no2 <- data.frame(pollutants=c("NO2 as main pollutant"," "),percentage=c(poll_no2,100-poll_no2))
      names(poll_no2) <- c("pollutants", "percentage")
      
      #PM2.5
      a4 <- total_days_df$"Days.PM2.5"
      poll_pm25 <- (a4)*100/total_days
      
      poll_pm25 <- data.frame(pollutants=c("PM2.5 as main pollutant"," "),percentage=c(poll_pm25,100-poll_pm25))
      names(poll_pm25) <- c("pollutants", "percentage")
      
      #PM10
      a5 <- total_days_df$Days.PM10
      poll_pm10 <- a5*100/total_days
      
      poll_pm10 <- data.frame(pollutants=c("PM10 as main pollutant"," "),percentage=c(poll_pm10,100-poll_pm10))
      names(poll_pm25) <- c("pollutants", "percentage")
      print("REACHED")
      
      pol_table <- data.frame(pollutants=c("SO2","CO","Ozone","NO2","PM2.5","PM10"),c(a0,a1,a2,a3,a4,a5))
      print(pol_table)
      names(pol_table) <- c("Pollutants","Number.of.days")
      print("REACHED")
      df2<- "empty"
      df3 <- "empty"
      list(value1,df1,df2,poll_so2,poll_co,poll_ozone,poll_no2,poll_pm25,poll_pm10,pol_table,df3)
    }
    #end of else
    
  })
  
  
  #BEGINNING OF PLOTS
  
  
  
  
  
  
  output$county_info <- renderUI({
    
    a<- input$county
    b<- input$state
    if(trimws(input$search) == "")
    {
      c<-trimws(input$county)
    }
    else
    {
      c<-curr_county
    }
    
    d <- subset(map_df, County.Name==c & State.Name==input$state & Latitude!=0 & Longitude!=0)
    if(is.data.frame(d) && nrow(d)!=0)
    {

      text <- paste("Current selected State is: <b>",input$state,"</b><br/><br/>Current selected county is:<b>",input$county,"</b>")
      HTML(text)
    }
    else
    {
      text <- "<p><b>Invalid county/Location not found for selected county"
      HTML(text)
    }
    
  })
  
  output$aqi_pie <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[1]],aes(x="",y=Percentage.Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 0, face = "bold"),
                                                                                                                                                                     axis.title.x = element_text(size=35),
                                                                                                                                                                     axis.title.y = element_text(size=0),
                                                                                                                                                                     legend.title=element_text(size=35), 
                                                                                                                                                                     legend.text=element_text(size=35),
                                                                                                                                                                     legend.key.size = unit(3,"line"),
                                                                                                                                                                     axis.text.x = element_text(size=35),
                                                                                                                                                                     axis.text.y = element_text(size=35)) + scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494")) + ggtitle("Percentage number of days as main pollutant")
    }
  })
  
  output$aqi_bar <- renderPlot({
    
    data_local <- getData()
    print(paste("dawta is :",data_local))
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[2]],aes(x=Air.Quality,y=Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                                                               axis.title.x = element_text(size=0),
                                                                                                                               axis.title.y = element_text(size=35),
                                                                                                                              
                                                                                                                               axis.text.x = element_text(angle=90,size=30),
                                                                                                                               legend.position="none",
                                                                                                                               axis.text.y = element_text(size=35))+ scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494")) + ggtitle("Number of days as main pollutant") + xlab("Name of pollutant") + ylab("Number of days")
      # legend.title=element_text(size=35), 
      # legend.text=element_text(size=35),
      # legend.key.size = unit(3,"line"),
    }
  })
  
  output$pollutants_bar <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[10]],aes(x=Pollutants,y=Number.of.days, fill=Pollutants)) + geom_bar(width = 1, stat = "identity")+theme(plot.title = element_text(size = 0, face = "bold"),
                                                                                                                               axis.title.x = element_text(size=35),
                                                                                                                               axis.title.y = element_text(size=35),
                                                                                                                               legend.title=element_text(size=35), 
                                                                                                                               legend.text=element_text(size=35),
                                                                                                                               legend.key.size = unit(3,"line"),
                                                                                                                               axis.text.x = element_text(size=35),
                                                                                                                               axis.text.y = element_text(size=35))+ scale_fill_manual(values=c("#d73027", "#fc8d59", "#fee090","#000000","#91bfdb","#4575b4")) + ylab("Number of days as main pollutant")
    }
  })
  
  output$pollutants_table <- renderDataTable({
    data_local <- getTableData()
    if(data_local == "empty")
    {
      emp2 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp2
    }
    else
    {
      data_local
    }
    
  },rownames= FALSE)
  
  output$aqi_table <- renderTable({
    data_local <- getData()
    if(data_local == "empty")
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp1
    }
    else
    {
      data_local[[2]]
    }
  })
  output$mpa_graph <- renderPlot({
    
    data_local <- getLineData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
      ggplot(data_local[[1]], aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                             axis.title.x = element_text(size=35),
                                                                                             axis.title.y = element_text(size=35),
                                                                                             legend.title=element_text(size=35),
                                                                                             legend.text=element_text(size=35),
                                                                                             legend.key.size = unit(3,"line"),
                                                                                             legend.key.width = unit(3,"cm"),
                                                                                             axis.text.x = element_text(size=30),
                                                                                             axis.text.y = element_text(size=30))+scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) + guides(linetype = guide_legend(override.aes = list(size = 140))) +xlab("Years") + ylab("AQI value")
    }
  })
  
  output$pollutants_graph <- renderPlot({
    
    data_local <- getLineData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20) + ylab("Number of days as main pollutant")
    }
    else
    {
      #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
      ggplot(data_local[[2]], aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                             legend.title=element_text(size=35),
                                                                                             legend.text=element_text(size=35),
                                                                                             axis.title.x = element_text(size=35),
                                                                                             axis.title.y = element_text(size=35),
                                                                                             legend.key.size = unit(3,"line"),
                                                                                             legend.key.width = unit(3,"cm"),
                                                                                             axis.text.x = element_text(size=30),
                                                                                             axis.text.y = element_text(size=30))+scale_color_manual(values=c("#d73027", "#fc8d59", "#fee090","#000000","#91bfdb","#4575b4")) + guides(linetype = guide_legend(override.aes = list(size = 140))) + xlab("Years") + ylab("Number of days as main pollutant")
      
    }
  })
  
  
  
  
  #BEGINNING OF PIE CHARTS
  
  output$so2 <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local[[4]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                             axis.title.x = element_text(size=35),
                                                                                                                                                             axis.title.y = element_text(size=35),
                                                                                                                                                             legend.title=element_text(size=40), 
                                                                                                                                                             legend.text=element_text(size=40),
                                                                                                                                                             legend.key.size = unit(5,"line"),
                                                                                                                                                             axis.text.x = element_text(size=20),
                                                                                                                                                             axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$co <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local[[5]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                        axis.title.x = element_text(size=35),
                                                                                                                                                                                        axis.title.y = element_text(size=35),
                                                                                                                                                                                        legend.title=element_text(size=40), 
                                                                                                                                                                                        legend.text=element_text(size=40),
                                                                                                                                                                                        legend.key.size = unit(5,"line"),
                                                                                                                                                                                        axis.text.x = element_text(size=20),
                                                                                                                                                                                        axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$ozone <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local[[6]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                         axis.title.x = element_text(size=35),
                                                                                                                                                                                         axis.title.y = element_text(size=35),
                                                                                                                                                                                         legend.title=element_text(size=40), 
                                                                                                                                                                                         legend.text=element_text(size=40),
                                                                                                                                                                                         legend.key.size = unit(5,"line"),
                                                                                                                                                                                         axis.text.x = element_text(size=20),
                                                                                                                                                                                         axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$no2 <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local[[7]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                         axis.title.x = element_text(size=35),
                                                                                                                                                                                         axis.title.y = element_text(size=35),
                                                                                                                                                                                         legend.title=element_text(size=40), 
                                                                                                                                                                                         legend.text=element_text(size=40),
                                                                                                                                                                                         legend.key.size = unit(5,"line"),
                                                                                                                                                                                         axis.text.x = element_text(size=20),
                                                                                                                                                                                         axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$pm25 <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local[[8]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                         axis.title.x = element_text(size=35),
                                                                                                                                                                                         axis.title.y = element_text(size=35),
                                                                                                                                                                                         legend.title=element_text(size=40), 
                                                                                                                                                                                         legend.text=element_text(size=40),
                                                                                                                                                                                         legend.key.size = unit(5,"line"),
                                                                                                                                                                                         axis.text.x = element_text(size=20),
                                                                                                                                                                                         axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$pm10 <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      print(data_local[[9]])
      pie = ggplot(data=data_local[[9]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                         axis.title.x = element_text(size=35),
                                                                                                                                                                                         axis.title.y = element_text(size=35),
                                                                                                                                                                                         legend.title=element_text(size=40), 
                                                                                                                                                                                         legend.text=element_text(size=40),
                                                                                                                                                                                         legend.key.size = unit(5,"line"),
                                                                                                                                                                                         axis.text.x = element_text(size=20),
                                                                                                                                                                                         axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  #-------------------------#
  #COMPARISON MODE CHARTS#
  
  #COUNTY1#
  output$county1_pollutant <- renderPlot({
    print("REACHED")
    data_local <- getPollutant1()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local,aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                        axis.title.x = element_text(size=35),
                                                                                                                                                                                        axis.title.y = element_text(size=35),
                                                                                                                                                                                        legend.title=element_text(size=40), 
                                                                                                                                                                                        legend.text=element_text(size=40),
                                                                                                                                                                                        legend.key.size = unit(5,"line"),
                                                                                                                                                                                        axis.text.x = element_text(size=20),
                                                                                                                                                                                        axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  
  output$aqi_table_1 <- renderTable({
    data_local <- getData1()
    if(data_local == "empty")
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp1
    }
    else
    {
      data_local[[2]]
    }
  })
  
  output$aqi_pie_1 <- renderPlot({
    
    data_local <- getData1()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[1]],aes(x="",y=Percentage.Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                     legend.title=element_text(size=80), 
                                                                                                                                                                     legend.text=element_text(size=80),
                                                                                                                                                                     legend.key.size = unit(10,"line"),
                                                                                                                                                                     axis.text.x = element_text(size=40),
                                                                                                                                                                     axis.text.y = element_text(size=40)) + scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494"))
    }
  })
  
  output$aqi_bar_1 <- renderPlot({
    
    data_local <- getData1()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[2]],aes(x=Air.Quality,y=Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                                                               axis.title.x = element_text(size=0),
                                                                                                                               axis.title.y = element_text(size=35),
                                                                                                                               
                                                                                                                               axis.text.x = element_text(angle=90,size=30),
                                                                                                                               legend.position="none",
                                                                                                                               axis.text.y = element_text(size=35))+ scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494")) + ggtitle("Number of days as main pollutant") + xlab("Name of pollutant") + ylab("Number of days")
    }
  })
  
  
  #COUNTY2#
  output$county2_pollutant <- renderPlot({
    print("REACHED")
    data_local <- getPollutant2()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local,aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                   axis.title.x = element_text(size=35),
                                                                                                                                                                                   axis.title.y = element_text(size=35),
                                                                                                                                                                                   legend.title=element_text(size=40), 
                                                                                                                                                                                   legend.text=element_text(size=40),
                                                                                                                                                                                   legend.key.size = unit(5,"line"),
                                                                                                                                                                                   axis.text.x = element_text(size=20),
                                                                                                                                                                                   axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  output$aqi_table_2 <- renderTable({
    data_local <- getData2()
    if(data_local == "empty")
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp1
    }
    else
    {
      data_local[[2]]
    }
  })
  
  output$aqi_pie_2 <- renderPlot({
    
    data_local <- getData2()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[1]],aes(x="",y=Percentage.Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                     legend.title=element_text(size=80), 
                                                                                                                                                                     legend.text=element_text(size=80),
                                                                                                                                                                     legend.key.size = unit(10,"line"),
                                                                                                                                                                     axis.text.x = element_text(size=40),
                                                                                                                                                                     axis.text.y = element_text(size=40)) + scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494"))
    }
  })
  
  output$aqi_bar_2 <- renderPlot({
    
    data_local <- getData2()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[2]],aes(x=Air.Quality,y=Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                                                                          axis.title.x = element_text(size=0),
                                                                                                                                          axis.title.y = element_text(size=35),
                                                                                                                                          
                                                                                                                                          axis.text.x = element_text(angle=90,size=30),
                                                                                                                                          legend.position="none",
                                                                                                                                          axis.text.y = element_text(size=35))+ scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494")) + ggtitle("Number of days as main pollutant") + xlab("Name of pollutant") + ylab("Number of days")
    }
  })
  
  #COUNTY3#
  output$county3_pollutant <- renderPlot({
    print("REACHED")
    data_local <- getPollutant3()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      pie = ggplot(data=data_local,aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                                   axis.title.x = element_text(size=35),
                                                                                                                                                                                   axis.title.y = element_text(size=35),
                                                                                                                                                                                   legend.title=element_text(size=40), 
                                                                                                                                                                                   legend.text=element_text(size=40),
                                                                                                                                                                                   legend.key.size = unit(5,"line"),
                                                                                                                                                                                   axis.text.x = element_text(size=20),
                                                                                                                                                                                   axis.text.y = element_text(size=20)) + ylab("") +xlab("Percentage number of days")
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  output$aqi_table_3 <- renderTable({
    data_local <- getData3()
    if(data_local == "empty")
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp1
    }
    else
    {
      data_local[[2]]
    }
  })
  output$aqi_pie_3 <- renderPlot({
    
    data_local <- getData3()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[1]],aes(x="",y=Percentage.Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+theme(plot.title = element_text(size = 60, face = "bold"),
                                                                                                                                                                     legend.title=element_text(size=80), 
                                                                                                                                                                     legend.text=element_text(size=80),
                                                                                                                                                                     legend.key.size = unit(10,"line"),
                                                                                                                                                                     axis.text.x = element_text(size=40),
                                                                                                                                                                     axis.text.y = element_text(size=40))+ scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494"))
    }
  })
  
  output$aqi_bar_3 <- renderPlot({
    
    data_local <- getData3()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      ggplot(data=data_local[[2]],aes(x=Air.Quality,y=Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                                                               axis.title.x = element_text(size=0),
                                                                                                                               axis.title.y = element_text(size=35),
                                                                                                                               
                                                                                                                               axis.text.x = element_text(angle=90,size=30),
                                                                                                                               legend.position="none",
                                                                                                                               axis.text.y = element_text(size=35))+ scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb","#41b6c4","#2c7fb8","#253494")) + ggtitle("Number of days as main pollutant") + xlab("Name of pollutant") + ylab("Number of days")
    }
  })
  
  output$map <- renderLeaflet({
    zz <- input$search
    zz1 <- input$county
    zz2 <- input$state
    a<- input$county
    b<- input$state
    print(paste("map",a,b))
    if(trimws(input$search) == "")
    {
      c<-trimws(input$county)
    }
    else
    {
      c<-curr_county
    }
    print(paste("c: ",c))
    
    d <- subset(map_df, County.Name==c & State.Name==input$state & Latitude!=0 & Longitude!=0)
    print(d)
    if(is.data.frame(d) && nrow(d)!=0)
    {
      lat_a <- d$Latitude[1]
      lng_a <- d$Longitude[1]
      content <- paste("<font size=10>",input$county,"</font>")
      
      #set default map zoomed to the center of the USA
      m <- leaflet() %>% addTiles() %>% setView(lat=lat_a,lng=lng_a,zoom=9) %>% addPopups(lng=lng_a, lat=lat_a, content,options=popupOptions(closeButton = FALSE,
                                                                                                                                             minWidth = 300,
                                                                                                                                             maxWidth = 300))
    }
    
    else
    {
      #set default map zoomed to the center of the USA
      m <- leaflet() %>% addTiles() %>% setView(lat=41.850033,lng=-100.6500523,zoom=4)
    }
  })
  
  output$county1_graph <- renderPlot({
    
    data_local <- getLineData1()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      if(input$graph_choice == "AQI")
      {
      #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
      ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                             axis.title.x = element_text(size=35),
                                                                                             axis.title.y = element_text(size=35),
                                                                                             legend.title=element_text(size=35),
                                                                                             legend.text=element_text(size=35),
                                                                                             legend.key.size = unit(3,"line"),
                                                                                             legend.key.width = unit(3,"cm"),
                                                                                             axis.text.x = element_text(size=30),
                                                                                             axis.text.y = element_text(size=30))+scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) + guides(linetype = guide_legend(override.aes = list(size = 140))) +xlab("Years") + ylab("AQI value")
      }
      else
      {
        ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                               legend.title=element_text(size=35),
                                                                                               legend.text=element_text(size=35),
                                                                                               axis.title.x = element_text(size=35),
                                                                                               axis.title.y = element_text(size=35),
                                                                                               legend.key.size = unit(3,"line"),
                                                                                               legend.key.width = unit(3,"cm"),
                                                                                               axis.text.x = element_text(size=30),
                                                                                               axis.text.y = element_text(size=30))+scale_color_manual(values=c("#d73027", "#fc8d59", "#fee090","#000000","#91bfdb","#4575b4")) + guides(linetype = guide_legend(override.aes = list(size = 140))) + xlab("Years") + ylab("Number of days as main pollutant")
      }
    }
  })
  
  output$county2_graph <- renderPlot({
    
    data_local <- getLineData2()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      if(input$graph_choice == "AQI")
      {
        #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
        ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                          axis.title.x = element_text(size=35),
                                                                                          axis.title.y = element_text(size=35),
                                                                                          legend.title=element_text(size=35),
                                                                                          legend.text=element_text(size=35),
                                                                                          legend.key.size = unit(3,"line"),
                                                                                          legend.key.width = unit(3,"cm"),
                                                                                          axis.text.x = element_text(size=30),
                                                                                          axis.text.y = element_text(size=30))+scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) + guides(linetype = guide_legend(override.aes = list(size = 140))) +xlab("Years") + ylab("AQI value")
      }
      else
      {
        ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                          legend.title=element_text(size=35),
                                                                                          legend.text=element_text(size=35),
                                                                                          axis.title.x = element_text(size=35),
                                                                                          axis.title.y = element_text(size=35),
                                                                                          legend.key.size = unit(3,"line"),
                                                                                          legend.key.width = unit(3,"cm"),
                                                                                          axis.text.x = element_text(size=30),
                                                                                          axis.text.y = element_text(size=30))+scale_color_manual(values=c("#d73027", "#fc8d59", "#fee090","#000000","#91bfdb","#4575b4")) + guides(linetype = guide_legend(override.aes = list(size = 140))) + xlab("Years") + ylab("Number of days as main pollutant")
      }
    }
  })
  
  output$county3_graph <- renderPlot({
    
    data_local <- getLineData3()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available",size=20)
    }
    else
    {
      if(input$graph_choice == "AQI")
      {
        #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
        ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                          axis.title.x = element_text(size=35),
                                                                                          axis.title.y = element_text(size=35),
                                                                                          legend.title=element_text(size=35),
                                                                                          legend.text=element_text(size=35),
                                                                                          legend.key.size = unit(3,"line"),
                                                                                          legend.key.width = unit(3,"cm"),
                                                                                          axis.text.x = element_text(size=30),
                                                                                          axis.text.y = element_text(size=30))+scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) + guides(linetype = guide_legend(override.aes = list(size = 140))) +xlab("Years") + ylab("AQI value")
      }
      else
      {
        ggplot(data_local, aes(x=years, y=value, col=variable)) + geom_line(size=3)+theme(plot.title = element_text(size = 30, face = "bold"),
                                                                                          legend.title=element_text(size=35),
                                                                                          legend.text=element_text(size=35),
                                                                                          axis.title.x = element_text(size=35),
                                                                                          axis.title.y = element_text(size=35),
                                                                                          legend.key.size = unit(3,"line"),
                                                                                          legend.key.width = unit(3,"cm"),
                                                                                          axis.text.x = element_text(size=30),
                                                                                          axis.text.y = element_text(size=30))+scale_color_manual(values=c("#d73027", "#fc8d59", "#fee090","#000000","#91bfdb","#4575b4")) + guides(linetype = guide_legend(override.aes = list(size = 140))) + xlab("Years") + ylab("Number of days as main pollutant")
      }
    }
  })
  
}
shinyApp(ui,server)