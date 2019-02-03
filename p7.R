library("shinydashboard")
library(ggplot2)
library(leaflet)
library("reshape2")

curr_state <- "Alabama"
curr_county <- ""
curr_ip <- ""

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





ui <- dashboardPage(
  dashboardHeader(title = "Air pollution stats in the US",titleWidth = 300),
  dashboardSidebar(
    width=300,
    title = "Controls",
    sliderInput("year", "Select year:", min=1980, max=2018, value=2018,sep=""),
    selectInput(inputId = "state","Select State:",choices=states),
    selectInput(inputId = "county","Select County:",choices=NULL),
    textInput(inputId="search","Search county:")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    fluidRow(
      box(
        
        plotOutput(outputId = "aqi_pie")
      ),
      box(
        plotOutput(outputId = "aqi_bar")
      ),
      box(
        leafletOutput("map")
      ),
      box(
        tableOutput(outputId = "aqi_table")
      ),
      box(
        plotOutput(outputId = "mpa_graph") #median, 90th percentile and max AQI
      ),
      box(
        plotOutput(outputId = "so2") #median, SO2 as main pollutant
      )
      ,
      box(
        plotOutput(outputId = "no2") #median, SO2 as main pollutant
      ),
      box(
        plotOutput(outputId = "co") #median, CO as main pollutant
      ),
      box(
        plotOutput(outputId = "ozone") #median, Ozone as main pollutant
      )
      ,
      box(
        plotOutput(outputId = "pm25") #median, PM2.5 as main pollutant
      )
      ,
      box(
        plotOutput(outputId = "pm10") #median, PM10 as main pollutant
      ),
      box(
        tableOutput(outputId = "pollutants_table") #number of days for each pollutant
      ),
      box(
        plotOutput(outputId = "pollutants_bar") #bar chart
      )
      ,
      box(
        plotOutput(outputId = "pollutants_graph") #6 line graph
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
    
    output$map <- renderLeaflet({
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
        lat_a <- d$Latitude[1]
        lng_a <- d$Longitude[1]
        content <- paste(input$county)
        
        #set default map zoomed to the center of the USA
        m <- leaflet() %>% addTiles() %>% setView(lat=lat_a,lng=lng_a,zoom=6) %>% addPopups(lng=lng_a, lat=lat_a, content)
      }
      
      else
      {
        #set default map zoomed to the center of the USA
        m <- leaflet() %>% addTiles() %>% setView(lat=41.850033,lng=-100.6500523,zoom=4)
      }
    })
    
  })
  
  
  
  
  
  
  
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
      names(df1) <- c("Air.Quality","Number.of.days")
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      names(value1) <- c("Air.Quality","Percentage.Number.of.days")
      
      temp1 <- subset(master_df,State==input$state & County==input$county)
      median <- temp1$Median.AQI
      years = 1980:2018
      median1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        median1[temp] <- median[i]
        i <- i + 1
      }
      
      p90 <- temp1$X90th.Percentile.AQI
      years = 1980:2018
      p901 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        p901[temp] <- p90[i]
        i <- i + 1
      }
      
      max_aqi <- temp1$Max.AQI
      years = 1980:2018
      max_aqi1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        max_aqi1[temp] <- max_aqi[i]
        i <- i + 1
      }
      

      p901 <- data.frame(years,p901)
      median1 <- data.frame(years,median1)
      max_aqi1 <- data.frame(years,max_aqi1)
      
      names(median1) = c("years", "median AQI")
      names(p901) = c("years", "90th.percentile")
      names(max_aqi1) = c("years", "Max AQI")
      
      line1<-merge(p901,median1,by="years")
      line1<-merge(line1,max_aqi1,by="years")
      line1_melted <- reshape2::melt(line1, id.var='years')
      
      
      df2 <- data.frame(years, line1_melted)
      
      #----------------------POLLUTANTS LINE CHART--------------------------#
      
      #---------SO2 -----#
      line_so2 <- temp1$Days.SO2
      years = 1980:2018
      line_so2_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_so2_1[temp] <- line_so2[i]
        i <- i + 1
      }
      #---------NO2 -----#
      line_no2 <- temp1$Days.NO2
      years = 1980:2018
      line_no2_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_no2_1[temp] <- line_no2[i]
        i <- i + 1
      }

      #---------Ozone -----#
      line_ozone <- temp1$Days.Ozone
      years = 1980:2018
      line_ozone_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_ozone_1[temp] <- line_ozone[i]
        i <- i + 1
      }
      #---------CO -----#
      line_co <- temp1$Days.CO
      years = 1980:2018
      line_co_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_co_1[temp] <- line_co[i]
        i <- i + 1
      }
      #---------PM2.5 -----#
      line_pm25 <- temp1$"Days.PM2.5"
      years = 1980:2018
      line_pm25_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_pm25_1[temp] <- line_pm25[i]
        i <- i + 1
      }
      #---------PM10-----#
      line_pm10 <- temp1$"Days.PM10"
      years = 1980:2018
      line_pm10_1 <- rep(NA, 2018-1980)
      i <- 1
      for(year in years){
        temp <- (year-1980) + 1
        line_pm10_1[temp] <- line_pm10[i]
        i <- i + 1
      }
      
      line_so2_1 <- data.frame(years,line_so2_1)
      line_no2_1 <- data.frame(years,line_no2_1)
      line_ozone_1 <- data.frame(years,line_ozone_1)
      line_co_1 <- data.frame(years,line_co_1)
      line_pm25_1 <- data.frame(years,line_pm25_1)
      line_pm10_1 <- data.frame(years,line_pm10_1)
      
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

      df3 <- data.frame(years, line2_melted)
      
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
      
      list(value1,df1,df2,poll_so2,poll_co,poll_ozone,poll_no2,poll_pm25,poll_pm10,pol_table,df3)
    }
    #end of else
    
  })
  
  
  #BEGINNING OF PLOTS
  
  
  
  
  
  
  
  
  output$aqi_pie <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      ggplot(data=data_local[[1]],aes(x="",y=Percentage.Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
    }
  })
  
  output$aqi_bar <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      ggplot(data=data_local[[2]],aes(x="",y=Number.of.days, fill=Air.Quality)) + geom_bar(width = 1, stat = "identity")
    }
  })
  
  output$pollutants_bar <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      ggplot(data=data_local[[10]],aes(x="",y=Number.of.days, fill=Pollutants)) + geom_bar(width = 1, stat = "identity")
    }
  })
  
  output$pollutants_table <- renderTable({
    data_local <- getData()
    if(data_local == "empty")
    {
      emp2 <- data.frame(
        "Percentage of days" = c("Data not available")
      )
      emp2
    }
    else
    {
      data_local[[10]]
    }

  })

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
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
      ggplot(data_local[[3]], aes(x=years, y=value, col=variable)) + geom_line()
    }
  })
  
  output$pollutants_graph <- renderPlot({
    
    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      #ggplot(data=data_local[[3]],aes(x=years, y=median1, group=1),color="red") + ggplot(data=data_local[[3]],aes(x=years, y=p901, group=1),color="blue") + geom_point()
      ggplot(data_local[[11]], aes(x=years, y=value, col=variable)) + geom_line()
    }
  })
  
  

  
  #BEGINNING OF PIE CHARTS
  
  output$so2 <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      pie = ggplot(data=data_local[[4]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })

  output$co <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      pie = ggplot(data=data_local[[5]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })

  output$ozone <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      pie = ggplot(data=data_local[[6]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })

  output$no2 <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      pie = ggplot(data=data_local[[7]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })

  output$pm25 <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      pie = ggplot(data=data_local[[8]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })

  output$pm10 <- renderPlot({

    data_local <- getData()
    if(data_local == "empty")
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available")
    }
    else
    {
      print(data_local[[9]])
      pie = ggplot(data=data_local[[9]],aes(x="",y=percentage, fill=pollutants)) + geom_bar(width = 100, stat = "identity")+ coord_polar("y", start=0)
      pie = pie + scale_fill_manual(values=c("#ffffff","#70747a"))
      pie
    }
  })
  
  
  
  
  
  
}
shinyApp(ui,server)