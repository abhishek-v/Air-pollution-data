library("shinydashboard")
library(ggplot2)

#Read in data from files
setwd("./data")
file_list = list.files(pattern="*.csv")
df_list = lapply(file_list, read.csv)

#Get list of states from latest file to be used in selectinput
df <- df_list[[39]] #year 2018
head(df)
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

# if("Autauga" %in% master_states[["Alabama"]])
# {
#   print("yes")
# } else
# {
#   print("no")
# }

# print(master_states[["Washington"]][1])


# print(head(master_df))

curr_state <- "Alabama" #hard coded initial value
curr_year <- 2018 #hard coded initial value
curr_county <- ""
curr_ip <- ""
no_county <- 0

global_flag <- 1

ui <- dashboardPage(
  dashboardHeader(title = "Air pollution stats in the US",titleWidth = 1000),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title="Select State:",
        selectInput(inputId = "state","Select State:",choices=states)
        
      ),
      box(
        title="Select County:",
        selectInput(inputId = "county","Select County:",choices=NULL),
        textInput(inputId="search","Search county:")
      ),
      box(
        title = "Controls",
        sliderInput("year", "Select year:", min=1980, max=2018, value=2018)
      ),
      box(
        tableOutput(outputId = "aqi_table")
      )
    ),
    fluidRow(
      box(
        plotOutput(outputId = "aqi_bar")
      ),
      box(
        plotOutput(outputId = "aqi_pie")
      )
    )
  )
)


server <- function(input,output,session){
  
  #slider_year <- reactive({((as.numeric(input$year)-2018)+1)})
  
  
  select_state <- reactive({
    state <- input$state
  })
  
  county_search <-reactive({
    county <- input$search
  })
  
  slider_year <- reactive({
    
    county_input <- trimws(county_search())
    state <- select_state()
    
    if(county_input != curr_ip)
    {
      global_flag <<- 4
      curr_ip <<- county_input
    }
    
    df <- df_list[[(input$year+38-2018)+1]]
    
    t_df <- subset(df,State==state)
    t_df <- t_df[grep(paste("^",county_input,sep=""), t_df$County, ignore.case=T),]
    
    counties <- master_states[[input$state]][grepl(paste("^",county_input,sep=""), master_states[[input$state]], ignore.case=T)]
    
    print(counties)
    print(input$state)
    print(input$county)
    
    
    if(length(counties) == 0)
    {
      #no county selected
      print("NO COUNTIES FOUND")
      no_county <<- 1
      #updateSelectInput(session,inputId="county","Select County:",choices=c("No counties found"),selected="No counties found")
    }
    else if(is.data.frame(t_df) && nrow(t_df)==0)
    {
      no_county <<-2
    }
    else
    {
      no_county <<- 0
    }
    
    if(no_county == 1)
    {
      global_flag <<- 5
    }
    
    if(input$state != curr_state)
    {
      curr_state <<- input$state
      global_flag <<- 1
    }
    
    else if(input$year != curr_year)
    {
      curr_year <<- input$year
      global_flag <<- 2
    }
    
    print(paste("global flag",global_flag))
    if(global_flag == 1)
    {
      print(counties)
      updateSelectInput(session,inputId="county","Select County:",choices=counties,selected=counties[1])
      global_flag <<- 3
      curr_county <<- counties[1]
      req(FALSE, cancelOutput=TRUE)
    }
    
    else if(global_flag ==2)
    {
      if(curr_county %in% counties)
      {
        updateSelectInput(session,inputId="county","Select county:",choices=counties,selected=curr_county)
        global_flag <<- 3
      }
      else
      {
        updateSelectInput(session,inputId="county","Select county:",choices=counties,selected=counties[1])
        global_flag <<- 3
        req(FALSE, cancelOutput=TRUE)
      }
      
    }
    else if(global_flag ==4)
    {
      updateSelectInput(session,inputId="county","Select county:",choices=counties,selected=counties[1])
      global_flag <<- 3
      req(FALSE, cancelOutput=TRUE)
      
    }
    
    else if(global_flag == 5)
    {
      updateSelectInput(session,inputId="county","Select county:",choices=NULL,selected="")
      global_flag <<- 3
      curr_county <<- ""

    }
    
    if(input$county != "")
    {
      curr_county<<- input$county
    }
    if(global_flag == 3)
    {
      temp_df <- subset(df,County==curr_county & State==input$state)
    }
    else
    {
      temp_df <- subset(df,County==curr_county & State==input$state)
    }
    
    
    if(is.data.frame(temp_df) && nrow(temp_df)==0 && no_county!=1)
    {
      no_county <- 2
    }
    
    value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
    value = value1/temp_df[["Days.with.AQI"]]
    
    if(no_county == 0)
    {
      df1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value
      )
      value1 <- data.frame(
        group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
        values = value1
      )
      l <- list(df1, value1,no_county)
    }
    else
    {
      l <- list("","",no_county)
    }
    
  })
  
  output$aqi_bar <- renderPlot({
    print(paste("no_county",(slider_year())[3]))
    if( (slider_year())[3] == 0)
    {
      ggplot(data=(slider_year())[[1]],aes(x="",y=values, fill=group)) + geom_bar(width = 1, stat = "identity")
    }
    else if((slider_year())[3]== 1)
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Invalid county")
    }
    else if((slider_year())[3]== 2)
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available for this county")
    }
  })
  
  output$aqi_pie <- renderPlot({
    if((slider_year())[3]== 0)
    {
      ggplot(data=(slider_year())[[1]],aes(x="",y=values, fill=group)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
    }
    else if((slider_year())[3]== 1)
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Invalid county")
    }
    else if((slider_year())[3]== 2)
    {
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "Data not available for this county")
    }
  })
  
  output$aqi_table <- renderTable({
    
    if((slider_year())[3] == 0)
    {
      (slider_year())[[2]]
    }
    else if((slider_year())[3]== 1)
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Invalid county")
      )
      emp1
    }
    else if((slider_year())[3]== 2)
    {
      emp1 <- data.frame(
        "Percentage of days" = c("Data not available for this county")
      )
      emp1
    }
  })
  
}
shinyApp(ui,server)
