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

curr_state <- "Alabama" #hard coded variable
curr_year <- 2018
curr_county <- ""

global_flag <- 0

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
  
  slider_year <- reactive({
  state <- select_state()
  
  
  #print(slider_year())
    print(((input$year+38-2018)+1))
  df <- df_list[[(input$year+38-2018)+1]]
  #FOLLOWING LINE HAS TO BE CHANGED TO BE MADE DYNAMIC
  
  t_df <- subset(df,State==state)
  counties <- unique(t_df$County)
  print(input$year)
  print(input$county)
  if(input$state != curr_state)
  {
    curr_state <<- input$state
    global_flag <<- 0
  }
  
  if(input$year != curr_year)
  {
    curr_year <<- input$year
    global_flag <<- 0
  }
  
  if(global_flag == 0)
  {
  updateSelectInput(session,inputId="county","Select State:",choices=counties,selected=counties[1])
  global_flag <<- 1
  curr_county <<- counties[1]
  }

  if(input$county != "" &   global_flag != 1)
  {
  curr_county <<- input$county
  temp_df <- subset(df,County==input$county & State==input$state)
  }
  else
  {
    temp_df <- subset(df,County==curr_county & State==input$state)
  }
  
  value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
  value = value1/temp_df[["Days.with.AQI"]]
  
  print(value)
  df1 <- data.frame(
    group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
    values = value
  )
  value1 <- data.frame(
    group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
    values = value1
  )
  l <- list(df1, value1)
  })

  output$aqi_bar <- renderPlot({
    ggplot(data=(slider_year())[[1]],aes(x="",y=values, fill=group)) + geom_bar(width = 1, stat = "identity")
    
  })
  
  output$aqi_pie <- renderPlot({
    ggplot(data=(slider_year())[[1]],aes(x="",y=values, fill=group)) + geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
  })
  
  output$aqi_table <- renderTable({(slider_year())[[2]]})
}
shinyApp(ui,server)
