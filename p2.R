library("shinydashboard")
library(ggplot2)

#Read in data from files
setwd("./data")
file_list = list.files(pattern="*.csv")
df_list = lapply(file_list, read.csv)

ui <- dashboardPage(
  dashboardHeader(title = "Air pollution stats in the US",titleWidth = 1000),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
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


server <- function(input,output){
  
  #slider_year <- reactive({((as.numeric(input$year)-2018)+1)})
  
  slider_year <- reactive({
  
  
  #print(slider_year())
    print(((input$year+38-2018)+1))
  df <- df_list[[(input$year+38-2018)+1]]

  temp_df <- subset(df,County=="Cook" & State=="Illinois")
  value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
  value = value1/temp_df[["Days.with.AQI"]]
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
