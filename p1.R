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
      )
    ),
    fluidRow(
      box(
        plotOutput(outputId = "aqi_pie")
      )
    )
  )
)


server <- function(input,output){
  
  #slider_year <- reactive({((as.numeric(input$year)-2018)+1)})

  # slider_year <- reactive({year <- input$year})
  
  year <- isolate(input$year)
  slider_year <- (year-2018)+1
  #print(slider_year())
  df <- df_list[[slider_year]]
  temp_df <- subset(df,County=="Cook")
  value1 = c(temp_df[["Good.Days"]],temp_df[["Moderate.Days"]],temp_df[["Unhealthy.for.Sensitive.Groups.Days"]],temp_df[["Unhealthy.Days"]],temp_df[["Very.Unhealthy.Days"]],temp_df[["Hazardous.Days"]])
  value = value1/temp_df[["Days.with.AQI"]]
  df1 <- data.frame(
    group = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),
    values = value
  )

 output$aqi_pie <- renderPlot({
   ggplot(data=df1,aes(x="",y=values, fill=group)) + geom_bar(width = 1, stat = "identity")
   
 })
}
shinyApp(ui,server)
