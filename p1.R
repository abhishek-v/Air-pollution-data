library("shinydashboard")


#Read in data from files


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
        textOutput(outputId = 'Header')
      )
    )
  )
)


server <- function(input,output){
 output$Header <- renderText({paste("You have selected",input$year)})
}
shinyApp(ui,server)
