#App to show the best hospital for a given disease for a particular state


library(shiny)

ui <- fluidPage(
  h1("WeightTrack"),
  sidebarLayout(sidebarPanel = (wellPanel(
                textInput(inputId = "a", label = "Enter your weight for today", value = ""),
                submitButton("Graph It!"))),
    mainPanel = (plotOutput(outputId = "final", hover = hoverOpts(id = "a"))))
)

server <- function(input, output){
  output$final <- renderPlot(addweight(input$a))
}

shinyApp(ui = ui, server = server)

