library(shiny)
library(RCurl)
library(drc)
source_https <- function(url, ...) {
  
  #parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, 
                             cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), 
         envir = .GlobalEnv)
  })
}

#source functions from Github
source_https("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Doug/4pl%20Robot.R")

ui <- fluidPage(
  h1("4pl Robot"),
  img(src="elisa2.jpg"),
  textInput(inputId = "myread", label = "Enter the filepath to the raw data file ending in .csv (if you are using Windows, replace all \\ characters with / characters)", value = "..."),
  textInput(inputId = "mysave", label = "Enter the filepath to the folder where you would like the output saved (if you are using Windows, replace all \\ characters with / characters)", value = "..."),
  submitButton(text = "Run the robot!"),
  verbatimTextOutput(outputId = "results")
)

server <- function(input, output){
  
  output$results <- renderPrint(
    
    Fourpl.robot(input$myread, input$mysave)
    
    )
                  
  
}

shinyApp(ui = ui, server = server)