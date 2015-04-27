library(shiny)

suppressPackageStartupMessages(library("tm"))
suppressPackageStartupMessages(library("stringi"))
suppressPackageStartupMessages(library("RWeka"))
suppressPackageStartupMessages(library("data.table"))

source("./code/tidytext.R")
source("./code/predict.R")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  reactivePred <- reactive({ 
    pred <- NULL
    if(input$text != ""){
      pred <- predict(input$text)
      if(!is.null(input$flagprof) & !is.null(pred)) pred <- verifyprof(pred)
    } 
    else pred <- ""
    
    if(is.null(pred)) pred <- "can't predict"
    pred
  })
  
  reactiveTitle <- reactive({
    if(!is.null(input$flagprob)) return("Other likely word(s)")
    else return("")
  })
  
  reactiveProb <- reactive({
    if(!is.null(input$flagprob)) return(reactivePred()[-1])
    else return("")
  })
    
  output$prediction <- renderText({ reactivePred()[1] })
  
  output$otherprob <- renderText({ reactiveProb() })
  
  output$titleprob <- renderText({ reactiveTitle() })
  
})