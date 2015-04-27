library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(  

  tabPanel("Prediction",
    
    img(src='logo3.png', align = "center", height="151", width="700"),
    
    hr(),
      
    h3("Insert a sequence of words in English"),
    
    tags$style(type='text/css', "#text { height:55px; width:700px; font-size: 22px;font-style: italic; font-weight: bold;}"),    
    
    textInput("text", label = "", value = ""),
    
    submitButton("Predict word"),
    checkboxGroupInput("flagprof", NULL, 
                       "Profanity filter", selected = "Profanity filter"),
    checkboxGroupInput("flagprob", NULL,
                       "View other possible words",
                       selected = "View other possible words")
            
#    fluidRow(
#      column(width = 3,
#             submitButton("Predict word")
#      ),
#      column(width = 3,
#             checkboxGroupInput("flagprof", NULL, 
#                                "Profanity filter", selected = "Profanity filter")
#      ),
#      column(width = 4,
#             checkboxGroupInput("flagprob", NULL, 
#                                "View other possible words",
#                                selected = "View other possible words")
#      )
#    ), 
    
  ),
  

  mainPanel(
    tabsetPanel(
      tabPanel("Prediction", 
               tags$style(type='text/css', "#prediction { height:55px; font-size: 22px;font-style: italic; font-weight: bold; }"),
               verbatimTextOutput("prediction"),
               br(),
               tags$style(type='text/css', "#titleprob { font-size: 16px }"),
               textOutput("titleprob"),
               tags$style(type='text/css', "#otherprob { font-size: 15px; font-style: italic }"),
               verbatimTextOutput("otherprob")
      ), 
      
      tabPanel("Documentation",
               br(),
               includeHTML("./www/Documentation.html")
      )
    ),
    width = 11.5
  )
  
 )
)