 shinyUI(fluidPage(
   
   textInput("email", "Email", ""),
   
   actionButton("submitEmail", "Submit"),
   
   tags$hr(),
   
   DT::dataTableOutput("result", width = 300),
   
   tags$hr(),
   
   uiOutput("chooseDebate"),
   
   uiOutput("chooseCandidate"),
   
   uiOutput("chooseTopic"),
   
   uiOutput("chooseRating"),
   
   actionButton("submit", "Submit")
  
))