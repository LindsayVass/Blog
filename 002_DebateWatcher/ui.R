 shinyUI(fluidPage(
   
   textInput("name", "Name", ""),
   
   textInput("email", "Email", ""),
   
   tags$hr(),
   
   DT::dataTableOutput("result", width = 300),
   
   tags$hr(),
   
   uiOutput("chooseDebate"),
   
   uiOutput("chooseCandidate"),
   
   uiOutput("chooseTopic"),
   
   uiOutput("chooseRating"),
   
   actionButton("submit", "Submit")
  
))