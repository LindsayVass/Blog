shinyUI(fluidPage(
  
  titlePanel("Debate Watcher"),
  
  conditionalPanel(
    condition = "input.submitEmail == false",
    textInput("email", "Email", ""),
    
    actionButton("submitEmail", "Submit"),
    
    tags$hr()),
  
  conditionalPanel(
    condition = "input.submitEmail == true",
    
    sidebarLayout(
      sidebarPanel(
        
        uiOutput("chooseDebate"),
        
        uiOutput("chooseCandidate"),
        
        uiOutput("chooseTopic"),
        
        uiOutput("chooseRating"),
        
        actionButton("submit", "Submit"),
        
        tags$br(),
        tags$br(),
        
       textOutput("ratingText")
      ),
      
      mainPanel(
        plotOutput("meanRating"),
        actionButton("doneRating", "View Your Results"),
        downloadButton('downloadData', 'Download Your Data')
      ))
  )
))