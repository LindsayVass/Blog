shinyUI(fluidPage(
  
  titlePanel("Debate Watcher"),
  
  conditionalPanel(
    condition = "input.submitEmail == false",
    textInput("email", "Email", ""),
    
    actionButton("submitEmail", "Submit"),
    
    tags$hr()),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("chooseDebate"),
      
      uiOutput("chooseCandidate"),
      
      uiOutput("chooseTopic"),
      
      uiOutput("chooseRating"),
      
      actionButton("submit", "Submit"),
      
      conditionalPanel(
        condition = "input.submit == true",
        actionButton("doneRating", "View Your Results")
      )
    ),
    
    
    
    mainPanel(
      plotOutput("meanRating"),
      
      downloadButton('downloadData', 'Download Your Data')
    ))
))