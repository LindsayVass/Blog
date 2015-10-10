shinyUI(fluidPage(
  
  titlePanel("Debate Watcher"),
  
  textInput("email", "Email", ""),
  
  actionButton("submitEmail", "Submit"),
  
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("chooseDebate"),
      
      uiOutput("chooseCandidate"),
      
      uiOutput("chooseTopic"),
      
      uiOutput("chooseRating"),
      
      actionButton("submit", "Submit"),
      
      actionButton("doneRating", "View Your Results")
    ),
    mainPanel(
      plotOutput("meanRating"),
      
      downloadButton('downloadData', 'Download Your Data')
    ))
))