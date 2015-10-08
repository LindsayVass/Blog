library(RMySQL)
library(dplyr)

load('Rda/mysqlLogin.Rda')

# Functions ---------------------------------------------------------------

connectToDatabase <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
}

addUser <- function(email) {
 
  db <- connectToDatabase()
  query <- sprintf("SELECT * FROM user WHERE email = '%s'", email)
  emailData <- dbGetQuery(db, query)

  
  if (nrow(emailData) == 0) {

    query <- sprintf("INSERT INTO user (id, name, email) VALUES (%s)",
                     paste0("NULL, NULL, '", email, "'"))
    dbGetQuery(db, query)
    
    query <- sprintf("SELECT * FROM user WHERE email = '%s'", email)
    emailData <- dbGetQuery(db, query)

  }
  
  dbDisconnect(db)
  
  return(emailData)
}

getDebateList <- function() {
  db <- connectToDatabase()
  
  # Construct the fetching query
  query <- sprintf("SELECT * FROM debate")
  
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

getCandidateList <- function(debateID) {
  db <- connectToDatabase()
  
  query <- sprintf("SELECT * FROM debate_has_candidate WHERE debate_id = %i", debateID)
  debateData <- dbGetQuery(db, query)
  
  query <- sprintf("SELECT * FROM candidate")
  candData <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  debateCand <- inner_join(debateData, candData, by = c("candidate_id" = "id"))
}

getTopicList <- function() {
  db <- connectToDatabase()
  
  query <- sprintf("SELECT * FROM topic")
  data <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  data
}

saveData <- function(data) {

  # Connect to the database
  db <- connectToDatabase() 
  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# Setup app ---------------------------------------------------------------

options(mysql = list(
  "host" = "192.185.88.181", 
  "port" = 3306,
  "user" = user,
  "password" = password
))

databaseName <- "lindsayv_debate"

debateList <- getDebateList()

topicList <- getTopicList()

fields <- c("debateID", "candidateID", "topicID", "rating")

# Server ------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # When the Submit Email button is clicked, save the form data
  observeEvent(input$submitEmail, {
    userID <- addUser(input$email)
  })
  
  # Drop-down selection box for which debate
  output$chooseDebate <- renderUI({
    selectInput("debate", "Choose a debate:", 
                choices = debateList$name)
  })
  
  # Drop-down selection box with candidates
  output$chooseCandidate <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$debate))
      return()
    
    theDebate <- input$debate
    debateID <- debateList$id[which(debateList$name == theDebate)]
    candidateList <- getCandidateList(debateID)
    
    # Create drop-down
    selectInput("candidate", "Choose a candidate:",
                choices = candidateList$name)
  })
  
  # Drop-down selection box with topics
  output$chooseTopic <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$debate))
      return()
    
    selectInput("topic", "Choose a topic:",
                choices = topicList$name)
  })
  
  # Slider input for ratings
  output$chooseRating <- renderUI({
    # If missing input, return 
    if(is.null(input$debate))
      return()
    
    sliderInput("rating", "Rating", 1, 5, 3, ticks = FALSE)
  })
  
    # Whenever a field is filled, aggregate all form data
     formData <- reactive({
      debateID    <- debateList$id[which(debateList$name == input$debate)]
      candidateID <- candidateList$candidate_id[which(candidateList$name == input$candidate)]
      topicID     <- topicList$id[which(topicList$name == input$topic)]
      
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
#     
#     # Show the previous result
#     # (update with current response when Submit is clicked)
#     output$result <- DT::renderDataTable({
#       input$submit
#       loadData()
#     })     
}
)



# library(RMySQL)
# 
# options(mysql = list(
#   "host" = "127.0.0.1",
#   "port" = 3306,
#   "user" = "myuser",
#   "password" = "mypassword"
# ))
# databaseName <- "myshinydatabase"
# table <- "responses"
# 
# saveData <- function(data) {
#   # Connect to the database
#   db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
#                   port = options()$mysql$port, user = options()$mysql$user, 
#                   password = options()$mysql$password)
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   # Submit the update query and disconnect
#   dbGetQuery(db, query)
#   dbDisconnect(db)
# }
# 
# loadData <- function() {
#   # Connect to the database
#   db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
#                   port = options()$mysql$port, user = options()$mysql$user, 
#                   password = options()$mysql$password)
#   # Construct the fetching query
#   query <- sprintf("SELECT * FROM %s", table)
#   # Submit the fetch query and disconnect
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }