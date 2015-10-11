library(RMySQL)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

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
  query <- sprintf("INSERT INTO result (id, created, rating, user_id, candidate_id, debate_id, topic_id) VALUES ('%s')",
                   paste0("NULL', now(), '",
                          paste(data$rating[1], 
                                data$user_id[1], 
                                data$candidate_id[1], 
                                data$debate_id[1], 
                                data$topic_id[1],
                                sep = "', '")))
  
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

retrieveData <- function(user_id, debate_id) {
  db <- connectToDatabase()
  
  # Construct the fetching query
  query <- sprintf("SELECT * FROM result WHERE user_id = %i AND debate_id = %i", user_id, debate_id)
  userData <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  candidateList <- getCandidateList(debate_id)
  
  userData <- inner_join(userData, candidateList) %>%
    rename(CandidateName = name)
  userData <- inner_join(userData, debateList, by = c("debate_id" = "id")) %>%
    rename(DebateName = name)
  userData <- inner_join(userData, topicList, by = c("topic_id" = "id")) %>%
    rename(TopicName = name)
  
  userData <- userData %>%
    select(created, rating, CandidateName:TopicName) %>%
    rename(RatingTime = created)
  
}

disconnectAll <- function () {
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) dbDisconnect(con)
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

fields <- c("rating", "user_id", "candidate_id", "debate_id", "topic_id")

partyColors <- suppressWarnings(brewer.pal(2, "Set1"))
partyColors <- c(partyColors[2], partyColors[1])

# Server ------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # Set up list to store ID values for querying database
  idValues <- reactiveValues(userID = NULL,
                             debateID = NULL,
                             candidateID = NULL,
                             topicID = NULL,
                             candidateList = NULL,
                             debateParty = NULL)
  
  # When the Submit Email button is clicked, save the form data
  observeEvent(input$submitEmail, {
    userData <- addUser(input$email)
    idValues$userID <- userData$id
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
    
    # Create drop-down
    theDebate <- input$debate
    debateID <- debateList$id[which(debateList$name == input$debate)]
    candidateList <- getCandidateList(debateID)
    
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
    
    data <- list(rating = input$rating,
                 user_id = idValues$userID,
                 candidate_id = idValues$candidateID,
                 debate_id = idValues$debateID,
                 topic_id = idValues$topicID)
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    idValues$debateID    <-debateList$id[which(debateList$name == input$debate)]
    candidateList        <- getCandidateList(idValues$debateID)
    idValues$candidateID <- candidateList$candidate_id[which(candidateList$name == input$candidate)]
    idValues$topicID     <- topicList$id[which(topicList$name == input$topic)]
    idValues$debateParty <- debateList$debate_party[which(debateList$name == input$debate)]
    
    saveData(formData())
  })   
  
  # Text describing rating
  observeEvent(input$submit, {
    output$ratingText <- renderText({ 
      paste0("You rated ", isolate(input$candidate), ' a ', isolate(input$rating), ' on ', isolate(input$topic), '.')
      })
  })

# When the Done Rating button is clicked, retrieve all data for this email/debate
observeEvent(input$doneRating, {

  userData <- retrieveData(idValues$userID, idValues$debateID)
  
  # Fill in the spot we created for a plot
  output$meanRating <- renderPlot({
    meanData <- userData %>%
      group_by(CandidateName, party) %>%
      summarise(MeanRating = mean(rating)) 
    meanData$CandidateName <- factor(meanData$CandidateName)
    meanData <- transform(meanData, 
                          CandidateName = reorder(CandidateName, MeanRating))
    
    # Select color based on party
    if (idValues$debateParty == "Democratic") {
      partyColors <- partyColors[1]
    } else if (idValues$debateParty == "Republican") {
      partyColors <- partyColors[2]
    } else {
    }
    
    # Create Plot
    ggplot(meanData, aes(x = CandidateName, y = MeanRating, fill = party)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = CandidateName, y = 0.25, label = CandidateName, hjust = 0), colour = "black", size = 8) +
      theme_few() +
      scale_fill_manual(values = partyColors) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size = 24),
            axis.title.x = element_text(vjust = -0.25)) +
      ylab("Mean Rating") +
      scale_y_continuous(limits = c(0, 5), breaks = c(1,2,3,4,5)) +
      coord_flip() +
      labs(fill = "")
  })
  
  # Allow the user to download their data
  output$downloadData <- downloadHandler(
    filename = 'debate_data.csv',
    content = function(file) {
      write.csv(userData, file)
    }
  )
})
}
)