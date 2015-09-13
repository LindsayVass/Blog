# Lindsay Vass
# 30 August 2015
#
# This script will retrieve the credits for each individual actor.

library(httr)
library(jsonlite)
library(dplyr)

load('Rda/3.Rda')

# Functions ---------------------------------------------------------------

getPrettyJson <- function(url) {
  data <- GET(url) %>%
    content() %>%
    toJSON() %>%
    fromJSON()
}

# Script ------------------------------------------------------------------

apiKey <- "15ecfe62edcdb1c2b79d50c56100c220"

creditsList <- vector(mode = "list", length = nrow(castList))
pb <- txtProgressBar(min = 1, max = nrow(castList), style = 3)
for (thisActor in 1:nrow(castList)) {
  setTxtProgressBar(pb, thisActor)
  creditsUrl <- paste0('http://api.tmdb.org/3/person/', castList$id[thisActor], '/movie_credits?api_key=', apiKey)
  creditsData <- getPrettyJson(creditsUrl)
  actorName <- as.character(castList$name[thisActor])
  movieName <- castList$Title[thisActor]
  movieRelease <- castList$ReleaseDate[thisActor]
  theseCredits <- creditsData$cast %>%
    mutate(Actor = actorName,
           Movie = movieName,
           ReleaseDate = movieRelease)
  creditsList[[thisActor]] <- theseCredits
}

creditsList <- data.table::rbindlist(creditsList)

save(file = 'Rda/4.Rda', list = 'creditsList')