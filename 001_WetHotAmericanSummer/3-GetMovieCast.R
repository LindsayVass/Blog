# Lindsay Vass
# 30 August 2015
#
# This script will go through each movie in the list and retrieve information 
# about the cast and date of release.

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

load('Rda/2.Rda')

# Functions ---------------------------------------------------------------

getPrettyJson <- function(url) {
  data <- GET(url) %>%
    content() %>%
    toJSON() %>%
    fromJSON()
}

checkName <- function(movieName) {
  
  movieName <- as.character(movieName)
  
  # check if contains year of film of format: (1982 film)
  yearFilmCheck <- regexpr(" \\([[:digit:]]{4} film\\)", movieName)
  if (yearFilmCheck != -1) {
    filmYear <- substr(movieName, yearFilmCheck + 2, yearFilmCheck + 5)
    trimmedName <- substr(movieName, 1, yearFilmCheck - 1)
  } else {
    # check if contains: (film)
    filmCheck <- regexpr(" \\(film\\)", movieName)
    filmYear  <- NA
    if (filmCheck != -1) {
      trimmedName <- substr(movieName, 1, filmCheck - 1)
    } else {
      trimmedName <- movieName # no trim
    }
  }
  return(list(name = trimmedName, year = filmYear))
}


# Script ------------------------------------------------------------------

apiKey <- "15ecfe62edcdb1c2b79d50c56100c220"
getIdUrl    <- paste0('http://api.tmdb.org/3/search/movie?api_key=', apiKey, '&query=')
getMovieUrl <- 'http://api.tmdb.org/3/movie/'

castList <- vector(mode = "list", length = nrow(comedyFilmList))
pb <- txtProgressBar(min = 1, max = nrow(comedyFilmList), style = 3)
for (thisMovie in 1:nrow(comedyFilmList)) {
  setTxtProgressBar(pb, thisMovie)
  
  # clean up name of movie, and save year if included
  movieName <- comedyFilmList$Title[thisMovie]
  movieNameData <- checkName(movieName)
  movieName <- movieNameData$name
  movieYear <- movieNameData$year
  
  # get movie ID
  if (is.na(movieYear)) {
    movieNameUrl <- URLencode(as.character(movieName))
  } else {
    movieNameUrl <- paste0(URLencode(as.character(movieName)), '&year=', movieYear)
  }
  
  queryUrl <- paste0(getIdUrl,movieNameUrl)
  json <- getPrettyJson(queryUrl)
  
  # if query returns multiple responses, we don't have a way of determining 
  # which one is correct, so move to the next movie
  if (json$total_results != 1) {
    next
  }
  
  movieID <- json$results$id
  releaseDate <- ymd(json$results$release_date)
  
  # get info about movie using ID
  queryUrl <- paste0(getMovieUrl, movieID, '/credits?api_key=', apiKey)
  movieJson <- getPrettyJson(queryUrl)  
  movieCast <- movieJson$cast 
  
  # if no cast info
  if (length(movieJson$cast) == 0) {
    next
  }
  movieCast <- movieCast %>%
    select(cast_id:order) %>%
    mutate(Title = movieName,
           ReleaseDate = releaseDate)
  castList[[thisMovie]] <- movieCast
}

castList <- data.table::rbindlist(castList)

save(file = 'Rda/3.Rda', list = 'castList')