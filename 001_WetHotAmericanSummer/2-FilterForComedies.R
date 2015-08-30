# Lindsay Vass
# 5 August 2015
#
# This script will go through each movie in the list, determine whether each 
# movie is a comedy, and remove movies that are not.

load('Rda/1.Rda')

apiLink <- "http://en.wikipedia.org/w/api.php?action=query&prop=categories&format=json&cllimit=500&titles="

comedyFilmList <- data.frame()
pb <- txtProgressBar(min = 1, max = nrow(filmList), style = 3)
for (thisFilm in 1:nrow(filmList)) {
  setTxtProgressBar(pb, thisFilm)
  filmTitle    <- filmList$Title[thisFilm]
  noSpaceTitle <- gsub(" ", "_", filmTitle)
  theLink      <- paste0(apiLink, noSpaceTitle)
  
  catData <- content(GET(theLink)) %>%
    unlist()
  if (length(grep("comedy", catData)) > 0){
    thisComedy <- data.frame(Title = filmTitle)
    comedyFilmList <- rbind(comedyFilmList, thisComedy)
  }
}

save(file = 'Rda/2.Rda', list = 'comedyFilmList')