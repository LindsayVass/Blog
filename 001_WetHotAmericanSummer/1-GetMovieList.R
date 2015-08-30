# Lindsay Vass
# 5 August 2015
#
# This script will acquire and clean the "List of cult films" from Wikipedia.

library(httr)
library(magrittr)
library(jsonlite)

convertToDf <- function(list) {
  list %>%
    unlist() %>%
    matrix() %>%
    data.frame()
}

df1 <- content(GET("http://en.wikipedia.org/w/api.php?action=query&prop=links&format=json&pllimit=500&titles=List%20of%20cult%20films"))
df2 <- content(GET("http://en.wikipedia.org/w/api.php?action=query&prop=links&format=json&pllimit=500&plcontinue=34301706%7C0%7CMuriel's_Wedding&titles=List%20of%20cult%20films"))

json1 <- fromJSON(toJSON(df1))
json2 <- fromJSON(toJSON(df2))

list1 <- json1$query$pages$"34301706"$links$title %>%
  convertToDf()
list2 <- json2$query$pages$"34301706"$links$title %>%
  convertToDf()

filmList <- rbind(list1, list2)
names(filmList) <- "Title"

rm(list = c("list1", "list2", "df1", "df2", "json1", "json2"))

save(file = 'Rda/1.Rda', list = "filmList")