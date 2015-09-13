# Lindsay Vass
# 30 August 2015
#
# This script will clean the data, and create new variables needed for analysis.

library(dplyr)
library(lubridate)

load('Rda/4.Rda')

cleanData <- creditsList %>%
  select(release_date:ReleaseDate) %>%
  rename(OtherMovie = title,
         OtherReleaseDate = release_date,
         CultClassicMovie = Movie,
         CultClassicReleaseDate = ReleaseDate)
cleanData$OtherReleaseDate <- ymd(cleanData$OtherReleaseDate)
cleanData <- cleanData %>%
  filter(is.na(ymd(as.character(OtherReleaseDate), tz="UTC")) == FALSE)

# get duration between wet hot american summer and the prequel
indWHAS <- min(which(cleanData$CultClassicMovie == "Wet Hot American Summer"))
releaseWHAS <- cleanData$CultClassicReleaseDate[indWHAS]
releaseWHASFDOC <- ymd('2015-07-31')
releaseInterval <- as.period(interval(releaseWHAS, releaseWHASFDOC, 'UTC'))

# categorize movies as before or after cult classic
cleanData <- cleanData %>%
  mutate(PrePostClassic = ifelse(OtherReleaseDate - CultClassicReleaseDate < 0, 'Pre', 'Post'))

# remove CultClassicMovie released after WHAS, and OtherReleases that were 
# released after releaseInterval
cleanData <- cleanData %>%
  filter(CultClassicReleaseDate <= releaseWHAS,
         as.period(OtherReleaseDate - CultClassicReleaseDate) < releaseInterval)

save(file = 'Rda/5.Rda', list = 'cleanData')