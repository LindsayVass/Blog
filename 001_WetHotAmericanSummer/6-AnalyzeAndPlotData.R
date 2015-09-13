# Lindsay Vass
# 30 August 2015
#
# This script will analyze and plot the data.

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

load('Rda/5.Rda')

summaryData <- cleanData %>%
  group_by(Actor, CultClassicMovie, PrePostClassic) %>%
  summarise(NumberOfMovies = n()) %>%
  arrange(Actor, CultClassicMovie) %>%
  as.data.frame()

# make list of 0 movies for cases where there are none
noMoviesPre <- cleanData %>%
  select(Actor, CultClassicMovie) %>%
  unique() %>%
  as.data.frame() %>%
  mutate(PrePostClassic = "Pre")
noMoviesPost <- cleanData %>%
  select(Actor, CultClassicMovie) %>%
  unique() %>%
  as.data.frame() %>%
  mutate(PrePostClassic = "Post")
noMovies <- rbind(noMoviesPre, noMoviesPost) %>%
  anti_join(summaryData) %>%
  mutate(NumberOfMovies = 0)

# summarize actor data
actorSummaryData <- rbind(summaryData, noMovies) %>%
  arrange(Actor, CultClassicMovie) %>%
  dcast(Actor + CultClassicMovie ~ PrePostClassic, value.var = "NumberOfMovies") %>%
  mutate(MovieChange = (Post - Pre) / (Pre + 1),
         WHAS = ifelse(CultClassicMovie == "Wet Hot American Summer", TRUE, FALSE))

# make scatter plot of movies before/after cult classic -------------------
cbPalette <- c("#0072B2", "#CC79A7")
actorLogSummaryData <- actorSummaryData %>%
  mutate(LogPre = log10(Pre + 1),
         LogPost = log10(Post + 1))
actorP <- actorLogSummaryData %>%
  ggplot(aes(x = LogPre, y = LogPost, colour = WHAS)) +
  geom_point(data = subset(actorLogSummaryData, WHAS == FALSE), size = 5, alpha = 0.3) +
  geom_point(data = subset(actorLogSummaryData, WHAS == TRUE), size = 5) +
  scale_colour_manual(values=cbPalette, guide = FALSE) +
  theme_few() +
  labs(x = "Movies Before Cult Classic (log)",
       y = "Movies After Cult Classic (log)")
actorP + geom_text(data = subset(actorLogSummaryData, LogPost > 1.5 & WHAS == TRUE), aes(x = LogPre + 0.35, y = LogPost, label = Actor, position = "dodge", fontface = "bold"))
#actorP + geom_text(data = subset(actorLogSummaryData, LogPost > 1.7 & WHAS == FALSE), aes(x = LogPre + 0.35, y = LogPost, label = Actor, position = "dodge", fontface = "bold"))
#dir.create('Figures')
ggsave('Figures/Actor_Scatterplot.pdf', useDingbats = FALSE)


# make density plot of movie change ---------------------------------------
actorLogSummaryData$WHAS <- mapvalues(actorLogSummaryData$WHAS, from = c("TRUE", "FALSE"), to = c("Wet Hot American Summer", "Other"))
densityP <- actorLogSummaryData %>%
  ggplot(aes(x = MovieChange, fill = WHAS)) + 
  geom_density(alpha = 0.3) +
 # geom_histogram(position = "dodge") +
  scale_fill_manual(values = cbPalette) +
  scale_x_continuous(limits = c(-1, 40)) +
  labs(x = "Proportional Change in Movies",
       y = "Density",
       fill = "Movie") +
  theme_few() +
  theme(legend.position = "top")
labelData <- actorLogSummaryData %>%
  filter(WHAS == "Wet Hot American Summer",
         MovieChange > 5)
for (i in 1:nrow(labelData)) {
  densityP <- densityP + 
    geom_segment(data = labelData[i, ], x = labelData$MovieChange[i], xend = labelData$MovieChange[i], y = 0, yend = 0.3, linetype = "longdash", colour = "dimgray") +
    #geom_text(x = labelData$MovieChange[i] + 1, y = 0.35, label = labelData$Actor[i], angle = 45)
    annotate("text", label = labelData$Actor[i], x = labelData$MovieChange[i], y = 0.31, angle = 45, hjust = 0)
}
densityP
ggsave('Figures/Actor_Density.pdf')


# Summarize and plot movie data -------------------------------------------

# summarize movie data
# since WHAS has 52 listed actors, many of whom are not the actors we would 
# think of when thinking about WHAS (e.g., many campers), we'll restrict to the 
# top 5, 10, etc. actors and compare it with the top 5, 10, etc. actors of other
# movies
topN <- c(5, 10, 15, 20)
plotN <- 10 # number of movies to show in plot
for (i in 1:length(topN)) {
  movieSummaryData <- actorSummaryData %>%
    ungroup() %>%
    group_by(CultClassicMovie) %>%
    filter(n() > topN[i]) %>%
    top_n(topN[i], MovieChange) %>% 
    summarise(MeanMovieChange   = mean(MovieChange),
              MedianMovieChange = median(MovieChange),
              MinMovieChange    = min(MovieChange),
              MaxMovieChange    = max(MovieChange)) %>%
    arrange(desc(MeanMovieChange))
  barP <- movieSummaryData[1:plotN, ] %>%
    ggplot(aes(x = reorder(CultClassicMovie, MeanMovieChange), y = MeanMovieChange)) +
    geom_bar(stat = "identity") +
    ylab("Mean Proportional Change in Movies") +
    ggtitle(paste("Top", topN[i], "Actors")) +
    coord_flip() +
    theme_few() +
    theme(axis.title.y = element_blank(),
          text = element_text(size = 18)) 
  ggsave(paste0('Figures/Movie_MeanChange_Bar_Top', topN[i], '.pdf'))
}


