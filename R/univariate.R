#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
univariate <- function(mdb) {

  # Format Data
  popularity <- as.data.frame(mdb %>% select(popularity))
  title <- as.data.frame(mdb %>% select(title) %>% group_by(title) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  type <- as.data.frame(mdb %>% select(title_type) %>% group_by(title_type) %>%
                             summarize(N = n()) %>%
                             mutate(p = N/ sum(N)), row.names = NULL)
  genre <- as.data.frame(mdb %>% select(genre) %>% group_by(genre) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  mpaa <- as.data.frame(mdb %>% select(mpaa_rating) %>% group_by(mpaa_rating) %>%
                           summarize(N = n()) %>%
                           mutate(p = N/ sum(N)), row.names = NULL)
  studio <- as.data.frame(mdb %>% select(studio) %>% group_by(studio) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)) %>%
                            arrange(desc(N)), row.names = NULL)
  year <- as.data.frame(mdb %>% select(thtr_rel_year))
  season <- as.data.frame(mdb %>% select(thtr_rel_season) %>% group_by(thtr_rel_season) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  month <- as.data.frame(mdb %>% select(thtr_rel_month) %>% group_by(thtr_rel_month) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)), row.names = NULL)
  imdbRating <- as.data.frame(mdb %>% select(imdb_rating))
  imdbVotes <- as.data.frame(mdb %>% select(imdb_num_votes))
  imdbLogVotes <- log2(as.data.frame(mdb %>% select(imdb_num_votes)))
  criticsScore <- as.data.frame(mdb %>% select(critics_score))
  audienceScore <- as.data.frame(mdb %>% select(audience_score))
  bestPicNom <- as.data.frame(mdb %>% select(best_pic_nom) %>% group_by(best_pic_nom) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  bestPicWin <- as.data.frame(mdb %>% select(best_pic_win) %>% group_by(best_pic_win) %>%
                                summarize(N = n()) %>%
                                mutate(p = N/ sum(N)), row.names = NULL)
  bestActorWin <- as.data.frame(mdb %>% select(best_actor_win) %>% group_by(best_actor_win) %>%
                                summarize(N = n()) %>%
                                mutate(p = N/ sum(N)), row.names = NULL)
  bestActressWin <- as.data.frame(mdb %>% select(best_actress_win) %>% group_by(best_actress_win) %>%
                                  summarize(N = n()) %>%
                                  mutate(p = N/ sum(N)), row.names = NULL)
  bestDirWin <- as.data.frame(mdb %>% select(best_dir_win) %>% group_by(best_dir_win) %>%
                                     summarize(N = n()) %>%
                                     mutate(p = N/ sum(N)), row.names = NULL)
  top200Box <- as.data.frame(mdb %>% select(top200_box) %>% group_by(top200_box) %>%
                                summarize(N = n()) %>%
                                mutate(p = N/ sum(N)), row.names = NULL)
  director <- as.data.frame(mdb %>% select(director) %>% group_by(director) %>%
                                 summarize(N = n()) %>%
                                 mutate(p = N/ sum(N)) %>%
                                 arrange(desc(N)), row.names = NULL)
  actor1 <- as.data.frame(mdb %>% select(actor1) %>% group_by(actor1) %>%
                              summarize(N = n()) %>%
                              mutate(p = N/ sum(N)) %>%
                              arrange(desc(N)), row.names = NULL)
  actor2 <- as.data.frame(mdb %>% select(actor2) %>% group_by(actor2) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)) %>%
                            arrange(desc(N)), row.names = NULL)
  actor3 <- as.data.frame(mdb %>% select(actor3) %>% group_by(actor3) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)) %>%
                            arrange(desc(N)), row.names = NULL)
  actor4 <- as.data.frame(mdb %>% select(actor4) %>% group_by(actor4) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)) %>%
                            arrange(desc(N)), row.names = NULL)
  actor5 <- as.data.frame(mdb %>% select(actor5) %>% group_by(actor5) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)) %>%
                            arrange(desc(N)), row.names = NULL)
  studioVotes <- as.data.frame(mdb %>% select(studio_votes))
  castVotes <- as.data.frame(mdb %>% select(cast_votes))
  studioExperience <- as.data.frame(mdb %>% select(studio_experience))
  directorExperience <- as.data.frame(mdb %>% select(director_experience))
  castExperience <- as.data.frame(mdb %>% select(cast_experience))


  # Conduct Analysis
  popularity <- analyzeNumeric(popularity, "Popularity")
  title <- analyzeCategorical(title, "Title")
  type <- analyzeCategorical(type, "Type")
  genre <- analyzeCategorical(genre, "Genre")
  mpaa <- analyzeCategorical(mpaa, "MPAA Rating")
  studio <- analyzeCategorical(head(studio, 10), "Top 10 Studios")
  year <- analyzeNumeric(year, "Year")
  season <- analyzeCategorical(season, "Season")
  month <- analyzeCategorical(month, "Month")
  imdbRating <- analyzeNumeric(imdbRating, "IMDB Rating")
  imdbVotes <- analyzeNumeric(imdbVotes, "IMDB Votes")
  imdbLogVotes <- analyzeNumeric(imdbLogVotes, "IMDB Log Votes")
  criticsScore <- analyzeNumeric(criticsScore, "Critics Scores")
  audienceScore <- analyzeNumeric(audienceScore, "Audience Scores")
  bestPicNom <- analyzeCategorical(bestPicNom, "Best Picture Nomination")
  bestPicWin <- analyzeCategorical(bestPicWin, "Best Picture Oscars")
  bestDirWin <- analyzeCategorical(bestDirWin, "Best Director Oscars")
  bestActorWin <- analyzeCategorical(bestActorWin, "Best Actor Oscars")
  bestActressWin <- analyzeCategorical(bestActressWin, "Best Actress Oscars")
  top200Box <- analyzeCategorical(top200Box, "Top 200 Box Office")
  director <- analyzeCategorical(director,25, "Directors")
  actor1 <- analyzeCategorical(head(actor1,25), "Actor1")
  actor2 <- analyzeCategorical(head(actor2,25), "Actor2")
  actor3 <- analyzeCategorical(head(actor3,25), "Actor3")
  actor4 <- analyzeCategorical(head(actor4,25), "Actor4")
  actor5 <- analyzeCategorical(head(actor5,25), "Actor5")
  studioVotes <- analyzeNumeric(studioVotes, "Average Studio Vote")
  castVotes <- analyzeNumeric(castVotes, "Cast Total Vote")
  studioExperience <- analyzeNumeric(studioExperience, "Studio Experience")
  directorExperience <- analyzeNumeric(directorExperience, "Director Experience")
  castExperience <- analyzeNumeric(castExperience, "Cast Experience")


  # Return analysis
  analysis <- list(
    popularity = popularity,
    type = type,
    genre = genre,
    mpaa = mpaa,
    studio = studio,
    year = year,
    season = season,
    month = month,
    imdbRating = imdbRating,
    imdbVotes = imdbVotes,
    imdbLogVotes = imdbLogVotes,
    criticsScore = criticsScore,
    audienceScore = audienceScore,
    bestPicNom = bestPicNom,
    bestPicWin = bestPicWin,
    bestDirWin = bestDirWin,
    bestActorWin = bestActorWin,
    bestActressWin = bestActressWin,
    top200Box = top200Box,
    director = director,
    actor1 = actor1,
    actor2 = actor2,
    actor3 = actor3,
    actor4 = actor4,
    actor5 = actor5,
    studioVotes = studioVotes,
    castVotes = castVotes,
    studioExperience = studioExperience,
    directorExperience = directorExperience,
    castExperience = castExperience
  )
  return(analysis)
}
