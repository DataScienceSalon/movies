#==============================================================================#
#                                 bivariate                                    #
#==============================================================================#
#' bivariate
#'
#' \code{bivariate} Performs bivariate analysis of predictors on the response
#' variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
bivariate <- function(mdb) {

  # Format Data
  type <- mdb %>% mutate(x = title_type, y = popularity) %>% select(x,y)
  genre <- mdb %>% mutate(x = genre, y = popularity) %>% select(x,y)
  mpaa <- mdb %>% mutate(x = mpaa_rating, y = popularity) %>% select(x,y)
  season <- mdb %>% mutate(x = thtr_rel_season, y = popularity) %>% select(x,y)
  month <- mdb %>% mutate(x = thtr_rel_month, y = popularity) %>% select(x,y)
  votes <- mdb %>% mutate(x = imdb_num_votes, y = popularity) %>% select(x,y)
  logVotes <- mdb %>% mutate(x = log2(imdb_num_votes), y = popularity) %>% select(x,y)
  studioVotes <- mdb %>% mutate(x = studio_votes, y = popularity) %>% select(x,y)
  castTtlVote <- mdb %>% mutate(x = cast_votes, y = popularity) %>% select(x,y)

  # Conduct analysis
  type <- analyzeCategorical(type, "Title Type", bivariate = TRUE)
  genre <- analyzeCategorical(genre, "Genre", bivariate = TRUE)
  mpaa <- analyzeCategorical(mpaa, "MPAA Rating", bivariate = TRUE)
  season <- analyzeCategorical(season, "Season of Release", bivariate = TRUE)
  month <- analyzeCategorical(month, "Month of Release", bivariate = TRUE)
  votes <- analyzeNumeric(votes, "Number of Votes", bivariate = TRUE)
  logVotes <- analyzeNumeric(logVotes, "Log Number of Votes", bivariate = TRUE)
  studioVotes <- analyzeNumeric(studioVotes, "Studio Average Votes", bivariate = TRUE)
  castTtlVote <- analyzeNumeric(castTtlVote, "Studio Average Votes", bivariate = TRUE)

  # Format results
  analysis <- list(
    type = type,
    genre = genre,
    mpaa = mpaa,
    season = season,
    month = month,
    votes = votes,
    logVotes = logVotes,
    studioVotes = studioVotes,
    castTtlVote = castTtlVote
  )

  return(analysis)
}
