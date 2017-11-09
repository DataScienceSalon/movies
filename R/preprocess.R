#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Performs preprocessing of data for analysis
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
preprocess <- function(movies) {

  # Change month to factor and create season variable
  mdb <- movies %>% mutate(
    thtr_rel_month = as.character(thtr_rel_month),
    thtr_rel_season = ifelse(thtr_rel_month %in% c(3:5), "Spring",
                             ifelse(thtr_rel_month %in% c(6:8), "Summer",
                                    ifelse(thtr_rel_month %in% c(9:11), "Fall",
                                           ifelse(thtr_rel_month %in% c(12), "Holidays", "Winter")))))

  mdb$thtr_rel_month <- factor(mdb$thtr_rel_month, levels = c("1", "2", "3", "4",
                                                              "5", "6", "7", "8",
                                                              "9", "10", "11", "12"))

  # Create Director Popularity Data Set
  dirPop <- mdb %>% group_by(director, thtr_rel_year) %>%
    summarize(dir_imdb_rating = sum(imdb_rating),
           dir_imdb_votes = sum(imdb_num_votes),
           dir_critics_score = sum(critics_score),
           dir_audience_score = sum(audience_score)) %>%
    select(director, thtr_rel_year, dir_imdb_rating, dir_imdb_votes, dir_critics_score,
           dir_audience_score)

  # Create Cast Popularity Data Set
  castPop1 <- mdb %>% mutate(actor = actor1)
  castPop1 <- castPop1 %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(imdb_rating),
           cast_imdb_votes = sum(imdb_num_votes),
           cast_critics_score = sum(critics_score),
           cast_audience_score = sum(audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)

  castPop2 <- mdb %>% mutate(actor = actor2)
  castPop2 <- castPop2 %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(imdb_rating),
              cast_imdb_votes = sum(imdb_num_votes),
              cast_critics_score = sum(critics_score),
              cast_audience_score = sum(audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)

  castPop3 <- mdb %>% mutate(actor = actor3)
  castPop3 <- castPop3 %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(imdb_rating),
              cast_imdb_votes = sum(imdb_num_votes),
              cast_critics_score = sum(critics_score),
              cast_audience_score = sum(audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)

  castPop4 <- mdb %>% mutate(actor = actor4)
  castPop4 <- castPop4 %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(imdb_rating),
              cast_imdb_votes = sum(imdb_num_votes),
              cast_critics_score = sum(critics_score),
              cast_audience_score = sum(audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)

  castPop5 <- mdb %>% mutate(actor = actor5)
  castPop5 <- castPop5 %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(imdb_rating),
              cast_imdb_votes = sum(imdb_num_votes),
              cast_critics_score = sum(critics_score),
              cast_audience_score = sum(audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)


  castPop <- rbind(castPop1, castPop2, castPop3, castPop4, castPop5)
  castPop <- castPop %>% group_by(actor, thtr_rel_year) %>%
    summarize(cast_imdb_rating = sum(cast_imdb_rating),
              cast_imdb_votes = sum(cast_imdb_votes),
              cast_critics_score = sum(cast_critics_score),
              cast_audience_score = sum(cast_audience_score)) %>%
    select(actor, thtr_rel_year, cast_imdb_rating, cast_imdb_votes, cast_critics_score,
           cast_audience_score)


  # Add director popularity variables
  mdb <- mdb %>% rowwise() %>%  mutate(
    dir_imdb_rating = sum(dirPop$dir_imdb_rating[dirPop$director == director &
                                                           dirPop$thtr_rel_year <= thtr_rel_year]),
    dir_imdb_votes = sum(dirPop$dir_imdb_votes[dirPop$director == director &
                                                           dirPop$thtr_rel_year <= thtr_rel_year]),
    dir_critics_score = sum(dirPop$dir_critics_score[dirPop$director == director &
                                                           dirPop$thtr_rel_year <= thtr_rel_year]),
    dir_audience_score = sum(dirPop$dir_audience_score[dirPop$director == director &
                                                           dirPop$thtr_rel_year <= thtr_rel_year])
    )
  mdb$dir_imdb_rating[is.na(mdb$dir_imdb_rating)] <- 0
  mdb$dir_imdb_votes[is.na(mdb$dir_imdb_votes)] <- 0
  mdb$dir_critics_score[is.na(mdb$dir_critics_score)] <- 0
  mdb$dir_audience_score[is.na(mdb$dir_audience_score)] <- 0

  # Add actor1 popularity variables
  mdb <- mdb %>% rowwise() %>% mutate(
    actor1_imdb_rating =
      sum(castPop$cast_imdb_rating[castPop$actor == actor1 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor1_imdb_votes =
      sum(castPop$cast_imdb_votes[castPop$actor == actor1 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor1_critics_score =
      sum(castPop$cast_critics_score[castPop$actor == actor1 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor1_audience_score =
      sum(castPop$cast_audience_score[castPop$actor == actor1 & castPop$thtr_rel_year <= thtr_rel_year]))
  mdb$actor1_imdb_rating[is.na(mdb$actor1_imdb_rating)] <- 0
  mdb$actor1_imdb_votes[is.na(mdb$actor1_imdb_votes)] <- 0
  mdb$actor1_critics_score[is.na(mdb$actor1_critics_score)] <- 0
  mdb$actor1_audience_score[is.na(mdb$actor1_audience_score)] <- 0

  # Add actor2 popularity variables
  mdb <- mdb %>% rowwise() %>% mutate(
    actor2_imdb_rating =
      sum(castPop$cast_imdb_rating[castPop$actor == actor2 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor2_imdb_votes =
      sum(castPop$cast_imdb_votes[castPop$actor == actor2 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor2_critics_score =
      sum(castPop$cast_critics_score[castPop$actor == actor2 & castPop$thtr_rel_year <= thtr_rel_year]),
    actor2_audience_score =
      sum(castPop$cast_audience_score[castPop$actor == actor2 & castPop$thtr_rel_year <= thtr_rel_year]))
  mdb$actor2_imdb_rating[is.na(mdb$actor2_imdb_rating)] <- 0
  mdb$actor2_imdb_votes[is.na(mdb$actor2_imdb_votes)] <- 0
  mdb$actor2_critics_score[is.na(mdb$actor2_critics_score)] <- 0
  mdb$actor2_audience_score[is.na(mdb$actor2_audience_score)] <- 0
}
