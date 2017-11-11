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

  #---------------------------------------------------------------------------#
  #                        Extract Variables of Interest                      #
  #---------------------------------------------------------------------------#
  mdb <- movies %>% select(title, title_type, genre, mpaa_rating, studio,
                           thtr_rel_year, thtr_rel_month, imdb_rating,
                           imdb_num_votes, critics_score, audience_score,
                           best_pic_nom, best_pic_win, best_actor_win,
                           best_actress_win, best_dir_win, top200_box,
                           director, actor1, actor2, actor3, actor4, actor5)
  mdb <- mdb[complete.cases(mdb),]


  #---------------------------------------------------------------------------#
  #                        Create Season and Month                            #
  #---------------------------------------------------------------------------#
  mdb <- mdb %>% mutate(
    thtr_rel_month = as.character(thtr_rel_month),
    thtr_rel_season = ifelse(thtr_rel_month %in% c(3:5), "Spring",
                             ifelse(thtr_rel_month %in% c(6:8), "Summer",
                                    ifelse(thtr_rel_month %in% c(9:11), "Fall",
                                           ifelse(thtr_rel_month %in% c(12), "Holidays", "Winter")))),
    popularity = 10 * imdb_rating + critics_score + audience_score)

  mdb$thtr_rel_month <- factor(mdb$thtr_rel_month, levels = c("1", "2", "3", "4",
                                                              "5", "6", "7", "8",
                                                              "9", "10", "11", "12"))

  #---------------------------------------------------------------------------#
  #                        Create Studio Votes                                #
  #---------------------------------------------------------------------------#
  studio <- movies %>% group_by(studio) %>%
    summarize(studio_votes = sum(imdb_num_votes))
  mdb <- left_join(mdb, studio)

  #---------------------------------------------------------------------------#
  #                           Create Cast Vote                                #
  #---------------------------------------------------------------------------#
  # Create actor score share data frames
  actor1 <- mdb %>% mutate(actor = actor1, votes = .40 * imdb_num_votes) %>%
    select(actor, votes)
  actor2 <- mdb %>% mutate(actor = actor2, votes = .30 * imdb_num_votes) %>%
    select(actor, votes)
  actor3 <- mdb %>% mutate(actor = actor3, votes = .15 * imdb_num_votes) %>%
    select(actor, votes)
  actor4 <- mdb %>% mutate(actor = actor4, votes = .10 * imdb_num_votes) %>%
    select(actor, votes)
  actor5 <- mdb %>% mutate(actor = actor5, votes = .05 * imdb_num_votes) %>%
    select(actor, votes)
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>% summarize(votes = sum(votes))
  actors <- actors[complete.cases(actors),]

  # Merge actor votes into main data frame
  mdb <- left_join(mdb, actors, by = c("actor1" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes1"
  mdb <- left_join(mdb, actors, by = c("actor2" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes2"
  mdb <- left_join(mdb, actors, by = c("actor3" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes3"
  mdb <- left_join(mdb, actors, by = c("actor4" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes4"
  mdb <- left_join(mdb, actors, by = c("actor5" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes5"

  # Create cast votes variable
  mdb <- mdb %>% mutate(cast_votes = votes1 + votes2 + votes3 + votes4 + votes5)

  #---------------------------------------------------------------------------#
  #                        Create Studio Experience                           #
  #---------------------------------------------------------------------------#
  studioExperience <- mdb %>% group_by(studio) %>%
    summarize(studio_experience = n())
  mdb <- left_join(mdb, studioExperience)

  #---------------------------------------------------------------------------#
  #                        Create Director Experience                         #
  #---------------------------------------------------------------------------#
  directorExperience <- mdb %>% group_by(director) %>%
    summarize(director_experience = n())
  mdb <- left_join(mdb, directorExperience)

  #---------------------------------------------------------------------------#
  #                        Create Cast Experience                             #
  #---------------------------------------------------------------------------#
  actor1 <- mdb %>% mutate(actor = actor1) %>% group_by(actor) %>%  summarize(N = n())
  actor2 <- mdb %>% mutate(actor = actor2) %>% group_by(actor) %>%  summarize(N = n())
  actor3 <- mdb %>% mutate(actor = actor3) %>% group_by(actor) %>%  summarize(N = n())
  actor4 <- mdb %>% mutate(actor = actor4) %>% group_by(actor) %>%  summarize(N = n())
  actor5 <- mdb %>% mutate(actor = actor5) %>% group_by(actor) %>%  summarize(N = n())
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>%  summarize(cast_experience = n())
  mdb <- left_join(mdb, actors, by = c("actor1" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce1"
  mdb <- left_join(mdb, actors, by = c("actor2" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce2"
  mdb <- left_join(mdb, actors, by = c("actor3" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce3"
  mdb <- left_join(mdb, actors, by = c("actor4" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce4"
  mdb <- left_join(mdb, actors, by = c("actor5" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce5"
  mdb <- mdb %>% mutate(cast_experience = ce1 + ce2 + ce3 + ce4 + ce5)


  return(mdb)
}
