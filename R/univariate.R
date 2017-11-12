#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @param mdb Data frame containing the preprocessed movie data set
#' @param mdbBox Data frame containing 100 samples from the mdb data frame with box office revenue added
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
univariate <- function(mdb, mdbBox) {

  # Conduct Qualitative Analysis
  type <- univariateQual(as.data.frame(mdb$title_type), xLab = "Title Type")
  genre <- univariateQual(as.data.frame(mdb$genre), xLab = "Genre")
  mpaa <- univariateQual(as.data.frame(mdb$mpaa_rating), xLab = "MPAA Rating")
  season <- univariateQual(as.data.frame(mdb$thtr_rel_season), xLab = "Season")
  month <- univariateQual(as.data.frame(mdb$thtr_rel_month), xLab = "Month")
  bestPicNom <- univariateQual(as.data.frame(mdb$best_pic_nom), xLab = "Best Picture Oscar Nomination")
  bestPicWin <- univariateQual(as.data.frame(mdb$best_pic_win), xLab = "Best Picture Oscar")
  bestDirWin <- univariateQual(as.data.frame(mdb$best_dir_win), xLab = "Best Director Oscar")
  bestActorWin <- univariateQual(as.data.frame(mdb$best_actor_win), xLab = "Best Actor Oscar")
  bestActressWin <- univariateQual(as.data.frame(mdb$best_actress_win), xLab = "Best Actress Oscar")
  top200Box <- univariateQual(as.data.frame(mdb$top200_box), xLab = "Top 200 Box Office")

  # Conduct Quantitative Analysis
  studioExperience <- univariateQuant(data.frame(mdb$title, mdb$studio_experience),
                                      yLab = "Studio Experience", units = "films")
  studioExperienceLog <- univariateQuant(data.frame(mdb$title, mdb$studio_experience_log),
                                      yLab = "Studio Experience", units = "log(films)")
  directorExperience <- univariateQuant(data.frame(mdb$title, mdb$director_experience),
                                        yLab = "Director Experience", units = "films")
  directorExperienceLog <- univariateQuant(data.frame(mdb$title, mdb$director_experience_log),
                                        yLab = "Director Experience", units = "log(films)")
  castExperience <- univariateQuant(data.frame(mdb$title, mdb$cast_experience),
                                    yLab = "Cast Experience", units = "films")
  castExperienceLog <- univariateQuant(data.frame(mdb$title, mdb$cast_experience_log),
                                    yLab = "Cast Experience", units = "log(films)")
  imdbVotes <- univariateQuant(data.frame(mdb$title, mdb$imdb_num_votes),
                               yLab = "IMDB Votes", units = "votes")
  imdbVotesLog <- univariateQuant(data.frame(mdb$title, mdb$imdb_num_votes_log),
                                  yLab = "IMDB Log Votes", units = "log votes")
  imdbRating <- univariateQuant(data.frame(mdb$title, mdb$imdb_rating),
                               yLab = "IMDB Rating", units = "points")
  criticsScores <- univariateQuant(data.frame(mdb$title, mdb$critics_score),
                                   yLab = "Critics Score", units = "points")
  audienceScores <- univariateQuant(data.frame(mdb$title, mdb$critics_score),
                                    yLab = "Audience Score", units = "points")
  studioVotes <- univariateQuant(data.frame(mdb$title, mdb$studio_votes),
                                 yLab = "Studio Votes", units = "votes")
  studioVotesLog <- univariateQuant(data.frame(mdb$title, mdb$studio_votes_log),
                                 yLab = "Log Studio Votes", units = "log(votes)")
  castVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_votes),
                               yLab = "Cast Votes", units = "votes")
  castVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_log),
                               yLab = "Log Cast Votes", units = "log(votes)")
  scores <- univariateQuant(data.frame(mdb$title, mdb$scores),
                            yLab = "Total Scores", units = "points")
  scoresLog <- univariateQuant(data.frame(mdb$title, mdb$scores_log),
                            yLab = "Log Total Scores", units = "points")
  votesImdbRating <- univariateQuant(data.frame(mdb$title, mdb$votes_imdb_rating),
                                     yLab = "Votes * IMDB Rating", units = "points")
  votesImdbRatingLog <- univariateQuant(data.frame(mdb$title, mdb$votes_imdb_rating_log),
                                        yLab = "Log(Votes * IMDB Rating)", units = "log points")
  votesCriticsScore <- univariateQuant(data.frame(mdb$title, mdb$votes_critics_score),
                                       yLab = "Votes * Critics Score", units = "points")
  votesCriticsScoreLog <- univariateQuant(data.frame(mdb$title, mdb$votes_critics_score_log),
                                          yLab = "Log(Votes * Critics Score)", units = "log points")
  votesAudienceScore <- univariateQuant(data.frame(mdb$title, mdb$votes_audience_score),
                                        yLab = "Votes * Audience Score", units = "points")
  votesAudienceScoreLog <- univariateQuant(data.frame(mdb$title, mdb$votes_audience_score_log),
                                           yLab = "Log(Votes * Audience Score)", units = "log points")
  votesScores <- univariateQuant(data.frame(mdb$title, mdb$votes_scores),
                                 yLab = "Votes * Total Score", units = "points")
  votesScoresLog <- univariateQuant(data.frame(mdb$title, mdb$votes_scores_log),
                                    yLab = "Log(Votes * Total Score)", units = "log points")
  boxOffice <- univariateQuant(data.frame(mdbBox$title, mdbBox$box_office),
                                 yLab = "Box Office", units = "dollars")
  boxOfficeLog <- univariateQuant(data.frame(mdbBox$title, mdbBox$box_office_log),
                               yLab = "Log Box Office", units = "log(dollars)")


  # Return analysis
  analysis <- list(
    type = type,
    genre = genre,
    mpaa = mpaa,
    season = season,
    month = month,
    bestPicNom = bestPicNom,
    bestPicWin = bestPicWin,
    bestDirWin = bestDirWin,
    bestActorWin = bestActorWin,
    bestActressWin = bestActressWin,
    top200Box = top200Box,
    studioExperience = studioExperience,
    studioExperienceLog = studioExperienceLog,
    directorExperience = directorExperience,
    directorExperienceLog = directorExperienceLog,
    castExperience = castExperience,
    castExperienceLog = castExperienceLog,
    imdbVotes = imdbVotes,
    imdbVotesLog = imdbVotesLog,
    imdbRating = imdbRating,
    criticsScores = criticsScores,
    audienceScores = audienceScores,
    studioVotes = studioVotes,
    studioVotesLog = studioVotesLog,
    castVotes = castVotes,
    castVotesLog = castVotesLog,
    scores = scores,
    scoresLog = scoresLog,
    votesImdbRating = votesImdbRating,
    votesImdbRatingLog = votesImdbRatingLog,
    votesCriticsScore = votesCriticsScore,
    votesCriticsScoreLog = votesCriticsScoreLog,
    votesAudienceScore = votesAudienceScore,
    votesAudienceScoreLog = votesAudienceScoreLog,
    votesScores = votesScores,
    votesScoresLog = votesScoresLog,
    boxOffice = boxOffice,
    boxOfficeLog = boxOfficeLog
  )
  return(analysis)
}
