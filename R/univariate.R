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

  # Conduct Analysis
  popularity <- univariateQuant(as.data.frame(mdb$popularity), yLab = "Popularity")
  type <- univariateQual(as.data.frame(mdb$title_type), xLab = "Title Type")
  genre <- univariateQual(as.data.frame(mdb$genre), xLab = "Genre")
  mpaa <- univariateQual(as.data.frame(mdb$mpaa_rating), xLab = "MPAA Rating")
  season <- univariateQual(as.data.frame(mdb$thtr_rel_season), xLab = "Season")
  month <- univariateQual(as.data.frame(mdb$thtr_rel_month), xLab = "Month")
  imdbRating <- univariateQual(as.data.frame(mdb$imdb_rating), xLab = "IMDB Rating")
  imdbVotes <- univariateQuant(as.data.frame(mdb$imdb_num_votes), yLab = "IMDB Votes")
  imdbLogVotes <- univariateQuant(as.data.frame(log2(mdb$imdb_num_votes)), yLab = "IMDB Log Votes")
  criticsScores <- univariateQuant(as.data.frame(mdb$critics_score), yLab = "Critics Score")
  audienceScores <- univariateQuant(as.data.frame(mdb$critics_score), yLab = "Audience Score")
  bestPicNom <- univariateQual(as.data.frame(mdb$best_pic_nom), xLab = "Best Picture Oscar Nomination")
  bestPicWin <- univariateQual(as.data.frame(mdb$best_pic_win), xLab = "Best Picture Oscar")
  bestDirWin <- univariateQual(as.data.frame(mdb$best_dir_win), xLab = "Best Director Oscar")
  bestActorWin <- univariateQual(as.data.frame(mdb$best_actor_win), xLab = "Best Actor Oscar")
  bestActressWin <- univariateQual(as.data.frame(mdb$best_actress_win), xLab = "Best Actress Oscar")
  top200Box <- univariateQual(as.data.frame(mdb$top200_box), xLab = "Top 200 Box Office")
  studioVotes <- univariateQuant(as.data.frame(mdb$studio_votes), yLab = "Studio Votes")
  castVotes <- univariateQuant(as.data.frame(mdb$cast_votes), yLab = "Cast Votes")
  studioExperience <- univariateQuant(as.data.frame(mdb$studio_experience), yLab = "Studio Experience")
  directorExperience <- univariateQuant(as.data.frame(mdb$director_experience), yLab = "Director Experience")
  castExperience <- univariateQuant(as.data.frame(mdb$cast_experience), yLab = "Cast Experience")


  # Return analysis
  analysis <- list(
    popularity = popularity,
    type = type,
    genre = genre,
    mpaa = mpaa,
    season = season,
    month = month,
    imdbRating = imdbRating,
    imdbVotes = imdbVotes,
    imdbLogVotes = imdbLogVotes,
    criticsScores = criticsScores,
    audienceScores = audienceScores,
    bestPicNom = bestPicNom,
    bestPicWin = bestPicWin,
    bestDirWin = bestDirWin,
    bestActorWin = bestActorWin,
    bestActressWin = bestActressWin,
    top200Box = top200Box,
    studioVotes = studioVotes,
    castVotes = castVotes,
    studioExperience = studioExperience,
    directorExperience = directorExperience,
    castExperience = castExperience
  )
  return(analysis)
}
