#==============================================================================#
#                                     mlr                                      #
#==============================================================================#
#' mlr
#'
#' \code{mlr} mlres the data set and creates data sets used
#' for analysis.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
mlr <- function(movies) {

  evalModel <- function(data, predictors) {

    predictorSet <- lapply(predictors, function(x) {
      p <- list(
        omit = x,
        predictors = predictors[(!(predictors %in% x))]
      )
      p
    })
    model <- rbindlist(lapply(seq_along(predictorSet), function(x) {
      lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[predictorSet[[x]]$predictors], collapse=" + ")))
      eval <- summary(lm(lrFormula, data))
      data.frame(Response = "Audience Score",
                 Predictor = paste(colnames(data)[predictorSet[[x]]$predictors], collapse = " "),
                 Omit = paste(colnames(data)[predictorSet[[x]]$omit], collapse = " "),
                 iOmit = predictorSet[[x]]$omit,
                 Adjusted.R2 = eval$adj.r.squared,
                 stringsAsFactors = FALSE)
    }))
    model
    return(model)
  }

  getBestModel <- function(data, predictors, baseAR2) {
    newModels <- evalModel(data, predictors)
    if (baseAR2 < newModels %>% summarize(max(Adjusted.R2))) {
      omit <-  newModels %>% filter(Adjusted.R2 == max(Adjusted.R2)) %>% select(iOmit)
      baseAR2 <- newModels %>% summarize(max(Adjusted.R2))
      predictors <- predictors[(!(predictors %in% omit))]
      return(getBestModel(data, predictors, baseAR2))
    } else {
      return(newModels)
    }
  }

  data <- movies %>% select(title_type, genre, runtime,
                            mpaa_rating, thtr_rel_month, dvd_rel_month,
                            imdb_rating, imdb_num_votes, critics_score,
                            best_pic_nom, best_pic_win, best_actor_win,
                            best_actress_win, best_dir_win, top200_box,
                            audience_score)

  predictors <- c(1:15)
  lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[predictors], collapse=" + ")))
  baseModel <- summary(lm(lrFormula, data))
  baseModel <- data.frame(Response = "Audience Score",
                          Omit = "None",
                          iOmit = NA,
                          Adjusted.R2 = baseModel$adj.r.squared,
                          stringsAsFactors = FALSE)
  bestModel <- getBestModel(data, predictors, baseAR2 = baseModel$adj.r.squared)

}
