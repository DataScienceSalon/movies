#==============================================================================#
#                             evaluateModel                                    #
#==============================================================================#
#' evaluateModel
#'
#' \code{evaluateModel} evaluates a model given a response variable and
#' predictors
#'
#' @param movies Data frame with movie features
#' @param omit Numeric indices for variables to omit from the model
#' @param predictors Numeric indices for predictor columns
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
evaluateModel <- function(movies, omit, predictors) {

  lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(movies)[predictors], collapse=" + ")))
  eval <- summary(lm(lrFormula, movies))
  result <- data.frame(Response = "Audience Score",
                       Predictors = paste(colnames(movies)[predictors], collapse = " "),
                       iPredictors = predictors,
                       Omit = colnames(movies)[omit],
                       iOmit = omit,
                       Adjusted.R2 = eval$adj.r.squared,
                       stringsAsFactors = FALSE)
  return(result)

}
