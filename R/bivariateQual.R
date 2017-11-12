#==============================================================================#
#                             bivariateQual                                    #
#==============================================================================#
#' bivariateQual
#'
#' \code{bivariateQual} Performs bivariate analysis of a categorical variable
#' on a single quantitative variable.
#'
#' @param data Two column data frame with categorical variable in column 1 and
#'             the quantitative variable in column 2#'
#' @param xLab Capitalized character string for the categorical variable
#' @param yLab Capitalized character string for the quantitative variable
#'
#' @return analysis plots, summary statistics, test results and interpretive text
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
bivariateQual <- function(data, xLab, yLab) {

  # Plots the data
  box <- plotBox(data, xLab = xLab, yLab = yLab)
  groups <- length(unique(data[[1]]))
  normalPlots <

  return(bp)
}
