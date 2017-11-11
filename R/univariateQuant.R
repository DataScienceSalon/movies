#==============================================================================#
#                             univariateQuant                                  #
#==============================================================================#
#' univariateQuant
#'
#' \code{univariateQuant} Performs univariate analysis on a single quantitative
#' variable.  The function returns descriptive statistics, a histogram
#' and a box plot to check outliers.
#'
#' @param data Single column data frame containing the quantitative variable
#' @param yLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#'  1. Summary Statistics
#'  2. Histogram
#'  3. Normal quantile plot
#'  4. Box plot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
univariateQuant <- function(data, yLab) {

  stats <- getSummaryStats(data)
  hist <- plotHist(data, yLab = yLab)
  qq <- plotQQ(data, yLab = yLab)
  box  <- plotBox(data, yLab = yLab)

  analysis <- list(
    stats = stats,
    hist = hist,
    qq = qq,
    box = box
  )

  return(analysis)
}
