#==============================================================================#
#                                 bivariate                                    #
#==============================================================================#
#' bivariate
#'
#' \code{bivariate} Performs bivariate analysis of variables
#'
#' @param mdb Data frame containing the preprocessed movie data set
#' @param mdbBox Data frame containing 100 samples from the mdb data frame with box office revenue added
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
bivariate <- function(mdb, mdbBox) {

  #---------------------------------------------------------------------------#
  #                       Box Office Revenue Analysis                         #
  #---------------------------------------------------------------------------#
  type <- mdbBox %>% select(title_type, box_office_log)
  type <- bivariateQual(data = type, xLab = "Type", yLab = "Log Box Office")

  analysis <- list(
    boxOffice = list(
      type = type
    )
  )
  return(analysis)

}
