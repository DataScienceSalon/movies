#==============================================================================#
#                                 process                                      #
#==============================================================================#
#' process
#'
#' \code{process} Pepares the data for the bivariate analysis stage
#'
#' @param mdb Data frame containing the modified movie data sest
#' @param mdbBox Data frame containing 100 random samples from mdb with box office revenue added
#'
#' @return list containing processed mdb and mdbBox data frames
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(mdb, mdbBox) {
  mdb <- mdb %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")
  mdbBox <- mdbBox %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")

  dataSets <- list(
    mdb = mdb,
    mdbBox = mdbBox
  )
  return(dataSets)
}
