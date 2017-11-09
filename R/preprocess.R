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

  mdb <- movies %>% mutate(
    thtr_rel_month = as.character(thtr_rel_month),
    thtr_rel_season = ifelse(thtr_rel_month %in% c(3:5), "Spring",
                             ifelse(thtr_rel_month %in% c(6:8), "Summer",
                                    ifelse(thtr_rel_month %in% c(9:11), "Fall",
                                           ifelse(thtr_rel_month %in% c(12), "Holidays", "Winter")))),
    audience_size = (imdb_num_votes * 100) +
      (imdb_num_votes * 100)^(audience_score  / mean(audience_score)))

  mdb$thtr_rel_month <- factor(mdb$thtr_rel_month, levels = c("1", "2", "3", "4",
                                                              "5", "6", "7", "8",
                                                              "9", "10", "11", "12"))

  return(mdb)
}
