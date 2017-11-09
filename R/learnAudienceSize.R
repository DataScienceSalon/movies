#==============================================================================#
#                             learnAudienceSize                                #
#==============================================================================#
#' learnAudienceSize
#'
#' \code{learnAudienceSize} Performs learnAudienceSizeing of data for analysis
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
learnAudienceSize <- function(movies) {

  mdb <- openxlsx::read.xlsx("./data/mdb.xlsx")

  data <- mdb %>% select(imdb_rating, imdb_num_votes, box_office) %>%
  filter(box_office > 0) %>%
    mutate(imdb_rate_vote == imdb_rating * imdb_num_votes)
  selected <- c(1:2)

  lrFormula <- formula(paste("box_office ~ ",
                             paste(colnames(data)[unique(selected)], collapse=" + ")))
  finalModel <- lm(lrFormula, data)
  summary(finalModel)
  return(mdb)
}
