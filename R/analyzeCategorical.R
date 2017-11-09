#==============================================================================#
#                             analyzeCategorical                               #
#==============================================================================#
#' analyzeCategorical
#'
#' \code{analyzeCategorical} Performs univariate analysis of categorical variables
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
analyzeCategorical <- function(data, var) {

  me <- 0.05
  alpha <- 0.05
  z <- (1-alpha/2)

  stats <- data %>%
    mutate(Level = data[,1],
           N = N,
           Proportion = round(p, 2),
           Cumulative = round(cumsum(Proportion), 2),
           pos = N / 2)

  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  barplot <- ggplot2::ggplot(data = stats,
                             ggplot2::aes(x = Level, y = N, fill = Level)) +
    ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_text(
      data = stats,
      ggplot2::aes(x = Level, y = pos,
                   label = paste0(N, " (",round(Proportion * 100, 0),"%)")),
      colour="black", family="Tahoma", size = 8) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(values = myPal(length(stats$Level))) +
    ggplot2::ggtitle(paste('Frequency and Proportion of Movies by', var))

  stats <- stats %>% select(Level, N, Proportion, Cumulative)

  analysis <- list(
    stats = stats,
    plot = barplot
  )

  return(analysis)
}
