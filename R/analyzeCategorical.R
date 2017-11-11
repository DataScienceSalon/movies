#==============================================================================#
#                             analyzeCategorical                               #
#==============================================================================#
#' analyzeCategorical
#'
#' \code{analyzeCategorical} Performs univariate analysis of categorical variables
#'
#' @param data Data frame containing data to be analyzed
#' @param var Character string containing the name of the variable or predictor
#' @param bivariate Logical indicating if the analysis is bivariate
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
analyzeCategorical <- function(data, var, bivariate = FALSE) {

  if (bivariate != TRUE) {

    stats <- data.frame(Min = round(min(data$N), 1),
                        Q1 = round(quantile(data$N, 0.25), 1),
                        Median = round(median(data$N), 1),
                        Mean = round(mean(data$N), 1),
                        Q3 = round(quantile(data$N, 0.75), 1),
                        Max = round(max(data$N), 1),
                        `NA's` = sum(is.na(data$N)),
                        SD = round(sd(data$N), 2),
                        IQR = round(quantile(data$N, 0.75) - quantile(data$N, 0.25) , 1),
                        row.names = NULL)

    ctable <- data %>%
      mutate(Level = data[,1],
             N = N,
             Proportion = round(p, 2),
             Cumulative = round(cumsum(Proportion), 2),
             pos = N / 2)

    myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

    barplot <- ggplot2::ggplot(data = ctable,
                               ggplot2::aes(x = Level, y = N, fill = Level)) +
      ggplot2::geom_bar(stat='identity') +
      ggplot2::theme_minimal(base_size = 24) +
      ggplot2::geom_text(
        data = ctable,
        ggplot2::aes(x = Level, y = pos,
                     label = paste0(N, " (",round(Proportion * 100, 0),"%)")),
        colour="black", family="Tahoma", size = 8) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_manual(values = myPal(length(ctable$Level))) +
      ggplot2::ggtitle(paste('Movies by', var))

    ctable <- ctable %>% select(Level, N, Proportion, Cumulative)

    analysis <- list(
      stats = stats,
      ctable = ctable,
      plot = barplot
    )
  } else {
    mod <- lm(y~x, data)
    aov <- anova(mod)

    myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

    boxplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = x, y = y, fill = x)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::labs(y = "Popularity", fill = var) +
      ggplot2::scale_fill_manual(values = myPal(length(unique(data$x)))) +
      ggplot2::ggtitle(paste('Movie Popularity by', var))

    analysis <- list(
      mod = broom::tidy(mod),
      aov = broom::tidy(aov),
      r2 = broom::glance(mod),
      plot = boxplot
    )
  }

  return(analysis)
}
