#==============================================================================#
#                             analyzeNumeric                                   #
#==============================================================================#
#' analyzeNumeric
#'
#' \code{analyzeNumeric} Performs univariate analysis of numeric variables
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
analyzeNumeric <- function(data, var, bivariate = FALSE) {

  if (bivariate == FALSE) {

    stats <- data.frame(Min = round(min(data[[1]]), 1),
                        Q1 = round(quantile(data[[1]], 0.25), 1),
                        Median = round(median(data[[1]]), 1),
                        Mean = round(mean(data[[1]]), 1),
                        Q3 = round(quantile(data[[1]], 0.75), 1),
                        Max = round(max(data[[1]]), 1),
                        `NA's` = sum(is.na(data[[1]])),
                        SD = round(sd(data[[1]]), 2),
                        IQR = round(quantile(data[[1]], 0.75) - quantile(data[[1]], 0.25) , 1),
                        Kurtosis = e1071::kurtosis(data[[1]]),
                        Skewness = e1071::skewness(data[[1]]),
                        row.names = NULL)

    hist <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = data[[1]])) +
      ggplot2::geom_histogram(position ='identity', color = "darkgreen", fill = "darkgreen") +
      ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::ggtitle(paste('Movies by', var))

    box <- ggplot2::ggplot(data, ggplot2::aes(x = "X", y = data[[1]])) +
      ggplot2::geom_boxplot() + ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 24) +
      ggplot2::scale_fill_brewer(palette = "Greens") +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::ggtitle(paste(var))


    analysis <- list(
      stats = stats,
      hist = hist,
      box = box
    )
  } else {
    mod <- lm(y~x, data)
    aov <- anova(mod)
    cv <- cor(data$x, data$y)

    myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

    scatter <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal(base_size = 24) +
      ggplot2::geom_smooth(method = lm, se = FALSE) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     legend.position = "right") +
      ggplot2::labs(x = var, y = "Popularity") +
      ggplot2::scale_fill_manual(values = myPal(length(unique(data$x)))) +
      ggplot2::ggtitle(paste('Popularity by', var))

    analysis <- list(
      mod = broom::tidy(mod),
      aov = broom::tidy(aov),
      cv = cv,
      r2 = broom::glance(mod),
      plot = scatter
    )

  }
  return(analysis)
}
