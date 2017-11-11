#==============================================================================#
#                             Visualization Functions                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                     Frequency and Proportion Bar Plot                        #
#------------------------------------------------------------------------------#
#' plotFreqProp
#'
#' \code{plotFreqPropTbl} Renders a stacked bar plot and a contingency table
#' showing frequencies and proportions
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param what Capital case character string describing what is being printed
#' @param varName Capital case character string containing the name of the variable for data structures
#' @param varDesc Capital case character string containing the description of the variable for plotting
#' @param order Character c('a', 'd') indicating whether to order by frequency ascending (a), or descending (d)
#'
#' @returns List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotFreqProp <- function(data, what = "Movies", varName, varDesc = NULL, order = NULL) {

  varDesc <- ifelse(is.null(varDesc), varName, varDesc)

  # Format Data
  df <- as.data.frame(data)
  df <- df %>%  group_by(.[[1]]) %>%
    summarize(N = n()) %>%
    mutate(Proportion = round(N / sum(N), 2),
           Cumulative = round(cumsum(Proportion), 2),
           pos = N / 2)
  colnames(df)[1] <- varName

  # Order data
  if (order == "a") {
    df <- df %>% arrange(N)
  } else if (order == "d") {
    df <- df %>% arrange(desc(N))
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = df,
                             ggplot2::aes(x = as.numeric(unlist(df[,1])),
                                          y = as.numeric(unlist(df[,2])),
                                          fill = as.character(unlist(df[,1]))))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = as.numeric(unlist(df[,1])),
                   y = as.numeric(unlist(df[,5])),
                   label = paste0(as.numeric(unlist(df[,2])), " (",
                                  round(as.numeric(unlist(df[,3])) * 100, 0),
                                  "%)")),
      colour="black", family="Tahoma", size = 8) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(values = myPal(length(df[[1]]))) +
    ggplot2::ggtitle(paste(what, 'by', varDesc)) +
    ggplot2::ylab(what) +
    ggplot2::labs(fill = varName)

    visual <- list(
      stats = df[,1:4],
      plot = barPlot
    )
    return(visual)
}


#------------------------------------------------------------------------------#
#                               Plot Histogram                                 #
#------------------------------------------------------------------------------#
#' plotHist
#'
#' \code{plotHist} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame or vector containing a single numeric variable
#' @param what Capital case character string describing what is being printed
#' @param varName Capital case character string containing the name of the variable for data structures
#' @param varDesc Capital case character string containing the description of the variable for plotting
#'
#' @returns List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotHist <- function(data, what = "Movies", varName, varDesc = NULL) {


  df <- data.frame(Min = round(min(data[[1]]), 1),
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
    ggplot2::geom_histogram(position ='identity', color = "palegreen4", fill = "palegreen4") +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(paste(what, 'by', varName)) +
    ggplot2::ylab(what)


  visual <- list(
    stats = df,
    plot = hist
  )

  return(visual)
}

#------------------------------------------------------------------------------#
#                               Plot Quantile                                  #
#------------------------------------------------------------------------------#
#' plotQQ
#'
#' \code{plotQQ} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame or vector containing a single numeric variable
#' @param what Capital case character string describing what is being printed
#'
#' @returns List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotQQ <- function(data, what = "Movies") {


  # Calculate summary statistics
  df <- data.frame(Min = round(min(data[[1]]), 1),
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

  # Render QQ Plot
  qq <- ggplot2::ggplot(data = data, mapping = aes(sample = data[[1]])) +
    qqplotr::stat_qq_band() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_point() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::ggtitle(what, "Normal Q-Q Plot")

  visual <- list(
    stats = df,
    plot = qq
  )

  return(visual)
}
