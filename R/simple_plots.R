# Scatterplot -------------------------------------------------------------

#' simple scatter plot, adding marginal histogram by default.
#' @import ggplot2
#'
#' @param data dataset
#' @param x x
#' @param y y
#' @param size0 point size, default to 1 of nobs<1000, 0.4 if nobs>1000
#' @param alpha0 alpha of point
#' @param dilute a number or logical, dafault to TRUE,
#'  will plot \code{nrow(data_long)/dilute} data. For example,
#'  if dilute = 5 will plot 1/5 of the data.
#' @param add_hist optional to add marginal histogram
#' using `ggExtra::ggMarginal` but notice
#' if add histogram, what is returned is no longer a ggplot2 object
#'
#' @return ggplot2 object if `add_hist = FALSE`
#' @examples
#' scatter.plot.simple(data = shap_score, x = "dayint", y = "AOT_Uncertainty")
#'
#' @export scatter.plot.simple
#'
scatter.plot.simple <-  function(data, x, y, size0 = 0.1, alpha0 = 0.3,
                                 dilute = FALSE,
                                 add_hist = TRUE){
  set.seed(1234)
  if (is.null(dilute)) dilute = FALSE
  if (dilute!=0){
    dilute <- ceiling(min(nrow(data)/10, abs(as.numeric(dilute))))
    # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data <- data[sample(nrow(data), min(nrow(data)/dilute, 1500))]
  }
  if (is.null(size0)) size0 <- if(nrow(data)<1000L) 1 else 0.4

  plot0 <- ggplot(data = data, aes(x = data[[x]], y = data[[y]]))+
    geom_point(size = size0, alpha = alpha0) +
    # geom_density_2d(aes(fill = ..level..), geom = "polygon") +
    labs(x = x, y = y) +
    theme_bw()

  # add histogram by default
  if(add_hist){
    plot0 <- ggExtra::ggMarginal(plot0, type = "histogram",
                                 bins = 50, size = 10, color="white")
  }
  return(plot0)
}

#' make customized scatter plot with diagonal line and R2 printed.
#' @importFrom ggpubr stat_cor
#' @import data.table
#' @importFrom BBmisc capitalizeStrings
#'
#' @param data dataset
#' @param x x
#' @param y y
#' @param dilute a number or logical, dafault to TRUE, will
#' plot \code{nrow(data_long)/dilute} data. For example,
#' if dilute = 5 will plot 1/5 of the data.
#' @param add_abline default to FALSE, add a diagonal line
#' @param add_hist optional to add marginal histogram using
#' `ggExtra::ggMarginal` but notice
#' if add histogram, what is returned is no longer a ggplot2 object
#' @return ggplot2 object if `add_hist = FALSE`
#' @examples
#' scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")
#'
#' @export scatter.plot.diagonal
#'
scatter.plot.diagonal <- function(data, x, y,
                                  dilute = FALSE,
                                  add_abline = FALSE,
                                  add_hist = TRUE){
  set.seed(1234)
  if (is.null(dilute)) dilute = FALSE
  if (dilute!=0){
    dilute <- ceiling(min(nrow(data)/10, abs(as.numeric(dilute))))
    # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data <- data[sample(nrow(data), min(nrow(data)/dilute, 1500))]
  }

  plot1 <-  ggplot(data = data, aes(x = data[[x]], y = data[[y]]))+
    geom_point(size = 0.1, alpha = 0.3) +
    theme_bw() +
    geom_smooth(method = 'lm') +
    labs(x = BBmisc::capitalizeStrings(x), y = BBmisc::capitalizeStrings(y)) +
    ggpubr::stat_cor(method = "pearson")



  if(add_abline){
    plot1 <- plot1 + geom_abline(intercept =0 , slope = 1, color = "grey")
  }

  print(paste("R2 is",
              signif(summary(lm(data[[y]]~data[[x]],
                                na.action = na.omit))$r.squared, 4),
              "."))
  # customize labels
  if(add_hist){
    plot1 <- ggExtra::ggMarginal(plot1, type = "histogram",
                                 bins = 50, size = 10, color="white")
  }
  return(plot1)
}
