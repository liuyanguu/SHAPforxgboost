# SHAP visualization functions for XGBoost,
# wrapped functions for
# summary plot, dependence plot, force plot, and interaction effect plot
# Further explained on my research blog: https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
# The package was applied in paper \doi{https://doi.org/10.5281/zenodo.3568449}
#
# Please cite if this is useful.
#

# Data preparation functions ----------------------------------------------

#' Get SHAP scores from a trained XGBoost or LightGBM model
#'
#' \code{shap.values} returns a list of three objects from XGBoost or LightGBM
#' model: 1. a dataset (data.table) of SHAP scores. It has the same dimension as
#' the X_train); 2. the ranked variable vector by each variable's mean absolute
#' SHAP value, it ranks the predictors by their importance in the model; and 3.
#' The BIAS, which is like an intercept. The rowsum of SHAP values including the
#' BIAS would equal to the predicted value (y_hat) generally speaking.
#'
#' @param xgb_model an XGBoost or LightGBM model object
#' @param X_train the data supplied to the `predict` function to get the
#'   prediction. It should be a matrix. Notice that coercing the matrix to a
#'   dense matrix by using `as.matrix` might lead to wrong behaviors in some
#'   cases. See discussion in issues on this topic.
#'
#' @import data.table
#' @import xgboost
#' @importFrom stats cutree dist hclust predict lm na.omit
#'
#' @export shap.values
#'
#' @return a list of three elements: the SHAP values as data.table, ranked
#'   mean|SHAP|, and BIAS
#'
#' @example R/example/example_fit_summary.R
#'
shap.values <- function(xgb_model,
                        X_train){

  shap_contrib <- predict(xgb_model,
                          (X_train),
                          predcontrib = TRUE)

  # Add colnames if not already there (required for LightGBM)
  if (is.null(colnames(shap_contrib))) {
    colnames(shap_contrib) <- c(colnames(X_train), "BIAS")
  }

  shap_contrib <- as.data.table(shap_contrib)

  # For both XGBoost and LightGBM, the baseline value is kept in the last column
  BIAS0 <- shap_contrib[, ncol(shap_contrib), with = FALSE][1]

  # Remove baseline and ensure the shap matrix has column names
  shap_contrib[, `:=`(BIAS, NULL)]

  # Make SHAP score in decreasing order
  imp <- colMeans(abs(shap_contrib))
  mean_shap_score <- imp[order(imp, decreasing = T)]

  return(list(shap_score = shap_contrib,
              mean_shap_score = mean_shap_score,
              BIAS0 = BIAS0))
}



#' Prepare SHAP values into long format for plotting
#'
#' Produce a dataset of 6 columns: ID of each observation, variable name, SHAP
#' value, variable values (feature value), deviation of the feature value for
#' each observation (for coloring the point), and the mean SHAP values for each
#' variable. You can view this example dataset included in the package:
#' \code{\link{shap_long_iris}}
#'
#' The ID variable is added for each observation in the `shap_contrib` dataset
#' for better tracking, it is created as `1:nrow(shap_contrib)` before melting
#' `shap_contrib` into long format.
#'
#' @param xgb_model an XGBoost (or LightGBM) model object, will derive the SHAP values from it
#' @param shap_contrib optional to directly supply a SHAP values dataset. If
#'   supplied, it will overwrite the `xgb_model` if `xgb_model` is also supplied
#' @param X_train the dataset of predictors used to calculate SHAP values, it
#'   provides feature values to the plot, must be supplied
#' @param top_n to choose top_n variables ranked by mean|SHAP| if needed
#' @param var_cat if supplied, will provide long format data, grouped by this
#'   categorical variable
#'
#' @import data.table
#' @export shap.prep
#'
#' @return a long-format data.table, named as `shap_long`
#'
#' @example R/example/example_fit_summary.R
#' @example R/example/example_categorical.R
#'
shap.prep <- function(xgb_model = NULL,
                      shap_contrib = NULL, # optional to directly supply SHAP values
                      X_train,
                      top_n = NULL,
                      var_cat = NULL
                      ){
  if (is.null(xgb_model) & is.null(shap_contrib)) stop("Please provide either `xgb_model` or `shap_contrib`")
  if (!is.null(shap_contrib)){
    if(paste0(dim(shap_contrib$shap_score), collapse = " ") != paste0(dim(X_train), collapse = " ")) stop("supply correct shap_contrib, remove BIAS column.\n")
  }

  # prep long-data
  shap <- if (is.null(shap_contrib)) shap.values(xgb_model, X_train) else list(
      shap_score = shap_contrib,
      mean_shap_score = colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = TRUE)]
    )
  std1 <- function(x){
    return ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

  # choose top n features
  if (is.null(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  top_n <- as.integer(top_n)
  if (!top_n%in%c(1:dim(X_train)[2])) {
    message ('Please supply correct top_n, by default use all features.\n')
    top_n <- dim(X_train)[2]
  }

  # arrange variables in descending order, thus the summary plot could be
  # plotted accordingly.
  shap_score_sub <- setDT(shap$shap_score)[, names(shap$mean_shap_score)[1:top_n], with = FALSE]
  shap_score_sub[, ID:= .I]
  # fv: feature values: the values in the original dataset
  # fv_sub: subset of feature values
  # since dayint is int, the package example will throw a warning here
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]

  if(is.null(var_cat)){
    # shap_score_sub contains the sample ID
    shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(fv_sub))
    vars_wanted <- colnames(fv_sub)

  } else if (var_cat%in%colnames(fv_sub)) {
    # exclude var_cat as it is used as a categorical group
    shap_score_long <- melt.data.table(shap_score_sub[,-..var_cat], measure.vars = colnames(fv_sub)[!colnames(fv_sub) %in% c(var_cat, "ID")])
    vars_wanted <- colnames(fv_sub)[!colnames(fv_sub) %in% var_cat]
  } else {
    stop("Please provide a correct var_cat variable, a categorical variable that
         exists in the dataset.")
  }
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = vars_wanted, value.name = "rfvalue")
  fv_sub_long[, stdfvalue := std1(rfvalue), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue;
  # standarized: stdfvalue
  if(is.null(var_cat)){
    shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  } else {
    shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue', var_cat), with = FALSE])
  }
  # mean_value: mean abs SHAP values by variable, used as the label by
  # `geom_text` in the summary plot
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2)
}


#' Prepare the interaction SHAP values from predict.xgb.Booster
#'
#' `shap.prep.interaction` just runs \code{shap_int <- predict(xgb_mod, (X_train), predinteraction = TRUE)}, thus it may not be necessary.
#' Read more about the xgboost predict function at `xgboost::predict.xgb.Booster`. Note that this functionality is unavailable for LightGBM models.
#'
#' @param xgb_model a xgboost model object
#' @param X_train the dataset of predictors used for the xgboost model
#'
#' @import xgboost
#' @import data.table
#' @export shap.prep.interaction
#' @return a 3-dimention array: #obs x #features x #features
#' @example R/example/example_interaction_plot.R
shap.prep.interaction <- function(xgb_model, X_train){
  stopifnot(inherits(xgb_model, "xgb.Booster"))
  shap_int <- predict(xgb_model, (X_train), predinteraction = TRUE)
  return(shap_int)
}

# SHAP summary plot ----------------------------------------------------------

#' SHAP summary plot core function using the long format SHAP values
#'
#' The summary plot (a sina plot) uses a long format data of SHAP values. The
#' SHAP values could be obtained from either a XGBoost/LightGBM model or a SHAP value
#' matrix using \code{\link{shap.values}}. So this summary plot function
#' normally follows the long format dataset obtained using `shap.values`. If you
#' want to start with a model and data_X, use
#' \code{\link{shap.plot.summary.wrap1}}. If you want to use a self-derived
#' dataset of SHAP values, use \code{\link{shap.plot.summary.wrap2}}. If a list
#' named **new_labels** is provided in the global environment (`new_labels` is
#' pre-loaded by the package as \code{NULL}), the plots will use that list to
#' label the variables, here is an example of such a list (the default labels):
#' \code{\link{labels_within_package}}.
#'
#' @param data_long a long format data of SHAP values from
#'   \code{\link{shap.prep}}
#' @param x_bound use to set horizontal axis limit in the plot
#' @param dilute being numeric or logical (TRUE/FALSE), it aims to help make the test
#'   plot for large amount of data faster. If dilute = 5 will plot 1/5 of the
#'   data. If dilute = TRUE or a number, will plot at most half points per
#'   feature, so the plotting won't be too slow. If you put dilute too high, at
#'   least 10 points per feature would be kept. If the dataset is too small
#'   after dilution, will just plot all the data
#' @param scientific  show the mean|SHAP| in scientific format. If TRUE, label
#'   format is 0.0E-0, default to FALSE, and the format will be 0.000
#' @param my_format supply your own number format if you really want
#' @param min_color_bound min color hex code for colormap. Color gradient is
#'   scaled between min_color_bound and max_color_bound. Default is "#FFCC33".
#' @param max_color_bound max color hex code for colormap. Color gradient is
#'   scaled between min_color_bound and max_color_bound. Default is "#6600CC".
#' @param kind By default, a "sina" plot is shown. As an alternative,
#'   set \code{kind = "bar"} to visualize mean absolute SHAP values as a
#'   barplot. Its color is controlled by \code{max_color_bound}. Other
#'   arguments are ignored for this kind of plot.
#'
#' @import ggplot2
#' @importFrom ggforce geom_sina
#' @export shap.plot.summary
#' @return returns a ggplot2 object, could add further layers.
#'
#' @example R/example/example_fit_summary.R
#'
shap.plot.summary <- function(data_long,
                              x_bound = NULL,
                              dilute = FALSE,
                              scientific = FALSE,
                              my_format = NULL,
                              min_color_bound = "#FFCC33",
                              max_color_bound = "#6600CC",
                              kind = c("sina", "bar")){

  kind <- match.arg(kind)
  if (kind == "bar") {
    imp <- shap.importance(data_long)
    p <- ggplot(imp, aes(x = variable, y = mean_abs_shap)) +
      geom_bar(stat = "identity", fill = max_color_bound) +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(imp[["variable"]]))) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 10)) +
      labs(x = element_blank(), y = "Avg(|SHAP|)")
    return(p)
  }

  if (scientific){label_format = "%.1e"} else {label_format = "%.3f"}
  if (!is.null(my_format)) label_format <- my_format
  # check number of observations
  N_features <- setDT(data_long)[,uniqueN(variable)]
  if (is.null(dilute)) dilute = FALSE

  nrow_X <- nrow(data_long)/N_features # n per feature
  if (dilute!=0){
    # if nrow_X <= 10, no dilute happens
    dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute)))) # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long),
                                  min(nrow(data_long)/dilute, nrow(data_long)/2))] # dilute
  }

  x_bound <- if (is.null(x_bound)) max(abs(data_long$value))*1.1 else as.numeric(abs(x_bound))
  plot1 <- ggplot(data = data_long) +
    coord_flip(ylim = c(-x_bound, x_bound)) +
    geom_hline(yintercept = 0) + # the y-axis beneath
    # sina plot:
    ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
              method = "counts", maxwidth = 0.7, alpha = 0.7) +
    # print the mean absolute value:
    geom_text(data = unique(data_long[, c("variable", "mean_value")]),
              aes(x = variable, y=-Inf, label = sprintf(label_format, mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2,
              fontface = "bold",
              check_overlap = TRUE) + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) +
    scale_color_gradient(low=min_color_bound, high=max_color_bound,
                         breaks=c(0,1), labels=c(" Low", "High "),
                         guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
    theme_bw() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          axis.title.x= element_text(size = 10)) +
    # reverse the order of features, from high to low
    # also relabel the feature using `label.feature`
    scale_x_discrete(limits = rev(levels(data_long$variable)),
                     labels = label.feature(rev(levels(data_long$variable))))+
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value  ")
  return(plot1)
}

#' A wrapped function to make summary plot from model object and predictors
#'
#' `shap.plot.summary.wrap1` wraps up function \code{\link{shap.prep}} and
#' \code{\link{shap.plot.summary}}
#' @param model the model
#' @param X the dataset of predictors used for calculating SHAP
#' @param top_n how many predictors you want to show in the plot (ranked)
#' @inheritParams shap.plot.summary
#'
#' @export shap.plot.summary.wrap1
#'
#' @example R/example/example_fit_summary.R
#'
shap.plot.summary.wrap1 <- function(model, X, top_n, dilute = FALSE){
  if(missing(top_n)) top_n <- dim(X)[2]
  if(!top_n%in%c(1:dim(X)[2])) stop('supply correct top_n')
  shap_long <- shap.prep(xgb_model = model, X_train = X, top_n = top_n)
  shap.plot.summary(data_long = shap_long, dilute = dilute)
}


#' A wrapped function to make summary plot from given SHAP values matrix
#'
#' `shap.plot.summary.wrap2` wraps up function \code{\link{shap.prep}} and
#' \code{\link{shap.plot.summary}}. Since SHAP matrix could be returned from
#' cross-validation instead of only one model, here the wrapped
#' \code{\link{shap.prep}} takes the SHAP score matrix `shap_score` as input
#'
#' @param shap_score the SHAP values dataset, could be obtained by
#'   \code{shap.prep}
#' @param X the dataset of predictors used for calculating SHAP values
#' @param top_n how many predictors you want to show in the plot (ranked)
#' @inheritParams shap.plot.summary
#'
#' @export shap.plot.summary.wrap2
#'
#' @example R/example/example_fit_summary.R
#'
shap.plot.summary.wrap2 <- function(shap_score, X, top_n, dilute = FALSE){
  if(missing(top_n)) top_n <- dim(X)[2]
  if(!top_n%in%c(1:dim(X)[2])) stop('supply correct top_n')
  shap_long2 <- shap.prep(shap_contrib = shap_score, X_train = X, top_n = top_n)
  shap.plot.summary(shap_long2, dilute = dilute)
}

# SHAP importance ----------------------------------------------------------

#' Variable importance as measured by mean absolute SHAP value.
#'
#' @param data_long a long format data of SHAP values from
#'   \code{\link{shap.prep}}
#' @param names_only If \code{TRUE}, returns variable names only.
#' @param top_n How many variables to be returned?
#'
#' @return returns \code{data.table} with average absolute SHAP
#' values per variable, sorted in decreasing order of importance.
#'
#' @export
#'
#' @examples
#'
#' shap.importance(shap_long_iris)
#'
#' shap.importance(shap_long_iris, names_only = 1)
#'
shap.importance <- function(data_long, names_only = FALSE, top_n = Inf) {
  out <- data_long[, list(mean_abs_shap = mean_value[1]), by = "variable"]
  out <- out[order(mean_abs_shap, decreasing = TRUE)]
  n <- min(nrow(out), top_n)

  if (isTRUE(names_only)) {
    out <- as.character(out[["variable"]])
  }
  out[seq_len(n)]
}

# Dependence plot  --------------------------------------------------------

#' Modify labels for features under plotting
#'
#' `label.feature` helps to modify labels. If a list is created in the global
#' environment named **new_labels** (\code{!is.null(new_labels}), the plots will
#' use that list to replace default list of labels
#' \code{\link{labels_within_package}}.
#'
#' @param x variable names
#'
#' @return a character, e.g. "date", "Time Trend", etc.
#'
label.feature <- function(x){
  # a saved list of some feature names that I am using
  labs <- SHAPforxgboost::labels_within_package
  # but if you supply your own `new_labels`, it will print your feature names
  # must provide a list.
  if (!is.null(new_labels)) {
    if(!is.list(new_labels)) {
      message("new_labels should be a list, for example,`list(var0 = 'VariableA')`.\n")
      }  else {
      message("Plot will use your user-defined labels.\n")
      labs = new_labels
      }
  }
  out <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.null(labs[[ x[i] ]])){
      out[i] <- x[i]
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(out)
}


#' Internal-function to revise axis label for each feature
#'
#' This function further fine-tune the format of each feature
#'
#' @param plot1 ggplot2 object
#' @param show_feature feature to plot
#'
#' @return returns ggplot2 object with further mordified layers based on the feature

plot.label <- function(plot1, show_feature){
  if (show_feature == 'dayint'){
    plot1 <- plot1 +
      scale_x_date(date_breaks = "3 years", date_labels = "%Y")
  } else if (show_feature == 'AOT_Uncertainty' | show_feature == 'DevM_P1km'){
    plot1 <- plot1 +
      scale_x_continuous(labels = function(x)paste0(x*100, "%"))
  } else if (show_feature == 'RelAZ'){
    plot1 <- plot1 +
      scale_x_continuous(breaks = c((0:4)*45), limits = c(0,180))
  }
  plot1
}


#' SHAP dependence plot and interaction plot, optional to be colored by a
#' selected feature
#'
#' This function by default makes a simple dependence plot with feature values
#' on the x-axis and SHAP values on the y-axis, optional to color by another
#' feature. It is optional to use a different variable for SHAP values on the
#' y-axis, and color the points by the feature value of a designated variable.
#' Not colored if \code{color_feature} is not supplied. If \code{data_int} (the
#' SHAP interaction values dataset) is supplied, it will plot the interaction
#' effect between \code{y} and \code{x} on the y-axis. Dependence plot is easy
#' to make if you have the SHAP values dataset from \code{predict.xgb.Booster}
#' or \code{predict.lgb.Booster}.
#' It is not necessary to start with the long format data, but since that is
#' used for the summary plot, we just continue to use it here.
#'
#' @param data_long the long format SHAP values from \code{\link{shap.prep}}
#'
#' @param x which feature to show on x-axis, it will plot the feature value
#' @param y which shap values to show on y-axis, it will plot the SHAP value of
#'   that feature. y is default to x, if y is not provided, just plot the SHAP
#'   values of x on the y-axis
#' @param color_feature which feature value to use for coloring, color by the
#'   feature value. If "auto", will select the feature "c" minimizing the
#'   variance of the shap value given x and c, which can be viewed as a
#'   heuristic for the strongest interaction.
#' @param data_int the 3-dimention SHAP interaction values array. if `data_int`
#'   is supplied, y-axis will plot the interaction values of y (vs. x).
#'   `data_int` is obtained from either \code{predict.xgb.Booster} or
#'   \code{\link{shap.prep.interaction}}
#' @param dilute a number or logical, dafault to TRUE, will plot
#'   \code{nrow(data_long)/dilute} data. For example, if dilute = 5 will plot
#'   20% of the data. As long as dilute != FALSE, will plot at most half the
#'   data
#' @param smooth optional to add a _loess_ smooth line, default to TRUE.
#' @param size0 point size, default to 1 if nobs<1000, 0.4 if nobs>1000
#' @param add_hist whether to add histogram using \code{ggMarginal}, default to
#'   TRUE. But notice the plot after adding histogram is a `ggExtraPlot` object
#'   instead of `ggplot2` so cannot add `geom` to that anymore. Turn the
#'   histogram off if you wish to add more `ggplot2` geoms
#' @param add_stat_cor add correlation and p-value from `ggpubr::stat_cor`
#' @param alpha point transparancy, default to 1 if nobs<1000 else 0.6
#' @param jitter_height amount of vertical jitter (see hight in \code{geom_jitter})
#' @param jitter_width amount of horizontal jitter (see width in \code{geom_jitter}). Use values close to 0, e.g. 0.02
#' @param ... additional parameters passed to \code{geom_jitter}
#'
#' @export shap.plot.dependence
#'
#' @return be default a `ggplot2` object, based on which you could add more geom
#'   layers.
#'
#' @example R/example/example_dependence_plot.R
#'
shap.plot.dependence <- function(
  data_long,
  x,
  y = NULL,
  color_feature = NULL,
  data_int = NULL,  # if supply, will plot SHAP
  dilute = FALSE,
  smooth = TRUE,
  size0 = NULL,
  add_hist = FALSE,
  add_stat_cor = FALSE,
  alpha = NULL,
  jitter_height = 0,
  jitter_width = 0,
  ...
  ){
  if (is.null(y)) y <- x
  data0 <- data_long[variable == y, .(variable, value)] # the shap value to plot for dependence plot
  data0$x_feature <- data_long[variable == x, rfvalue]

  # Note: strongest_interaction can return NULL if there is no color feature available
  # Thus, we keep this condition outside the next condition
  if (!is.null(color_feature) && color_feature == "auto") {
    color_feature <- strongest_interaction(X0 = data0, Xlong = data_long)
  }
  if (!is.null(color_feature)) {
    data0$color_value <- data_long[variable == color_feature, rfvalue]
  }
  if (!is.null(data_int)) data0$int_value <- data_int[, x, y]

  nrow_X <- nrow(data0)
  if (is.null(dilute)) dilute = FALSE
  if (dilute != 0){
    dilute <- ceiling(min(nrow(data0)/10, abs(as.numeric(dilute))))
    # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data0 <- data0[sample(nrow(data0), min(nrow(data0)/dilute, nrow(data0)/2))] # dilute
  }

  # for dayint, reformat date
  if (x == "dayint"){
    data0[, x_feature:= as.Date(data0[, x_feature], format = "%Y-%m-%d",
                                origin = "1970-01-01")]
  }

  if (is.null(size0)) {
    size0 <- if (nrow(data0) < 1000L) 1 else 0.4
  }

  if (is.null(alpha)) {
    alpha <- if (nrow(data0) < 1000L) 1 else 0.6
  }
  plot1 <- ggplot(
    data = data0,
    aes(x = x_feature,
        y = if (is.null(data_int)) value else int_value,
        color = if (!is.null(color_feature)) color_value else NULL)
    ) +
    geom_jitter(
      size = size0,
      width = jitter_width,
      height = jitter_height,
      alpha = alpha,
      ...
    ) +
    labs(y = if (is.null(data_int)) paste0("SHAP value for ", label.feature(y)) else
      paste0("SHAP interaction values for\n", label.feature(x), " and ", label.feature(y)),
         x = label.feature(x),
         color = if (!is.null(color_feature))
           paste0(label.feature(color_feature), "\n","(Feature value)") else NULL) +
    scale_color_gradient(low="#FFCC33", high="#6600CC",
                         guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))

  # a loess smoothing line:
  if (smooth) {
    plot1 <- plot1 +
      geom_smooth(method = "loess", color = "red", size = 0.4, se = FALSE)
  }
  plot1 <- plot.label(plot1, show_feature = x)

  # add correlation
  if (add_stat_cor) {
    plot1 <- plot1 + ggpubr::stat_cor(method = "pearson")
  }

  # add histogram
  if (add_hist) {
    plot1 <- ggExtra::ggMarginal(
      plot1, type = "histogram", bins = 50, size = 10, color = "white"
    )
  }

  plot1
}


# Stack plot --------------------------------------------------------------

#' Prepare data for SHAP force plot (stack plot)
#'
#' Make force plot for \code{top_n} features, optional to randomly plot certain
#' portion of the data in case the dataset is large.
#'
#' @param shap_contrib shap_contrib is the SHAP value data returned from
#'   predict, here an ID variable is added for each observation in
#'   the `shap_contrib` dataset for better tracking, it is created in the
#'   begining as `1:nrow(shap_contrib)`. The ID matches the output from
#'   \code{\link{shap.prep}}
#' @param data_percent what percent of data to plot (to speed up the testing
#'   plot). The accepted input range is (0,1], if observations left is too few,
#'   there will be an error from the clustering function
#' @param top_n integer, optional to show only top_n features, combine the rest
#' @param cluster_method default to ward.D, please refer to `stats::hclust` for
#'   details
#' @param n_groups a integer, how many groups to plot in
#'   \code{\link{shap.plot.force_plot_bygroup}}
#'
#' @export shap.prep.stack.data
#' @return a dataset for stack plot
#'
#' @example R/example/example_force_plot.R
#'
shap.prep.stack.data <- function(
  shap_contrib,
  top_n = NULL,
  data_percent = 1,
  cluster_method = 'ward.D',
  n_groups = 10L
  ){
  ranked_col <- names(colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)])
  shap_contrib2 <- setDT(shap_contrib)[,ranked_col, with = F]
  shap_contrib2[, ID:= .I]
  # sample `data_percent` of the data, making the plot faster
  set.seed(1234)
  if (data_percent>1|data_percent<=0) data_percent <- 1
  shap_contrib2 <- shap_contrib2[sample(.N, .N * data_percent)]
  # setkey(shap_contrib2, ID)

  # select columns into `shapobs`, default to all columns
  if(is.null(top_n)) top_n <- length(ranked_col)
  top_n <- as.integer(top_n)
  if(!top_n%in%c(1:length(ranked_col))) top_n <- length(ranked_col)
  if(top_n < length(ranked_col)){
    top_ranked_col <- ranked_col[1:top_n]
    bottom_col <- ranked_col[-(1:top_n)]
    shap_contrib2[, rest_variables:= rowSums(.SD), .SDcol = bottom_col]
    # dataset with desired variables for plot
    shapobs <- shap_contrib2[, c("ID", top_ranked_col, "rest_variables"), with = F]
    message("The SHAP values of the Rest ", length(ranked_col) - top_n, " features were summed into variable 'rest_variables'.\n")
  } else {
    shapobs <- shap_contrib2
    message("All the features will be used.\n")
  }

  # sort by cluster
  clusters <- hclust(dist(scale(shapobs)), method = cluster_method)
  # re-arrange the rows accroding to dendrogram
  shapobs$group <- cutree(clusters, n_groups)
  shapobs <- shapobs[, clusterid:= clusters$order][rank(clusterid),]
  shapobs[,clusterid:=NULL]
  shapobs[,sorted_id:=.I] # this is the id that sort all the obs
  return(shapobs)
}


#' Make the SHAP force plot
#'
#' The force/stack plot, optional to zoom in at certain x-axis location or zoom
#' in a specific cluster of observations.
#'
#' @param shapobs The dataset obtained by \code{shap.prep.stack.data}.
#' @param id the id variable.
#' @param zoom_in_location where to zoom in, default at place of 60 percent of
#'   the data.
#' @param y_parent_limit  set y-axis limits.
#' @param y_zoomin_limit  \code{c(a,b)} to limit the y-axis in zoom-in.
#' @param zoom_in default to TRUE, zoom in by \code{ggforce::facet_zoom}.
#' @param zoom_in_group optional to zoom in certain cluster.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggforce facet_zoom
#' @export shap.plot.force_plot
#'
#' @example R/example/example_force_plot.R
#'
shap.plot.force_plot <- function(
  shapobs,
  id = 'sorted_id',
  zoom_in_location = NULL,
  y_parent_limit = NULL,
  y_zoomin_limit = NULL,# c(a,b) to limit the y-axis in zoom-in
  zoom_in = TRUE,  # default zoom in to zoom_in_location
  zoom_in_group = NULL # if is set (!NULL), zoom-in to the defined cluster
){
  # optional: location to zoom in certain part of the plot
  shapobs_long <- melt.data.table(shapobs,
       measure.vars = colnames(shapobs)[!colnames(shapobs)%in%c(id, "group", "ID")])
  # display.brewer.pal("Paired")
  p <- ggplot(shapobs_long, aes_string(x = id, y = "value" , fill = "variable")) +
    geom_col(width =1, alpha = 0.9) +
    # geom_area() +
    labs(fill = 'Feature', x = 'Observation',
         y = 'SHAP values by feature:\n (Contribution to the base value)') +
    geom_hline(yintercept = 0, col = "gray40") +
    theme_bw() +
    coord_cartesian(ylim = y_parent_limit)
  # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

  # how to apply color
  if (dim(shapobs)[2]-1<=12){p <- p +
    # [notes] yes this discrete palette has a maximum of 12
    scale_fill_manual(values = RColorBrewer::brewer.pal(dim(shapobs)[2]-1, 'Paired'))
  } else {
    p <- p +  scale_fill_viridis_d(option = "D")  # viridis color scale
  }

  # how to zoom in
  if (!is.null(y_parent_limit)&!is.null(y_zoomin_limit)) {
    warning("Just notice that when parent limit is set, the zoom in axis limit won't work, it seems to be a problem of ggforce.\n")
    }
  if(zoom_in&is.null(zoom_in_group)){
    x_mid <- if (is.null(zoom_in_location)) shapobs[,.N]*0.6 else zoom_in_location
    x_interval <- stats::median(c(50, 150, floor(shapobs[,.N]*0.1)))
    message("Data has N = ", shapobs[,.N]," | zoom in length is ", x_interval, " at location ",x_mid, ".\n")
    p <- p +
      ggforce::facet_zoom(xlim = c(x_mid, x_mid + x_interval),
                 ylim = y_zoomin_limit, horizontal = F,
                 zoom.size = 0.6
      ) +
      theme(zoom.y = element_blank(), validate = FALSE) # zoom in using this line
  } else if (zoom_in){

    message("Data has N = ", shapobs[,.N]," | zoom in at cluster ",zoom_in_group," with N = ",shapobs[group ==zoom_in_group,.N], ".\n")
    p <- p +
      ggforce::facet_zoom(x = group == zoom_in_group,
                 ylim = y_zoomin_limit, horizontal = F,
                 zoom.size = 0.6
      ) +
      theme(zoom.y = element_blank(), validate = FALSE) # zoom in using this line
  }
  else{
    message("Data has N = ", shapobs[,.N]," | no zoom in.\n")
  }
  return(p)
}


#' Make the stack plot, optional to zoom in at certain x or certain cluster
#'
#' A collective display of zoom-in plots: one plot for every group of the
#' clustered observations.
#'
#' @param shapobs The dataset obtained by \code{shap.prep.stack.data}.
#' @param id the id variable.
#' @param y_parent_limit  set y-axis limits.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggforce facet_zoom
#' @export shap.plot.force_plot_bygroup
#'
#' @example R/example/example_force_plot.R
#'
shap.plot.force_plot_bygroup <- function(
  shapobs,
  id = 'sorted_id',
  y_parent_limit = NULL
){
  # optional: location to zoom in certain part of the plot
  shapobs_long <- melt.data.table(shapobs,
    measure.vars = colnames(shapobs)[!colnames(shapobs)%in%c(id, "group", "ID")])
  p <- ggplot(shapobs_long, aes_string(x = id, y = "value" , fill = "variable")) +
    geom_col(width =1, alpha = 0.9) +
    facet_wrap(~ group, scales = "free", ncol = 2) +
    # geom_area() +
    labs(fill = 'Feature', x = 'Observation',
         y = "") +
    geom_hline(yintercept = 0, col = "gray40") +
    theme_bw() +
    # theme(legend.justification = c(1, 1), legend.position = c(1, 1),
    #       legend.text=element_text(size=rel(0.8)))+
    coord_cartesian(ylim = y_parent_limit)
  # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  # how to apply color
  if (dim(shapobs)[2]-1<=12){p <- p +
    # [notes] yes this discrete palette has a maximum of 12
    scale_fill_manual(values = RColorBrewer::brewer.pal(dim(shapobs)[2]-1, 'Paired'))
  } else {
    p <- p +  scale_fill_viridis_d(option = "D")  # viridis color scale
  }
  return(p)
}

# miscellaneous
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".", "rfvalue", "value","variable","stdfvalue",
                           "x_feature", "mean_value",
                           "int_value", "color_value",
                           "new_labels","labels_within_package",
                           "..var_cat",
                           "group", "rest_variables", "clusterid", "ID", "sorted_id", "BIAS",
                           "cond_mean", "mean_abs_shap"))
}
