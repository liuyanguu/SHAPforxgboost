# SHAP visualization functions for XGBoost,
# wrapped up functions for
# summary plot, dependence plot, force plot, and interaction effect plot
# Further explained on my research blog: https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
# Please cite http://doi.org/10.5281/zenodo.3334713
#


if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".", "rfvalue", "value","variable","stdfvalue",
                           "x_feature", "mean_value",
                           "int_value", "color_value",
                           "new_labels","labels_within_package",
                           "group", "rest_variables", "clusterid", "id", "BIAS"))
  }


# data preparation functions ----------------------------------------------
#' return SHAP contribution from xgboost model
#'
#' \code{shap.values} returns from xgboost model a list of 1.the matrix of shap score and
#' 2. the ranked variable vector by each variable's mean absolute SHAP value
#'
#' @param xgb_model a xgboost model object
#' @param X_train the dataset of predictors used for the xgboost model
#'
#' @import data.table
#' @import xgboost
#' @importFrom stats cutree dist hclust predict lm na.omit
#'
#' @export shap.values
#'
#' @return a list of three elements, the SHAP values as data.table, ranked mean|SHAP|, BIAS
#'
#' @example R/example/example_fit_summary.R
#'
shap.values <- function(xgb_model,
                        X_train){
  shap_contrib <- predict(
                          xgb_model,
                          as.matrix(X_train),
                          predcontrib = TRUE,
                          approxcontrib = FALSE)
  shap_contrib <- as.data.table(shap_contrib)
  BIAS0 <- shap_contrib[,ncol(shap_contrib), with = FALSE][1]
  shap_contrib[, BIAS := NULL] # BIAS is an extra column produced by `predict`
  # make SHAP score in decreasing order:
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = mean_shap_score,
              BIAS0 = BIAS0))
}



#' prep SHAP values into long format for plotting
#'
#' @param xgb_model a xgboost model object
#' @param shap_contrib optional to supply SHAP values dataset, default to NULL
#' @param X_train the dataset of predictors used for the xgboost model
#' if not NULL, will be taken as SHAP values,
#' @param top_n to choose top_n variables ranked by mean|SHAP| if needed
#'
#' @import data.table
#' @export shap.prep
#'
#' @return a long-format data.table, named as `shap_long` in other functions
#'
#' @example R/example/example_fit_summary.R
#'
shap.prep <- function(xgb_model = NULL,
                      shap_contrib = NULL, # optional to supply SHAP values
                      X_train,
                      top_n = NULL){
  if (!is.null(shap_contrib)){
    if(paste0(dim(shap_contrib), collapse = " ") != paste0(dim(X_train), collapse = " ")) stop("supply correct shap_contrib, remove BIAS column.\n")
  }
  # prep long-data
  shap <- if (is.null(shap_contrib)) shap.values(xgb_model, X_train) else list(
    shap_score = shap_contrib,
    mean_shap_score = colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  )

  std1 <- function(x){
    return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
  }

  # choose top n features
  if (is.null(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  top_n <- as.integer(top_n)
  if (!top_n%in%c(1:dim(X_train)[2])) {
    message ('Please supply correct top_n, by default use all features.\n')
    top_n <- dim(X_train)[2]
  }

  # descending order
  shap_score_sub <- setDT(shap$shap_score)[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))

  # feature values: the values in the original dataset
  # dayint is int, will throw a warning here
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))

  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue;
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2)
}


#' prepare the interaction SHAP values from predict.xgb.Booster
#'
#' This function just runs \code{shap_int <- predict(xgb_mod, as.matrix(X_train), predinteraction = TRUE)}, may not be necessary,
#' maybe just use xgboost::predict.xgb.Booster directly,
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
  shap_int <- predict(xgb_model, as.matrix(X_train), predinteraction = TRUE)
  return(shap_int)
}

# SHAP summary plot ----------------------------------------------------------

#' SHAP summary plot core function using the long-format SHAP values
#'
#' The summary plot (sina plot) uses a long-format data of SHAP values. The long-format data
#' could be obtained from either xgboost model or a SHAP matrix using \code{\link{shap.values}}.
#' If you want to start with xgbmodel and data_X, use \code{\link{shap.plot.summary.wrap1}}.
#' If you want to use self-derived SHAP matrix, use \code{\link{shap.plot.summary.wrap2}}.
#' If a global list named **new_labels** is provided (\code{!is.null(new_labels}),
#' the plots will use that list to replace default labels \code{\link{labels_within_package}}.
#' @param data_long a long format data of SHAP values from \code{\link{shap.prep}}
#' @param x_bound in case need to limit x_axis_limit
#' @param dilute a number or logical, dafault to TRUE, will plot \code{nrow(data_long)/dilute} data.
#' for example, if dilute = 5 will plot 1/5 of the data.
#' If dilute = TRUE or a number, we will plot at most half points per feature, so the plot won't be too slow.
#' If you put dilute too high, at least 10 points per feature would be kept.
#' If the dataset is even smaller than that, will just plot all the data.
#' @param scientific  show the mean|SHAP| in scientific format or not
#' default to F, label format is 0.000,
#' If true, label format is 0.0E-0,
#' @param my_format supply your own number format if you really want to do so
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
                              dilute = FALSE, scientific = FALSE,
                              my_format = NULL){

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
    geom_hline(yintercept = 0) + # the y axis beneath
    # sina plot:
    ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
              method = "counts", maxwidth = 0.7, alpha = 0.7) +
    # print the mean absolute value:
    geom_text(data = unique(data_long[, c("variable", "mean_value")]),
              aes(x = variable, y=-Inf, label = sprintf(label_format, mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2,
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) +
    scale_color_gradient(low="#FFCC33", high="#6600CC",
                         breaks=c(0,1), labels=c(" Low","High "),
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

#' A wrapped function to make summary plot from xgb model object and predictors
#'
#' wraps up function \code{\link{shap.prep}} and \code{\link{shap.plot.summary}}
#' If a global list named **new_labels** is provided (\code{!is.null(new_labels}),
#' the plots will use that list to replace default labels \code{\link{labels_within_package}}.
#' @param model the xgboost model
#' @param X the dataset of predictors used for the xgboost model
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
#' Sometimes the SHAP matrix is returned from cross-validation.
#' This function wraps up function \code{\link{shap.prep}} and \code{\link{shap.plot.summary}}.
#'
#' If a global list named **new_labels** is provided (\code{!is.null(new_labels}),
#' the plots will use that list to replace default labels \code{\link{labels_within_package}}.
#'
#' @param shap_score the SHAP values dataset, could be obtained by \code{shap.prep}.
#' @param X the dataset of predictors used for the xgboost model
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


# dependence plot  --------------------------------------------------------

#' helper function to modify labels for features under plotting
#'
#' If a list is created in the environment named **new_labels** (\code{!is.null(new_labels}),
#' the plots will use that list to replace default list of labels \code{\link{labels_within_package}}.
#'
#' @param x variable names
#'
#' @return a character, e.g. "date", "Time Trend", etc.
#'
label.feature <- function(x){
  labs = labels_within_package # a saved list of some feature names that I am using
  # but if you supply your own `new_labels`, it will print your feature names
  # must provide a list.
  if (!is.null(new_labels)) {
    if(!is.list(new_labels)) {
      message("new_labels should be a list, for example,`list(var0 = 'VariableA')`.\n")
      }  else {
      message("Plot will use user-defined labels.\n")
      labs = new_labels}
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


#' internal-function to revise axis label for each feature
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
#' This function makes the simple dependence plot with SHAP values on the y axis, optional to add color
#' by another feature, optional to use a different y variable for SHAP values
#' Not colored if \code{color_feature} is not supplied.
#' If \code{data_int} (the SHAP interaction values dataset) is supplied, it will plot
#' the interaction effect between \code{y} and \code{x} on the y axis.
#'
#' Dependence plot is very easy to make if you have the SHAP values dataset from \code{predict.xgb.Booster}
#' It is not necessary to start with the long-format data, but since I used that
#' for the summary plot, I just continue to use the long dataset
#' @param data_long the long format SHAP values from \code{\link{shap.prep}}
#' @param data_int the 3-dimention SHAP interaction values array.
#' if `data_int` is supplied, y axis will plot the interaction values of y (vs. x)
#' @param dilute a number or logical, dafault to TRUE,
#' will plot \code{nrow(data_long)/dilute} data.
#' For example, if dilute = 5 will plot 20% of the data.
#' As long as dilute != FALSE, will plot at most half the data.
#'
#' from \code{predict.xgb.Booster} or \code{\link{shap.prep.interaction}}.
#' @param x which feature to show on x axis, it will plot the feature value.
#' @param y which shap values to show on y axis, it will plot the SHAP value of that feature.
#' y is default to x, if y is not provided, just plot the SHAP values of x on the y axis
#' @param color_feature which feature value to use for coloring, color by the feature value.
#' @param smooth optional to add _loess_ smooth line, default to TRUE.
#' @param size0 point size, default to 1 of nobs<1000, 0.4 if nobs>1000.
#' @param add_hist whether to add histogram using \code{ggMarginal}, default to TRUE.
#' But notice the plot after adding histogram it is \code{ggExtraPlot} object, cannot
#' add geom to that anymore. If wish to add more ggplot layers, turn the histogram off
#'
#' @export shap.plot.dependence
#'
#' @return returns a ggplot2 object, based on which you could add more geom layers.
#'
#' @example R/example/example_dependence_plot.R
#'
shap.plot.dependence <- function(data_long,
                                       x,
                                       y = NULL,
                                       color_feature = NULL,
                                       data_int = NULL,  # if supply, will plot SHAP interaction values
                                       dilute = FALSE,
                                       smooth = TRUE,
                                       size0 = NULL,
                                       add_hist = FALSE
                                       )
  {
  if (is.null(y)) y <- x
  data0 <- data_long[variable == y,.(variable, value)] # the shap value to plot for dependence plot
  data0$x_feature <- data_long[variable == x, rfvalue]
  if (!is.null(color_feature)) data0$color_value <- data_long[variable == color_feature, rfvalue]
  if (!is.null(data_int)) data0$int_value <- data_int[, x, y]

  nrow_X <- nrow(data0)
  if (is.null(dilute)) dilute = FALSE
  if (dilute!=0){
    dilute <- ceiling(min(nrow(data0)/10, abs(as.numeric(dilute))))
    # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data0 <- data0[sample(nrow(data0), min(nrow(data0)/dilute, nrow(data0)/2))] # dilute
  }

  # for dayint, reformat date
  if (x == 'dayint'){
    data0[, x_feature:= as.Date(data0[,x_feature], format = "%Y-%m-%d",
                                origin = "1970-01-01")]
  }
  if (is.null(size0)) size0 <- if(nrow(data0)<1000L) 1 else 0.4
  plot1 <- ggplot(data = data0,
                  aes(x = x_feature,
                      y = if (is.null(data_int)) value else int_value,
                      color = if (!is.null(color_feature)) color_value else NULL))+
    geom_point(size = size0, alpha = if(nrow(data0)<1000L) 1 else 0.6)+
    labs(y = if (is.null(data_int)) paste0("SHAP value for ", label.feature(y)) else
      paste0("SHAP interaction values for\n", label.feature(x), " and ", label.feature(y)) ,
         x = label.feature(x),
         color = if (!is.null(color_feature))
           paste0(label.feature(color_feature), "\n","(Feature value)") else NULL) +
    scale_color_gradient(low="#FFCC33", high="#6600CC",
                         guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
    theme_bw() +
    theme(legend.position="bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8))
    # a loess smoothing line:
  if(smooth){
    plot1 <- plot1 + geom_smooth(method = 'loess', color = 'red', size = 0.4, se = F)
  }
  plot1 <- plot.label(plot1, show_feature = x)
  if(add_hist){
    plot1 <- ggExtra::ggMarginal(plot1, type = "histogram", bins = 50, size = 10, color="white")
  }
  plot1
}


# stack plot --------------------------------------------------------------

#' Prepare data for SHAP force plot (stack plot)
#'
#' Make force plot for \code{top_n} features, option to randomly
#' plot certain portion of the data in case the dataset is large.
#'
#' @param shap_contrib shap_contrib is the SHAP value data returned from predict.xgb.booster
#' @param data_percent what percent of data to plot (to speed up), in the range of (0,1]
#' @param top_n integer, optional to show only top_n features, combine the rest
#' @param cluster_method default to ward.D
#' @param n_groups a integer, how many groups to plot in \code{\link{shap.plot.force_plot_bygroup}}
#'
#' @export shap.prep.stack.data
#' @return a dataset for stack plot
#'
#' @example R/example/example_force_plot.R
#'
shap.prep.stack.data <- function(shap_contrib,
                            top_n = NULL,
                            data_percent = 1,
                            cluster_method = 'ward.D',
                            n_groups = 10L){
  ranked_col <- names(colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)])
  shap_contrib2 <- setDT(shap_contrib)[,ranked_col, with = F]

  # sample `data_percent` of the data, making the plot faster
  set.seed(1234)
  if (data_percent>1|data_percent<=0) data_percent <- 1
  shap_contrib2 <- shap_contrib2[sample(.N, .N * data_percent)]

  # select columns into `shapobs`, default to all columns
  if(is.null(top_n)) top_n <- length(ranked_col)
  top_n <- as.integer(top_n)
  if(!top_n%in%c(1:length(ranked_col))) top_n <- length(ranked_col)
  if(top_n < length(ranked_col)){
    top_ranked_col <- ranked_col[1:top_n]
    bottom_col <- ranked_col[-(1:top_n)]
    shap_contrib2[, rest_variables:= rowSums(.SD), .SDcol = bottom_col]
    # dataset with desired variables for plot
    shapobs <- shap_contrib2[, c(top_ranked_col, "rest_variables"), with = F]
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
  shapobs[,id:=.I]
  return(shapobs)
}


#' make the SHAP force plot
#'
#' The force/stack plot, optional to zoom in at certain x or certain cluster.
#'
#'
#' @param shapobs The dataset obtained by \code{shap.prep.stack.data}.
#' @param id the id variable.
#' @param zoom_in_location where to zoom in, default at place of 60 percent of the data.
#' @param y_parent_limit  set y axis limits.
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
shap.plot.force_plot <- function(shapobs, id = 'id',
                            zoom_in_location = NULL,
                            y_parent_limit = NULL,
                            y_zoomin_limit = NULL,   # c(a,b) to limit the y-axis in zoom-in
                            zoom_in = TRUE,  # default zoom in to zoom_in_location
                            zoom_in_group = NULL # if is set (!NULL), zoom-in to the defined cluster
){
  # optional: location to zoom in certain part of the plot
  shapobs_long <- melt.data.table(shapobs, id.vars = c(id,"group"))
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


#' make the stack plot, optional to zoom in at certain x or certain cluster
#'
#' A collective display of zoom in plot: one plot of every group of clustered observations.
#'
#' @param shapobs The dataset obtained by \code{shap.prep.stack.data}.
#' @param id the id variable.
#' @param y_parent_limit  set y axis limits.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggforce facet_zoom
#' @export shap.plot.force_plot_bygroup
#'
#' @example R/example/example_force_plot.R
#'
shap.plot.force_plot_bygroup <- function(shapobs, id = 'id',
                                     y_parent_limit = NULL
){
  # optional: location to zoom in certain part of the plot
  shapobs_long <- melt.data.table(shapobs, id.vars = c(id,"group"))
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

