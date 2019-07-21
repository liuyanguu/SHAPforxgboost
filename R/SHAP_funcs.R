#' Functions for the SHAP visualization
#' Further explained on my research blog: https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
#'


# data preparation functions ----------------------------------------------

#' return from xgboost model the matrix of shap score and ranked variable list
#' @param xgb_model a xgboost model object
#' @param X_train the dataset of predictors used for the xgboost model
#' @export shap.values
#' @example example_fit_summary.R
#'
shap.values <- function(xgb_model, X_train){
  shap_contrib <- predict(xgb_model, as.matrix(X_train),
                          predcontrib = TRUE,
                          approxcontrib = FALSE)
  shap_contrib <- as.data.table(shap_contrib)
  BIAS0 <- shap_contrib[,data.table::first(BIAS)]
  shap_contrib[, BIAS := NULL] # BIAS is an extra column produced by `predict`
  # make SHAP score in decreasing order:
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = mean_shap_score,
              BIAS0 = BIAS0))
}


#' prep SHAP values into long format for plotting
#' @param shap_result is a list contains SHAP values dataset
#'  and a vector of ranked variables, obtained by shap.values if derived from model
#' @param X_train the dataset of predictors used for the xgboost model
#' @export shap.prep
#' @example example_fit_summary.R
#'
shap.prep <- function(shap, X_train, top_n){
  std1 <- function(x){
    return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
  }
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
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


# modelling functions --------------------------------------------------------

#' A wrapped function to run xgboost model
#' @param X predictors, **should NOT contains Y**
#' @param Y dependent variable Y
#' @param xgb_param a list of hyperparameters selected
#' @example example_fit_summary.R
#' @export xgboost.fit
xgboost.fit <- function(X, Y, xgb_param, verbose = FALSE,...){
  xgb_threads  <-  (
    if (Sys.info()["nodename"] == "belle") 14 # 32 cores
    else if (Sys.info()["nodename"] == "coco") 22 # 48 cores
    else parallel::detectCores()) - 2
  # print(unlist(xgb_param))
  # be careful X should not contains Y.
  if (!is.null(xgb_param$seed)) set.seed(xgb_param$seed) else set.seed(1234)
  xgbmod <- xgboost::xgboost(data = as.matrix(X), label = as.matrix(Y),
                    params = xgb_param, nrounds = xgb_param$nrounds,
                    verbose = verbose, print_every_n = xgb_param$nrounds/10,
                    nthread = xgb_threads,
                    early_stopping_rounds = 8)
  return(xgbmod)
}




# summary plot ----------------------------------------------------------

#' SHAP summary plot
#' The summary plot (sina plot) using a long-format data of SHAP values.
#' @param data_long: a long format data of SHAP values which looks like:
#'     variable value rfvalue stdfvalue mean_value
#' 1:   dayint -0.23   11012   0.00000       0.14
#' 2:   dayint -0.20   11012   0.00000       0.14
#' @param dilute by default print all data, but that would be slow
#' @param x_bound_given in case need to limit x_axis_limit
#' @param dilute dafault to F, if dilute, will plot at most 1,000 points per feature.
#' @param scientific default to F, "%.1e", show the mean|SHAP| in scientific or not
#' if nobs > 10,000. if dilute, it will use at most 1000 observations
#' @example example_fit_summary.R
#' @export plot.shap.summary
plot.shap.summary <- function(data_long, x_bound_given = NULL,
                              dilute = F, scientific = F){

  if (scientific){label_format = "%.1e"} else {label_format = "%.3f"}
  if (packageVersion("ggforce")<"0.2.1.9000"){message("To make sina plot work make sure ggforce is updated")}
  N_features <- setDT(data_long)[,uniqueN(variable)]
  N_obs <- nrow(data_long)/N_features
  if (dilute){
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long), min(nrow(data_long), N_features*1000))] # dilute
  }
  # mean_value cannot be 0 in the new version of ggforce
  data_long <- data_long[mean_value!=0, ]
  if (is.null(x_bound_given)){
    x_bound <- max(abs(data_long$value))} else {
      x_bound <- x_bound_given # given bound of SHAP values
    }
  plot1 <- ggplot(data = data_long)+
    coord_flip() +
    # sina plot:
    # geom_point(aes(x = variable, y = value, color = stdfvalue)) +
    geom_sina(aes(x = variable, y = value, color = stdfvalue),
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
                         breaks=c(0,1), labels=c("  Low","High   "),
                         guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
    theme_bw() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          axis.title.x= element_text(size = 10)) +
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)))+
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value  ")
  return(plot1)
}

#' A wraped function to make summary plot from xgb model and X
#' @param model the xgboost model
#' @param X the dataset of predictors used for the xgboost model
#' @param top_n how many predictors you want to show in the plot (ranked)
#' @example example_fit_summary.R
#' @export plot.shap.summary.wrap1
plot.shap.summary.wrap1 <- function(model, X, top_n){
  if(missing(top_n)) top_n <- dim(X)[2]
  if(!top_n%in%c(1:dim(X)[2])) stop('supply correct top_n')

  shap_long <- shap.prep(shap = shap.values(model, X),
                         X_train = X, top_n = top_n)
  # make summary plot
  plot.shap.summary(data_long = shap_long)
}

#' summary plot using given SHAP values matrix
#' supply a self-made SHAP values dataset
#' (e.g. sometimes as output from cross-validation)
#' @param model the xgboost model
#' @param X the dataset of predictors used for the xgboost model
#' @param top_n how many predictors you want to show in the plot (ranked)
#' @example example_fit_summary.R
#' @export plot.shap.summary.wrap2
#'
plot.shap.summary.wrap2 <- function(shap_score, X, top_n){
  if(missing(top_n)) top_n <- dim(X)[2]
  if(!top_n%in%c(1:dim(X)[2])) stop('supply correct top_n')
  shap_result <- list(shap_score = shap_score,
                      mean_shap_score =
                        colMeans(abs(shap_score))[order(colMeans(abs(shap_score)),
                                                        decreasing = T)])
  shap_long2 <- shap.prep(shap = shap_result, X_train = X, top_n = top_n)
  plot.shap.summary(shap_long2)
}


# dependence plot  --------------------------------------------------------

#' helper function to modify variables names
#' @param x feature names
#'
label.feature <- function(x){
  labs = list(diffcwv = "Diff CWV (cm)",
    dayint = "Time trend",
    date = "",
    Column_WV = "MAIAC CWV (cm)",
    AOT_Uncertainty = "Blue band uncertainty",
    elev = "Elevation (m)",
    aod = "Aerosol optical depth",
    RelAZ = "Relative azimuth angle",
    DevAll_P1km = expression(paste("Proportion developed area in 1",km^2)),
    # newly added
    dist_water_km = "Distance to water (km)",
    forestProp_1km = expression(paste("Proportion of forest in 1",km^2)),

    # `diff440 = Aer_optical_depth (DSCOVR MAIAC) -  aer_aod440 (AERONET)`
    Aer_optical_depth = "DSCOVR EPIC MAIAC AOD400nm",
    aer_aod440 = "AERONET AOD440nm",
    aer_aod500 = "AERONET AOD500nm",
    diff440 = "DSCOVR MAIAC - AERONET AOD",
    diff440_pred = "Predicted Error",
    aer_aod440_hat = "Predicted AERONET AOD440nm",

    # New MCD19
    AOD_470nm = "AERONET AOD470nm",
    Optical_Depth_047_t = "MAIAC AOD470nm (Terra)",
    Optical_Depth_047_a = "MAIAC AOD470nm (Aqua)"

  )
  if (is.null(labs[[x]])){
    return(x)
  }else{
    return(labs[[x]])
  }
}

# the basic SHAP core, accept data with only 'show_feature' as variable
# the core function to plot SHAP for one feature
#
plot.shap.core <- function(show_feature, data0){

  plot1 <- ggplot(data = data0, aes(x = rfvalue, y = value))+
    geom_point(size = 0.1, color = "blue2", alpha = 0.3)+
    geom_smooth(method = 'loess', color = 'red', size = 0.4, se = F) +
    theme_bw() +
    labs(y = "", x = label.feature(show_feature))
  return(plot1)
}

# sub-function to revise label for each feature:
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

#' SHAP dependence plot with marginal histogram
#' This version is not colored by another variable, see `plot.shap.dependence.color`
#' @param show_feature which feature to show
#' @param data_long the long format SHAP values
#' @param dilute optional T/F to sample data
#' @param by100 T/F dilute by 100 or 10
#' @example example_dependence_force_plot.R
#' @export plot.shap.dependence
plot.shap.dependence <- function(show_feature, data_long,
                               dilute = F, customize_label = F,
                               by100 = F){
  setkey(data_long, variable)
  data0 <- data_long[show_feature]
  if (dilute){
    set.seed(1234)
    data0 <- data0[sample(nrow(data0), nrow(data0)/(if(by100)100 else 10))] # dilute
  }
  # for dayint, reformat date
  if (show_feature == 'dayint'){
    data0[, rfvalue:= as.Date(rfvalue, format = "%Y-%m-%d", origin = "1970-01-01")]
  }
  # make core plot
  plot1 <- plot.shap.core(show_feature, data0 = data0)
  # customize labels (for cwv, put dayint on 3-year interval)
  if (customize_label){
    plot1 <- plot.label(plot1, show_feature = show_feature)
  }
  # add histogram
  plot2 <- ggMarginal(plot1, type = "histogram", bins = 50, size = 10, color="white")
  plot2
}
# plot.shap.dependence('dayint', shap_long2)



#' SHAP dependence plot with marginal histogram, colored by a selcted feature value
#' @param data_long the long format SHAP values
#' @param x which feature to show on x axis
#' @param y_shap which shap values to show on y axis
#' @param color_feature which feature value to use for coloring
#' @example example_dependence_force_plot.R
#' @export plot.shap.dependence.color
plot.shap.dependence.color <- function(data_long , x,  y_shap, color_feature){
  data0 <- data_long[variable == y_shap] # contains the shap value to plot:"value"
  data0$x_feature <- data_long[variable == x, rfvalue]
  data0$color_value <- data_long[variable == color_feature, rfvalue]
  # for dayint, reformat date
  if (x == 'dayint'){
    data0[, x_feature:= as.Date(data0[,x_feature], format = "%Y-%m-%d",
                                origin = "1970-01-01")]
  }
  plot1 <- ggplot(data = data0,
                  aes(x = x_feature, y = value, color = color_value))+
    geom_point(size = 0.2, alpha = 0.6)+
    # a loess smoothing line:
    geom_smooth(method = 'loess', color = 'red', size = 0.4, se = F) +
    labs(y = paste0("SHAP value for ", label.feature(y_shap)),
         x = label.feature(x),
         color = paste0(label.feature(color_feature),"\n","(Feature value)")) +
    scale_color_gradient(low="#FFCC33", high="#6600CC",
                         guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
    theme_bw() +
    theme(legend.position="bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8))
  plot1 <- plot.label(plot1, show_feature = x)
  plot1
}
# plot.shap.dependence.color(data_long = shap_long2, x = 'dayint',y_shap = 'dayint', color_feature = 'Column_WV')


# stack plot --------------------------------------------------------------

#' SHAPstack plot
#' shap_contrib is the SHAP value matrix returned from predict, top_n features, option to randomly plot p*100 percent of the data in case the dataset is large
#' @param shap_contrib the SHAP values dataset
#' @param data_percent what percent of data to plot (to speed up), in the range of (0,1]
#' @param top_n optional to show only top_n features, combine the rest
#' @example example_dependence_force_plot.R
#' @export shap.stack.data
shap.stack.data <- function(shap_contrib, top_n = NULL,
                            data_percent = 1,
                            cluster_method = 'ward.D',
                            n_groups = 10){
  ranked_col <- names(colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)])
  shap_contrib2 <- setDT(shap_contrib)[,..ranked_col]

  # sample `data_percent` of the data, making the plot faster
  set.seed(1234)
  if (data_percent>1|data_percent<=0) data_percent <- 1
  shap_contrib2 <- shap_contrib2[sample(.N, .N * data_percent)]

  # select columns into `shapobs`
  if(is.null(top_n))top_n <- length(ranked_col) # by default, show all variables
  if (top_n < length(ranked_col)){
    top_ranked_col <- ranked_col[1:top_n]
    bottom_col <- ranked_col[-(1:top_n)]
    shap_contrib2[, rest_variables:= rowSums(.SD), .SDcol = bottom_col]
    # dataset with desired variables for plot
    shapobs <- shap_contrib2[, c(top_ranked_col, "rest_variables"), with = F]
    cat(paste("The SHAP values of the Rest", length(ranked_col) - top_n, "variables were summed into variable 'rest_variables'.\n"))
  } else if (top_n == length(ranked_col)) {
    shapobs <- shap_contrib2
  } else {stop(paste("top_n should not above", length(ranked_col)))}


  # sort by cluster
  clusters <- hclust(dist(scale(shapobs)), method = cluster_method)
  # re-arrange the rows accroding to dendrogram
  shapobs$group <- cutree(clusters, n_groups)
  shapobs <- shapobs[, clusterid:= clusters$order][rank(clusterid),]
  shapobs[,clusterid:=NULL]
  shapobs[,id:=.I]
  return(shapobs)
}

#' make the stack plot, optional to zoom in at certain x or certain cluster
#' @example example_dependence_force_plot.R
#' @export plot.shap.force_plot
plot.shap.force_plot <- function(shapobs, id = 'id',
                            zoom_in_location = NULL, # where to zoom in, default at place of 60% of the data
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
    scale_fill_manual(values = brewer.pal(dim(shapobs)[2]-1, 'Paired'))
  } else {
    p <- p +  scale_fill_viridis_d(option = "D")  # viridis color scale
  }

  # how to zoom in
  if (!is.null(y_parent_limit)&!is.null(y_zoomin_limit)) cat("Just notice that when parent limit is set, the zoom in axis limit won't work, it seems to be a problem of ggforce.\n")
  if(zoom_in&is.null(zoom_in_group)){
    x_mid <- if (is.null(zoom_in_location)) shapobs[,.N]*0.6 else zoom_in_location
    x_interval <- median(c(50, 150, floor(shapobs[,.N]*0.1)))
    cat("Data has N:", shapobs[,.N],"| zoom in length is", x_interval, "at location",x_mid, "\n")
    p <- p +
      facet_zoom(xlim = c(x_mid, x_mid + x_interval),
                 ylim = y_zoomin_limit, horizontal = F,
                 zoom.size = 0.6
      ) +
      theme(zoom.y = element_blank(), validate = FALSE) # zoom in using this line
  } else if (zoom_in){
    cat("Data has N:", shapobs[,.N],"| zoom in at cluster",zoom_in_group,"with N:",shapobs[group ==zoom_in_group,.N], "\n")
    p <- p +
      facet_zoom(x = group == zoom_in_group,
                 ylim = y_zoomin_limit, horizontal = F,
                 zoom.size = 0.6
      ) +
      theme(zoom.y = element_blank(), validate = FALSE) # zoom in using this line
  }
  else{
    cat("Data has N:", shapobs[,.N],"| no zoom in\n")
  }
  return(p)
}


#' make the stack plot, optional to zoom in at certain x or certain cluster
#' zoomed in plot: one plot of each cluster
#' @example example_dependence_force_plot.R
#' @export plot.shap.force_plot_bygroup
plot.shap.force_plot_bygroup <- function(shapobs, id = 'id',
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
    scale_fill_manual(values = brewer.pal(dim(shapobs)[2]-1, 'Paired'))
  } else {
    p <- p +  scale_fill_viridis_d(option = "D")  # viridis color scale
  }
  return(p)
}

