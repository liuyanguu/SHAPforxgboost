# These are the datasets loaded so all the example could run directly

#' labels_within_package: Some labels package auther defined to make his plot, mainly serve the paper publication.
#'
#' It contains a list that match each feature to its labels. It is used in the function \code{\link{plot.label}}.
#'
#'
#' @docType data
#' @references \url{http://doi.org/10.5281/zenodo.3334713}
#' @keywords Labels
"labels_within_package"


#'new_labels: a place holder default to NULL.
#'
#'if supplied as a list, it offers user to rename labels
#'
#' @docType data
#' @keywords Labels
"new_labels"


#' Terra satellite data (X,Y) for running the xgboost model .
#'
#' Data.table, contains 9 features, and about 10,000 observations
#'
#' @docType data
#' @references \url{http://doi.org/10.5281/zenodo.3334713}
#' @keywords Terra
"dataXY_df"


#' SHAP values example from dataXY_df .
#'
#' @docType data
#' @references \url{http://doi.org/10.5281/zenodo.3334713}
#' @keywords Terra
"shap_score"


#' SHAP values example using iris dataset.
#' @docType data
#' @keywords iris
"shap_values_iris"


#' The long-format SHAP values example using iris dataset.
#'
#' @docType data
#' @keywords iris
"shap_long_iris"


#' The interaction effect SHAP values example using iris dataset.
#'
#' @docType data
#' @keywords iris
"shap_int_iris"
