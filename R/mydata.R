# These are the datasets loaded so all the example could run directly


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
