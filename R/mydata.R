# These are the datasets loaded so all the example could run directly

#' labels_within_package: Some labels package auther defined to make his plot, mainly serve the paper publication.
#'
#' It contains a list that match each feature to its labels. It is used in the function \code{\link{label.feature}}.
#'
#'
#' @docType data
#' @references \url{http://doi.org/10.5281/zenodo.3334713}
#' @keywords Labels
#' @details labels_within_package <- list(
#'    dayint = "Time trend",
#'    diffcwv = "delta CWV (cm)",
#'    date = "",
#'    Column_WV = "MAIAC CWV (cm)",
#'    AOT_Uncertainty = "Blue band uncertainty",
#'    elev = "Elevation (m)",
#'    aod = "Aerosol optical depth",
#'    RelAZ = "Relative azimuth angle",
#'    DevAll_P1km = expression(paste("Proportion developed area in 1",km^2)),
#'    dist_water_km = "Distance to water (km)",
#'    forestProp_1km = expression(paste("Proportion of forest in 1",km^2)),
#'    Aer_optical_depth = "DSCOVR EPIC MAIAC AOD400nm",
#'    aer_aod440 = "AERONET AOD440nm",
#'    aer_aod500 = "AERONET AOD500nm",
#'    diff440 = "DSCOVR MAIAC - AERONET AOD",
#'    diff440_pred = "Predicted Error",
#'    aer_aod440_hat = "Predicted AERONET AOD440nm",
#'    AOD_470nm = "AERONET AOD470nm",
#'    Optical_Depth_047_t = "MAIAC AOD470nm (Terra)",
#'    Optical_Depth_047_a = "MAIAC AOD470nm (Aqua)"
#'    )
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
