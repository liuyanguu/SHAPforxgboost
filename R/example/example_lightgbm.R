####
#
# **LightGBM**
mod_lgb = lightgbm::lgb.train(
  params = list(objective = "regression"),
  data = lightgbm::lgb.Dataset(data.matrix(iris[, -1]),
                               label = iris[[1]]),
  nrounds = 10,
  verbose = -2
)

shap_lgb <- shap.prep(mod_lgb, X_train = data.matrix(iris[, -1]))

shap.plot.summary(shap_lgb)
shap.plot.dependence(shap_lgb, x = "Sepal.Width", smooth = FALSE)
