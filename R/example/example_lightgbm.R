####
#
# **LightGBM**
mod_lgb = lightgbm::lgb.train(
  params = list(objective = "regression",
                interaction_constraints =
                  list(c("Sepal.Width", "Petal.Length"), "Petal.Width", "Species")
                ),
  data = lightgbm::lgb.Dataset(data.matrix(iris[, -1]),
                               label = iris[[1]]),
  nrounds = 100,
  verbose = -2
)

shap_lgb <- shap.prep(mod_lgb, X_train = data.matrix(iris[, -1]))

shap.plot.summary(shap_lgb)
shap.plot.dependence(shap_lgb, x = "Sepal.Width")

# Thanks to interaction constraints, the strongest (and only)
# modelled interaction effect comes from Sepal.Width and Petal.Length
shap.plot.dependence(shap_lgb, x = "Sepal.Width", color_feature = "auto")
