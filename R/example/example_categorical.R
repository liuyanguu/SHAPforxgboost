# suppressPackageStartupMessages({
# library("SHAPforxgboost")
# library("data.table")
# library("ggplot2")
# })

data("iris")
# set a categorical variable for example
set.seed(123)
iris$Group <- 0
iris[sample(1:nrow(iris), nrow(iris)/2), "Group"] <- 1
X_train = as.matrix(setDT(iris)[,c(colnames(iris)[1:4], "Group"), with = FALSE])
mod1 = xgboost::xgboost(
  data = X_train, label = iris$Species, gamma = 0, eta = 1,
  lambda = 0, nrounds = 1, verbose = FALSE)

shap_long2 <- shap.prep(xgb_model = mod1, X_train = X_train, var_cat = "Group")
# **SHAP summary plot**
shap.plot.summary(shap_long2, scientific = TRUE) +
  facet_wrap(~ Group)

# ggsave("sample_cat.png", width = 8, height = 4)
