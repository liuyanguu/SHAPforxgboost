# Example: Using var_cat to group by a categorical variable
# The var_cat argument creates grouped long-format data for faceted plots

library("data.table")
data("iris")
set.seed(123)
iris$Group <- 0
iris[sample(1:nrow(iris), nrow(iris)/2), "Group"] <- 1

data.table::setDT(iris)
X_train = as.matrix(iris[,c(colnames(iris)[1:4], "Group"), with = FALSE])
y_train = as.numeric(iris$Species) - 1  # Convert factor to numeric
dtrain = xgboost::xgb.DMatrix(data = X_train, label = y_train)
params = list(learning_rate = 1, min_split_loss = 0, reg_lambda = 0,
              objective = 'reg:squarederror', nthread = 1)
mod1 = xgboost::xgb.train(params = params, data = dtrain,
                          nrounds = 1, verbose = 0)

# Use var_cat to create faceted plots by Group
shap_long2 <- shap.prep(xgb_model = mod1, X_train = X_train, var_cat = "Group")
shap.plot.summary(shap_long2, scientific = TRUE) +
  ggplot2::facet_wrap(~ Group)
