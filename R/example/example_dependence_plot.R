# Example: SHAP dependence plots

# 1. Simple dependence plot: SHAP values vs feature values
shap.plot.dependence(data_long = shap_long_iris, x="Petal.Length",
                     add_hist = TRUE, add_stat_cor = TRUE)

# 2. Show different SHAP values on y-axis
shap.plot.dependence(data_long = shap_long_iris, x="Petal.Length",
                           y = "Petal.Width")

# 3. Color by another feature's values
shap.plot.dependence(data_long = shap_long_iris, x="Petal.Length",
                           color_feature = "Petal.Width")

# 4. Customize x, y, and color features
shap.plot.dependence(data_long = shap_long_iris, x="Petal.Length",
                           y = "Petal.Width", color_feature = "Petal.Width")

# 5. Additional options: histogram, smooth line, data dilution
shap.plot.dependence(data_long = shap_long_iris, x="Petal.Length",
                     y = "Petal.Width", color_feature = "Petal.Width",
                     add_hist = TRUE, smooth = FALSE, dilute = 3)

# Create multiple plots at once
plot_list <- lapply(names(iris)[2:3], shap.plot.dependence, data_long = shap_long_iris)

# SHAP interaction effect plot
# First, prepare the model and interaction data
X_iris = as.matrix(iris[,1:4])
y_iris = as.numeric(iris[[5]]) - 1
dtrain = xgboost::xgb.DMatrix(data = X_iris, label = y_iris)
params = list(learning_rate = 1, min_split_loss = 0, reg_lambda = 0,
              objective = 'reg:squarederror', nthread = 1)
mod1 = xgboost::xgb.train(params = params, data = dtrain,
                          nrounds = 1, verbose = 0)

# Get interaction SHAP values (two methods):
data_int <- shap.prep.interaction(xgb_model = mod1, X_train = X_iris)
# Or directly:
shap_int <- predict(mod1, X_iris, predinteraction = TRUE)

# Plot interaction effects (y-axis shows interaction values)
shap.plot.dependence(data_long = shap_long_iris,
                           data_int = shap_int_iris,
                           x="Petal.Length",
                           y = "Petal.Width",
                           color_feature = "Petal.Width")
