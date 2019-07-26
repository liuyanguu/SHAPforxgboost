# prepare dataset, and xgboost model for plotting
# to install
# devtools::install_github("liuyanguu/SHAPforxgboost")
# Please cite http://doi.org/10.5281/zenodo.3334713
# these code run through 3 examples


library("SHAPforxgboost")

# Sample 1. Satellite data ------------------------------------------------

y_var <-  "diffcwv"
date0 <- format(Sys.Date(), "%y_%m_%d")
dataX <- dataXY_df[,-..y_var]
# hyperparameter tuning results
param_dart <- list(objective = "reg:linear",  # For regression
                   nrounds = 366,
                   eta = 0.018,
                   max_depth = 10,
                   gamma = 0.009,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

xgb_mod <- xgboost.fit(X = as.matrix(dataX), Y = as.matrix(dataXY_df[[y_var]]), xgb_param = param_dart)
pred_mod <- predict(xgb_mod, as.matrix(dataX))

# to get shap_long directly from model
shap_values_mod <- shap.values(xgb_model = xgb_mod, X_train = dataX)
shap_long_mod <- shap.prep(shap_values_mod, dataX)

# or if the SHAP values (`shap_score`) have already been calculated during cross-validation process,
# as in my case:
shap_values <- list(
  shap_score = shap_score,
  mean_shap_score = colMeans(abs(shap_score))[order(colMeans(abs(shap_score)), decreasing = T)]
)
shap_long <- shap.prep(shap_values, dataX) # the long data from the paper
## summary plot
plot.shap.summary(shap_long_mod, dilute = NULL)
##
plot.shap.summary(shap_long_mod, dilute = 10500)

## interaction plot
shap_int <- predict(xgb_mod, as.matrix(dataX), predinteraction = TRUE)
dim(shap_int)
shap_int[, , 1]
# same:
head(rowSums(shap_int, dims = 1),20)
head(predict(xgb_mod, as.matrix(dataX)),20)

# rowSums by `dims = 1` returns the model output, since it adds up all the SHAP values
rowSums(shap_int, dims = 1)
# rowSums by `dims = 2`, gives the 150 x 5 shap values
rowSums(shap_int, dims = 2)

# the dependence plot
plot.shap.dependence.color(data_long = shap_long, x = 'dayint', y = 'Column_WV')
# the interaction SHAP plot
plot.shap.dependence.color(data_long = shap_long, x = 'dayint', y = 'Column_WV', data_int = shap_int)


# Sample 2. Model A --------------------------------------------------

d <- as.data.table(cbind(Fever = c(0,0,1,1),
                         Cough = c(0,1,0,1),
                         y = c(0,0,0,80)
))
X1 = as.matrix(d[,.(Fever, Cough)])
m1 = xgboost::xgboost(
  data = X1, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0, nrounds = 1, verbose = F)
shap_m <- shap.values(m1, X1)
shap_long_m <- shap.prep(shap_m, X1)
names(shap_m$shap_score) <- paste0("SHAP.", names(shap_m$shap_score))
output_simple <- cbind(x = X1, y.actual = d$y, y.pred = predict(m1, X1), shap_m$shap_score, BIAS = shap_m$BIAS0)
output_simple

# Not very meaningful to use on very small dataset. But can run.
plot.shap.summary(shap_long_m)
plot.shap.summary.wrap1(m1, X1, dilute = F)
plot.shap.dependence.color(data_long = shap_long_m, x = "Fever", y = "Cough", smooth = F)

# Sample 3. Example using iris ------------------------------------------------------

data("iris")
X1 = as.matrix(iris[,-5])
mod1 = xgboost::xgboost(
  data = X1, label = iris$Species, gamma = 0, eta = 1, lambda = 0,nrounds = 1, verbose = F)

shap_int <- predict(mod1, as.matrix(X1), predinteraction = TRUE)
shap_int[, , "Sepal.Length"]
shap_int["Petal.Length", "Petal.Width", ]
# rowSums by first dimention returns the model output, since it adds up all the SHAP values
rowSums(shap_int, dims = 1)
# rowSums by dims = 2, gives 150 x 5 shap values
rowSums(shap_int, dims = 2)

# shap.values() has the SHAP data matrix and ranked features by mean|SHAP|
shap_values <- shap.values(mod1, X1)
shap_values$shap_score
shap_values$mean_shap_score

# shap.prep() returns the long-format SHAP data
shap_long <- shap.prep(shap_values, X1)

# **SHAP summary plot**
plot.shap.summary(data_long2, dilute = 100)

# Alternatives:
# option 1: from the xgboost model
plot.shap.summary.wrap1(mod1, X1, top_n = 3, dilute = FALSE)

# option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross-validation)
plot.shap.summary.wrap2(shap_score = shap_values$shap_score, X1, top_n = 3)

# **SHAP dependence plot**
plot.shap.dependence.color(shap_long, x="Petal.Length",
                           y_shap = "Petal.Length", color_feature = "Petal.Width")
# without color
plot.shap.dependence(shap_long, show_feature = "Petal.Length")

# **SHAP force plot**
force_plot_data <- shap.stack.data(shap_contrib = shap_values$shap_score, n_groups = 4)
plot.shap.force_plot(force_plot_data)
plot.shap.force_plot_bygroup(force_plot_data)
