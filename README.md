# SHAPforxgboost

The goal of this SHAPforxgboost **R** package is to create SHAP (SHapley Additive exPlnation) visualization plots for XGBoost in R. 



Please refer to this blog for more examples and discussion on SHAP values in R, why use SHAP, and compared to Gain: 
[SHAP visualization for XGBoost in R](https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/)


## Installation

Please install from github:
``` r
devtools::install_github("liuyanguu/SHAPforxgboost")
```

## Examples

Code in the next session

**Summary plot**

```{r}
# run the model with built-in data
y_var <-  "diffcwv"
dataX <- dataXY_df[,-..y_var]
# hyperparameter tuning results
param_dart <- list(objective = "reg:linear",  # For regression
                   nrounds = 366,
                   eta = 0.018,
                   max_depth = 10,
                   gamma = 0.009,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

mod <- xgboost.fit(X = as.matrix(dataX), Y = as.matrix(dataXY_df[[y_var]]), 
                       xgb_param = param_dart)
                       
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod, X_train = dataX)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score

# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = mod, X_train = dataX)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = dataX)

# **SHAP summary plot**
shap.plot.summary(shap_long)
shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)

# Alternatives options to make the same plot:
# option 1: from the xgboost model
shap.plot.summary.wrap1(mod, X = as.matrix(dataX))

# option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross-validation)
shap.plot.summary.wrap2(shap_values$shap_score, as.matrix(dataX))

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-9-1.png" width="500" height="400"/>
</p>

**Dependence plot**

```{r}
# **SHAP dependence plot**
shap.plot.dependence.color(data_long = shap_long, x= "Column_WV",
                           y = "AOT_Uncertainty", color_feature = "AOT_Uncertainty")
                           
shap.plot.dependence.color(data_long = shap_long, x= "dayint",
                           y = "Column_WV", color_feature = "Column_WV")                           
                           
# without color version but has marginal distribution, just plot SHAP value against feature value:
shap.plot.dependence(data_long = shap_long, "Column_WV")

```

```{r}
fig_list = lapply(names(shap_values$mean_shap_score)[1:6], shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-11-1.png"/>
</p>


**SHAP interaction plot**

```{r}
# prepare the data using either: 
# (this step is slow since it calculates all the combinations of features.)
data_int <- shap.prep.interaction(xgb_mod = mod, X_train = as.matrix(dataX))
# or:
shap_int <- predict(mod, as.matrix(dataX), predinteraction = TRUE)

# **SHAP interaction effect plot **
shap.plot.dependence.color(data_long = shap_long,
                           data_int = shap_int,
                           x= "Column_WV",
                           y = "AOT_Uncertainty", 
                           color_feature = "AOT_Uncertainty")


```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-15-1.png"/>
</p>



**SHAP force plot**

```{r}
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
shap.plot.force_plot(plot_data)
shap.plot.force_plot_bygroup(plot_data)

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-1.png"/>
</p>

<p align="center">  
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-2.png"/>
</p>



## Reference

Recent submitted paper from my lab that applies these figures:
[Gradient Boosting Machine Learning to Improve Satellite-Derived Column Water Vapor Measurement Error](http://doi.org/10.5281/zenodo.3334713)

Corresponding SHAP plots package in Python: [https://github.com/slundberg/shap](https://github.com/slundberg/shap)

Paper 1. 2017 [A Unified Approach to Interpreting Model Predictions](https://arxiv.org/abs/1705.07874)  
Paper 2. 2019 [Consistent Individualized Feature Attribution for Tree
Ensembles](https://arxiv.org/abs/1802.03888)  
Paper 3. 2019 [Explainable AI for Trees: From Local Explanations to Global Understanding](https://arxiv.org/abs/1905.04610)
