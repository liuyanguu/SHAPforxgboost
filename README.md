# SHAPforxgboost

This package creates SHAP (SHapley Additive exPlanation) visualization plots
 for 'XGBoost' in R. It provides summary plot, dependence plot, interaction plot,
 and force plot. It relies on the 'dmlc/xgboost' package to produce SHAP values.
 Please refer to 'slundberg/shap' for the original implementation of SHAP in Python. 

The purpose is to create some conveniences for making these plot in R. I understand 'ggplot' is highly flexible so people always need to fine-tune here and there. But adding more flexibility just over-complicates the wrapped functions. I tried to find a balance. All the functions except force plot return ggplot object, it is possible to add more layers. The dependence plot `shap.plot.dependence` returns ggplot object if without the marginal histogram by default.

I have built in some default labels for feature names, you could also supply features labels as a list named `new_labels`, the functions will use this list and label accordingly. Another option is you can always overwrite the labels on the ggplot by adding `labs` layer to the ggplot object. 

Please refer to this blog for more examples and discussion on SHAP values in R, why use SHAP, and comparison to Gain: 
[SHAP visualization for XGBoost in R](https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/)


## Installation

Please install from github:
``` r
devtools::install_github("liuyanguu/SHAPforxgboost")
```

You can also install the released version of plotthis from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SHAPforxgboost")
```

## Examples

**Summary plot**

```{r}
# run the model with built-in data, these codes can run directly if package installed  
library("SHAPforxgboost")
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

mod <- xgboost::xgboost(data = as.matrix(dataX), label = as.matrix(dataXY_df[[y_var]]), 
                       xgb_param = param_dart, nrounds = param_dart$nrounds,
                       verbose = FALSE, nthread = parallel::detectCores() - 2,
                       early_stopping_rounds = 8)
                       
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod, X_train = dataX)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score

# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = mod, X_train = dataX)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = dataX)
# (Notice that there will be a data.table warning from `melt.data.table` due to `dayint` coerced from integer to double)

# **SHAP summary plot**
shap.plot.summary(shap_long)

# sometimes for a preview, you want to plot less data to make it faster using `dilute`
shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)

# Alternatives options to make the same plot:
# option 1: start with the xgboost model
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
# if without y, will just plot SHAP values of x vs. x
shap.plot.dependence(data_long = shap_long, x = "dayint")


# optional to color the plot by assigning `color_feature` (Fig.A)
shap.plot.dependence(data_long = shap_long, x= "dayint",
                           color_feature = "Column_WV")
                           
# optional to put a different SHAP values on the y axis to view some interaction (Fig.B)                shap.plot.dependence(data_long = shap_long, x= "dayint",
                           y = "Column_WV", color_feature = "Column_WV")                          

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-11-1.png"/>
</p>


```{r}
# To make plots for a group of features:
fig_list = lapply(names(shap_values$mean_shap_score)[1:6], shap.plot.dependence, 
                  data_long = shap_long, dilute = 5)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)

```

**SHAP interaction plot**

```{r}
# prepare the data using either: 
# (this step is slow since it calculates all the combinations of features. This example takes 10s.)
data_int <- shap.prep.interaction(xgb_mod = mod, X_train = as.matrix(dataX))
# or:
shap_int <- predict(mod, as.matrix(dataX), predinteraction = TRUE)

# **SHAP interaction effect plot **
shap.plot.dependence(data_long = shap_long,
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
# choose to show top 4 features by setting `top_n = 4`, set 6 clustering groups.  
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
# choose to zoom in at location 500, set y-axis limit using `y_parent_limit`  
# it is also possible to set y-axis limit for zoom-in part alone using `y_zoomin_limit`  
shap.plot.force_plot(plot_data, zoom_in_location = 500, y_parent_limit = c(-1,1))
shap.plot.force_plot_bygroup(plot_data)

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-1.png"/>
</p>

<p align="center">  
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-2.png"/>
</p>



## Reference

Recently submitted paper from my lab that applies these figures:
[Gradient Boosting Machine Learning to Improve Satellite-Derived Column Water Vapor Measurement Error](http://doi.org/10.5281/zenodo.3334713)

Corresponding SHAP plots package in Python: [https://github.com/slundberg/shap](https://github.com/slundberg/shap)

Paper 1. 2017 [A Unified Approach to Interpreting Model Predictions](https://arxiv.org/abs/1705.07874)  
Paper 2. 2019 [Consistent Individualized Feature Attribution for Tree
Ensembles](https://arxiv.org/abs/1802.03888)  
Paper 3. 2019 [Explainable AI for Trees: From Local Explanations to Global Understanding](https://arxiv.org/abs/1905.04610)
