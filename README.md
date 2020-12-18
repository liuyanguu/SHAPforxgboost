# SHAPforxgboost

[![CRAN version](http://www.r-pkg.org/badges/version/SHAPforxgboost)](https://cran.r-project.org/package=SHAPforxgboost) [![](https://cranlogs.r-pkg.org/badges/SHAPforxgboost)](https://cran.r-project.org/package=SHAPforxgboost) [![](https://cranlogs.r-pkg.org/badges/grand-total/SHAPforxgboost?color=orange)](https://cran.r-project.org/package=SHAPforxgboost)

This package creates SHAP (SHapley Additive exPlanation) visualization plots
 for 'XGBoost' in R. It provides summary plot, dependence plot, interaction plot,
 and force plot and relies on the SHAP implementation provided by 'XGBoost' and 'LightGBM'.
 Please refer to 'slundberg/shap' for the original implementation of SHAP in Python. 

All the functions except force plot return ggplot object, it is possible to add more layers. The dependence plot `shap.plot.dependence` returns ggplot object if without the marginal histogram by default.

To revise feature names, you could define a global variable named `new_labels`, the plotting functions will use this list as new feature labels. The `SHAPforxgboost::new_labels` is an object default to `NULL`. Or you could just overwrite the labels on the ggplot by adding a `labs` layer to the ggplot object. 

Please refer to this blog for the vignette of this package: more examples and discussion on SHAP values in R, why use SHAP, and a comparison to Gain: 
[SHAP visualization for XGBoost in R](https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/)


## Installation

Please install from CRAN or Github:
``` r
install.packages("SHAPforxgboost")
```

``` r
devtools::install_github("liuyanguu/SHAPforxgboost")
```

## Examples

**Summary plot**

```{r}
# run the model with built-in data, these codes can run directly if package installed  
library("SHAPforxgboost")
y_var <-  "diffcwv"
dataX <- as.matrix(dataXY_df[,-..y_var])

# hyperparameter tuning results
params <- list(objective = "reg:squarederror",  # For regression
                   eta = 0.02,
                   max_depth = 10,
                   gamma = 0.01,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

mod <- xgboost::xgboost(data = dataX, label = as.matrix(dataXY_df[[y_var]]), 
                       params = params, nrounds = 200,
                       verbose = FALSE, 
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
shap.plot.summary.wrap1(mod, X = dataX)

# option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross-validation)
shap.plot.summary.wrap2(shap_values$shap_score, dataX)

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
                           
# optional to put a different SHAP values on the y axis to view some interaction (Fig.B)      
shap.plot.dependence(data_long = shap_long, x= "dayint",
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
# notice: this step is slow since it calculates all the combinations of features. It may take over 5 minutes on a personal laptop.
shap_int <- shap.prep.interaction(xgb_mod = mod, X_train = dataX)
# it is the same as:
shap_int <- predict(mod, dataX, predinteraction = TRUE)

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

# plot by each cluster
shap.plot.force_plot_bygroup(plot_data)

```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-1.png"/>
</p>

<p align="center">  
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-16-2.png"/>
</p>

## Citation
The citation could be seen obtained using `citation("SHAPforxgboost")`
```{r}
To cite package ‘SHAPforxgboost’ in publications use:

  Yang Liu and Allan Just (2019). SHAPforxgboost: SHAP Plots for 'XGBoost'. R package version 0.0.3.
  https://github.com/liuyanguu/SHAPforxgboost/

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {SHAPforxgboost: SHAP Plots for 'XGBoost'},
    author = {Yang Liu and Allan Just},
    year = {2019},
    note = {R package version 0.0.6},
    url = {https://github.com/liuyanguu/SHAPforxgboost/},
  }
```

## Reference

My lab's paper using these figures:  
[Gradient Boosting Machine Learning to Improve Satellite-Derived Column Water Vapor Measurement Error](https://doi.org/10.5281/zenodo.3568449)

Corresponding SHAP plots package in Python: [https://github.com/slundberg/shap](https://github.com/slundberg/shap)
Paper 1. 2017 [A Unified Approach to Interpreting Model Predictions](https://arxiv.org/abs/1705.07874)  
Paper 2. 2019 [Consistent Individualized Feature Attribution for Tree
Ensembles](https://arxiv.org/abs/1802.03888)  
Paper 3. 2019 [Explainable AI for Trees: From Local Explanations to Global Understanding](https://arxiv.org/abs/1905.04610)
