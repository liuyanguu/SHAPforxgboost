# SHAPforxgboost

** Under further revise **

<!-- badges: start -->
<!-- badges: end -->

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
plot.shap.summary(shap_long)
# or 
plot.shap.summary.wrap1(mod1, X_data, top_n = 9)
# or
plot.shap.summary.wrap2(shap_score = SHAP_values, X_data, top_n = 9)
```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-8-1.png"/>
</p>

**Dependence plot**

```{r}
plot.shap.dependence.color(shap_long, x="Petal.Length",
                           y_shap = "Petal.Length", color_feature = "Petal.Width")
```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-11-1.png"/>
</p>

**Force plot**

```{r}
plot.shap.force_plot(force_plot_data)
```

<p align="center">
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-7-1.png"/>
</p>

<p align="center">  
  <img src = "https://liuyanguu.github.io/post/2019-07-18-visualization-of-shap-for-xgboost_files/figure-html/unnamed-chunk-7-2.png"/>
</p>

## Sample code 

```{r}
# Example use iris
data("iris")
X1 = as.matrix(iris[,-5])
mod1 = xgboost(
  data = X1, label = iris$Species, gamma = 0, eta = 1, lambda = 0,nrounds = 1, verbose = F)

# shap.values() has the SHAP data matrix and ranked features by mean|SHAP|
shap_values <- shap.values(mod1, X1)
# ranked features:
shap_values$mean_shap_score

# shap.prep() returns the long-format SHAP data
shap_long <- shap.prep(shap_values, X1)
# or
shap_long <- shap.prep(shap.values(mod1, X1), X1)

# **SHAP summary plot**
plot.shap.summary(shap_long)

# Alternatives:
# option 1: from the xgboost model
plot.shap.summary.wrap1(mod1, X1, top_n = 3)

# option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross-validation)
plot.shap.summary.wrap2(shap_score = shap_values$shap_score, X1, top_n = 3)

# **SHAP dependence plot**
plot.shap.dependence.color(shap_long, x="Petal.Length",
                           y_shap = "Petal.Length", color_feature = "Petal.Width")

# **SHAP force plot**
force_plot_data <- shap.stack.data(shap_contrib = shap_values$shap_score, n_groups = 2)
plot.shap.force_plot(force_plot_data)
plot.shap.force_plot_bygroup(force_plot_data)
```


## Reference

Main references by Slundberg:  
Paper 1. 2017 [A Unified Approach to Interpreting Model Predictions](https://arxiv.org/abs/1705.07874)  
Paper 2. 2019 [Consistent Individualized Feature Attribution for Tree
Ensembles](https://arxiv.org/abs/1802.03888)  
Paper 3. 2019 [Explainable AI for Trees: From Local Explanations to Global Understanding](https://arxiv.org/abs/1905.04610)
