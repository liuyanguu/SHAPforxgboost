# SHAPforxgboost 0.0.6
* 12/12/2020 Added support for LightGBM.

# SHAPforxgboost 0.0.3
* 01/22/2020 I realized that I did not watch my own repo, I fixed some issues raised, for example, y-axis goes under the data instead of on it.    
* 02/08/2020 Added an `var_cat` option to `shap.prep` so if supply a categorical variable, the long-format data would use `var_cat` as a labelling variable. For example, we can make two summary plot by adding `facet_wrap(~ var_cat)` to the `shap.plot.summary`  .
* Added an ID variable for each observation to the dataset produced by `shap.prep` and `shap.prep.stack.data`, which might be useful under certain situation.  
* Revised most functions' documentations.

# SHAPforxgboost 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* 07/30/2019 Version 0.0.1 uploaded to cran.
* 08/10/2019 Version 0.0.2 released with update, fixed some bugs with dilute. 
* 08/27/2019 Major change, merged function `shap.plot.dependence.color` into `shap.plot.dependence`.
