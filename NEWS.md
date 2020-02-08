# SHAPforxgboost 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* 07/30/2019 Version 0.0.1 uploaded to cran.
* 08/10/2019 Version 0.0.2 released with update, fixed some bugs with dilute. 
* 08/27/2019 Major change, merge function `shap.plot.dependence.color` into `shap.plot.dependence`.

# SHAPforxgboost 0.0.3
* 01/22/2020 I am sorry that I did not watch my own repo, I fixed issues raised, for example, y-axis goes under the data instead of on it.   
* 02/08/2020 Add an `var_cat` option to `shap.prep` so if supply a categorical variable, the long-format data would use `var_cat` as a labelling variable. For example, we can make two summary plot by adding `facet_wrap(~ var_cat)` to the `shap.plot.summary`
