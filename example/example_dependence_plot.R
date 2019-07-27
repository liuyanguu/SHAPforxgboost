library(SHAPforxgboost)
# **SHAP dependence plot**
plot.shap.dependence.color(data_long = shap_long_iris, x="Petal.Length",
                           y = "Petal.Width", color_feature = "Petal.Width")
# the without color version, just plot SHAP value against feature value:
plot.shap.dependence(data_long = shap_long_iris, "Petal.Length")

