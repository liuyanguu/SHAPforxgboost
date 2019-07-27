
# **SHAP force plot**
plot_data <- shap.stack.data(shap_contrib = shap_values_iris, n_groups = 2)
plot.shap.force_plot(plot_data)
plot.shap.force_plot_bygroup(plot_data)
