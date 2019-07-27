
# **SHAP force plot**
plot_data <- shap.prep.stack.data(shap_contrib = shap_values_iris,
                                  n_groups = 2)
shap.plot.force_plot(plot_data)
shap.plot.force_plot_bygroup(plot_data)
