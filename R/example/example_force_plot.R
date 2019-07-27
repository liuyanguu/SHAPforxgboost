
# **SHAP force plot**
plot_data <- shap.prep.stack.data(shap_contrib = shap_values_iris,
                                  n_groups = 4)
shap.plot.force_plot(plot_data)
shap.plot.force_plot(plot_data,  zoom_in_group = 2)

# plot all the clusters:
shap.plot.force_plot_bygroup(plot_data)
