# Example: SHAP force plots (stacked bar charts)
# Shows contribution of each feature to individual predictions

plot_data <- shap.prep.stack.data(shap_contrib = shap_values_iris,
                                  n_groups = 4)
shap.plot.force_plot(plot_data)
shap.plot.force_plot(plot_data, zoom_in_group = 2)

# Plot all clusters separately
shap.plot.force_plot_bygroup(plot_data)
