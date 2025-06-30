library(ggplot2)
library(dplyr)
library(tidyr)

param8_samples <- coeffs[, 8, ]
num_equations <- nrow(param8_samples)
num_iterations <- ncol(param8_samples)
iteration_axis <- 1:num_iterations

param8_samples_full <- coeffs[, 8, ]
num_total_equations_in_original <- nrow(param8_samples_full)

states_to_plot <- c(1, 2, 4, 5)

param8_samples <- param8_samples_full[states_to_plot, , drop = FALSE]

num_equations <- nrow(param8_samples)
param8_df_wide <- as.data.frame(t(param8_samples))
colnames(param8_df_wide) <- paste("Hidden State", states_to_plot)
param8_df_wide$Iteration <- 1:num_iterations

data_long <- tidyr::pivot_longer(
  param8_df_wide,
  cols = starts_with("Hidden State"),
  names_to = "Equation_Label",
  values_to = "Value"
)

data_long$Equation_Label <- as.character(data_long$Equation_Label)
new_levels <- paste("Hidden State", c(1, 2, 4, 5))
data_long$Equation_Label <- factor(data_long$Equation_Label, levels = new_levels)

summary_stats <- data_long %>%
  group_by(Equation_Label) %>%
  summarise(
    Mean_Value = mean(Value),
    Median_Value = median(Value),
    .groups = 'drop'
  )

plot3_density_gg <- ggplot(data_long, aes(x = Value)) +
  geom_density(aes(fill = Equation_Label), alpha = 0.7, show.legend = FALSE) +
  geom_vline(data = summary_stats, aes(xintercept = Mean_Value, linetype = "Mean"), color = "red", linewidth = 1) +
  scale_linetype_manual(
    values = c("Mean" = "solid"),
    labels = c("Mean")
  ) +
  facet_wrap(~ Equation_Label, scales = "free", ncol = 1) +
  labs(
    x = 'Coefficients of temp_s',
    y = "Density"
  ) +
  theme_linedraw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-7, 7))

print(plot3_density_gg)
ggsave("density_plot_beta8-5.png", plot = plot3_density_gg, width = 8, height = 7, dpi = 300, units = "in")