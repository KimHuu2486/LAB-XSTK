#EX7

x_values <- c(1.5, 2, 3, 10)
n_values <- c(50, 100, 150, 200)
simulation_df <- expand.grid(x = x_values, n = n_values)

simulation_df$gia_tri_bieu_thuc <- simulation_df$n * (simulation_df$x^(1/simulation_df$n) - 1)
simulation_df$gia_tri_ln_x <- log(simulation_df$x)
simulation_df$sai_so <- abs(simulation_df$gia_tri_bieu_thuc - simulation_df$gia_tri_ln_x)

cat("Ket qua mo phong so:", '\n')
print(simulation_df)

n_plot_values <- 1:200
plot_df <- expand.grid(x = x_values, n = n_plot_values)
plot_df$gia_tri_bieu_thuc <- plot_df$n * (plot_df$x^(1/plot_df$n) - 1)

my_plot <- ggplot(plot_df, aes(x = n, y = gia_tri_bieu_thuc, color = as.factor(x))) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = log(1.5), color = "gray", linetype = "dashed") +
  geom_hline(yintercept = log(2), color = "gray", linetype = "dashed") +
  geom_hline(yintercept = log(3), color = "gray", linetype = "dashed") +
  geom_hline(yintercept = log(10), color = "gray", linetype = "dashed") +
  labs(
    title = "Hoi tu cua n(x^(1/n) - 1) -> ln(x)",
    x = "n",
  ) +
  
  scale_color_discrete(name = 'x') +
  theme_grey(base_size = 15) +
  theme(
    axis.title.y = element_blank()
  )

my_plot + coord_cartesian(ylim = c(0, 7))