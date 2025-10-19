library(ggplot2)
library(patchwork)

plot_sequence <- function(n_value) {
  n <- 1:n_value
  x_n <- (1 + 1/n)^n
  e <- exp(1)
  df <- data.frame(n, x_n)
  
  ggplot(df, aes(x = n, y = x_n)) +
    geom_line(color = "steelblue", size = 1) +
    geom_hline(yintercept = e, linetype = "dashed", color = "red") +
    labs (
      x = "n",
      y = expression(x[n])
    ) +
    theme_minimal(base_size = 15)
}

p1 <- plot_sequence(10)
p2 <- plot_sequence(50)
p3 <- plot_sequence(100)
p4 <- plot_sequence(500)
p5 <- plot_sequence(1000)

final_plot <- (p1 | p2) / ( p3 | p4) / p5 +
  plot_annotation(title = "Hoi tu cua (1 + 1/n)^n ->e voi cac kich thuoc khac mau khac nhau")

print(final_plot)

