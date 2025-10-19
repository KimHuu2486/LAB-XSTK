#EX6
n_large = 200
n_values <- c(5, 10, 20, 50, 100, 200)

integrand_a <- function(x, n) {
  exp(n * log(1 + x / n) - 2 * x)
}

integrand_b <- function(x, n) {
  ifelse(x > n, 0, exp(n * log(1 - x / n) + x / 2))
}

analyze_and_plot <- function(n_values, integrand_function, limit_value, plot_title) {
  
  integral_values <- sapply(n_values, function(current_n) {
    result <- integrate(integrand_function, lower = 0, upper = current_n, n = current_n)
    return(result$value)
  })
  
  df <- data.frame(n = n_values, Gia_tri_tich_phan = integral_values)
  
  p <- ggplot(df, aes(x = n, y = Gia_tri_tich_phan)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 4, shape = 16) +
    geom_hline(yintercept = limit_value, color = "red", linetype = "dashed", linewidth = 1.2) +
    labs(
      title = plot_title,
      x = "n",
      y = "Gia tri tich phan"
    ) +
    theme_minimal(base_size = 15)
  
  print(p)
  
  return(df)
}

n_values_to_check <- c(5, 10, 20, 50, 100, 200)

result_a <- integrate(integrand_a, lower = 0, upper = n_large, n = n_large)
cat("Ket qua cau a:", result_a$value, "\n")

df_a <- analyze_and_plot(
  n_values = n_values_to_check,
  integrand_function = integrand_a,
  limit_value = 1, "(a) Hoi tu ve 1"
)

result_b <- integrate(integrand_b, lower = 0, upper = n_large, n = n_large)
cat("Ket qua cau b:", result_b$value, "\n")

df_b <- analyze_and_plot(
  n_values = n_values_to_check,
  integrand_function = integrand_b,
  limit_value = 2, "(b) Hoi tu ve 2"
)
