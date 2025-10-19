# EX2
library("ggplot2")

riemann_sum <- function(f_x, a, b, n) {
  delta <- (b - a) / n
  x_mid <- seq(a, b - delta, delta)
  r_sum = sum(f_x(x_mid) * delta)
  return(r_sum)
}

f_x <- function(x) {
  return(exp(-(x^2)))
}

a = 0L
b = 1L

n <- seq(1, 501, 10)
r_sum_n <- c()
for (i in n) {
  r_sum_n <- c(r_sum_n, riemann_sum(f_x, a, b, i))
}

df <- data.frame(n = n, r_sum_n = r_sum_n)

plot <- ggplot(df, aes(x = n, y = r_sum_n)) + 
  labs (
    title = "Tổng Riemann xấp xỉ",
    x = "Số khoảng con (n)",
    y = "Giá trị xấp xỉ"
  ) +
  geom_line(color = "red") + 
  geom_point(size = 1, color = "blue")

print(plot)