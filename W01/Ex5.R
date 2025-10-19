#EX5
library(ggplot2)
library(dplyr)
library(patchwork)

ns <- c(2, 5, 10, 50, 200)
x <- seq(0, 1, length.out = 400)


df_list <- lapply(ns, function(n) {
  data.frame(x = x, n = n, fn = n * x^(n - 1))
})
df_all <- bind_rows(df_list)


df_lim <- data.frame(
  x = x,
  limfn = ifelse(x < 1, 0, 1)
)


n_values <- seq(0, 20, by = 1)
integrals <- sapply(n_values, function(n) {
  if (n == 0) return(NA)
  integrate(function(x) n * x^(n - 1), 0, 1)$value
})
ints <- data.frame(n = n_values, integral = integrals)

#Hinh 1
p1 <- ggplot(df_all, aes(x = x, y = fn, color = factor(n))) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = expression("Do thi f_n(x) == n*x^(n-1)"),
    subtitle = "Cac ham f_n(x) voi n khac nhau",
    x = "x", y = expression(f[n](x)),
    color = "n"
  ) +
  theme_gray(base_size = 15) +
  coord_cartesian(ylim = c(0, 200)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Hinh 2
p2 <- ggplot(df_lim, aes(x = x, y = limfn)) +
  geom_line(color = "blue", linewidth = 2) +
  geom_point(aes(x = 1, y = 1), color = "red", size = 3) +
  labs(
    title = expression("Ham gioi han" ~ lim(f[n](x))),
    x = "x", y = expression(lim(f[n](x)))
  ) +
  coord_cartesian(ylim = c(0, 1.0)) +
  theme_gray(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Hinh 3
p3 <- ggplot(ints, aes(x = n, y = integral)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Su hoi tu cua tich phan",
    subtitle = expression(lim(integral(f[n])) != integral(lim(f[n]))),
    x = "n", y = expression(integral[0]^1~f[n](x)*dx)
  ) +
  theme_gray(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0)
  ) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(5, 10, 15, 20), expand = expansion(mult = c(0, 0.05)))

(p1 + p2) / p3