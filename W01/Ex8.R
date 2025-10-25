#Ex8
library(ggplot2)
library(dplyr)
library(patchwork)

#1) dinh nghia
a_n <- function(n) 2^(-n)
b_n <- function(n) 2^(1 - n)

f_n <- function(x, n) ifelse(x > a_n(n) & x < b_n(n), 2^n, 0)

# hinh chu nhat (area = 1)
fn_rect <- function(n) {
  a <- a_n(n); b <- b_n(n); h <- 2^n
  width <- b - a; area <- h * width
  
  df_rect <- data.frame(x = c(a, b, b, a), y = c(0, 0, h, h))
  point_data <- data.frame(x = seq(0, 1, length.out = 21), y = 0)
  
  ggplot() +
    geom_polygon(data = df_rect, aes(x, y),
                 fill = "steelblue", alpha = 0.55, color = "black") +
    geom_point(data = point_data, aes(x, y), color = "red", size = 2) +
    geom_segment(aes(x = a, xend = b, y = h, yend = h),
                 color = "red", linetype = "dashed") +
    geom_segment(aes(x = a, xend = a, y = 0, yend = h),
                 color = "red", linetype = "dashed") +
    geom_segment(aes(x = b, xend = b, y = 0, yend = h),
                 color = "red", linetype = "dashed") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, h * 1.1)) +
    labs(
      title = bquote(f[.(n)](x) ~ "- on (" * 2^{-.(n)} * "," ~ 2^{1-.(n)} * ")"),
      subtitle = paste0("area = ", round(h, 3), " x ", round(width, 6),
                        " = ", round(area, 3)),
      x = "x", y = expression(f[n](x))
    ) +
    annotate("text", x = (a + b) / 2, y = h * 1.02, label = "area = 1",
             color = "red", size = 4) +
    theme_minimal(base_size = 14)
}

# 4 hinh dau
p1 <- fn_rect(1); p2 <- fn_rect(2); p3 <- fn_rect(3); p4 <- fn_rect(4)
(p1 + p2) / (p3 + p4)

# 3) thu hep khoang theo n
Ns <- 1:8
intervals <- tibble(
  n = Ns,
  x_left  = a_n(Ns),
  x_right = b_n(Ns)
)

p_shrink <- ggplot(intervals) +
  geom_segment(aes(x = x_left, xend = x_right, y = n, yend = n),
               linewidth = 6, color = "royalblue") +
  geom_point(aes(x = (x_left + x_right)/2, y = n), color = "red", size = 2) +
  scale_y_reverse(breaks = Ns) +
  labs(
    title = "Interval shrink as n increases",
    subtitle = "interval (2^{-n}, 2^{1-n}) -> 0 as n -> inf",
    x = "x in [0,1]", y = "n"
  ) +
  theme_minimal(base_size = 14)
p_shrink

# (a) tich phan on [0,1]
n_values <- c(1, 2, 3, 4, 6, 8, 10, 12)
ints <- sapply(n_values, function(n) {
  integrate(function(x) f_n(x, n), lower = 0, upper = 1)$value
})
df_int <- data.frame(n = n_values, integral = ints)

p_int <- ggplot(df_int, aes(n, integral)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(title = "int_0^1 f_n(x) dx = 1 (all n)", x = "n", y = "integral") +
  theme_minimal(base_size = 14)
p_int

# (b)
x_grid <- seq(0, 1, length.out = 600)
df_plot <- bind_rows(lapply(c(1, 2, 4, 8, 16), function(n) {
  tibble(x = x_grid, n = n, fn = f_n(x_grid, n))
}))

p_fn <- ggplot(df_plot, aes(x, fn, color = factor(n))) +
  geom_line() +
  labs(title = expression("f[n](x) for several n"),
       x = "x", y = expression(f[n](x)), color = "n") +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal(base_size = 14)

p_lim <- ggplot(data.frame(x = x_grid, y = 0), aes(x, y)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = expression("limit of f[n](x) is 0 on [0,1]"),
       x = "x", y = "limit f_n(x)") +
  theme_minimal(base_size = 14)

p_fn / p_lim