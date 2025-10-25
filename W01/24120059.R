#Exercise 1

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


#Exercise 2

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


#Exercise 3

library("ggplot2")

generate_matrix <- function(m, n, rank) {
  mat_u <- matrix(rnorm(m * rank), nrow = m, ncol = rank)
  mat_v <- matrix(rnorm(rank * n), nrow = rank, ncol = n)
  
  final_mat <- mat_u %*% mat_v
  return(final_mat)
}

frob_norm <- function(A) {
  res <- sqrt(sum(A^2))
  return(res)
}

spec_norm <- function(A) {
  res <- sqrt(max(eigen(t(A) %*% A)$values))
  return(res)
}

d1 <- function(x) x
d2 <- function(x) 2*x

num_rows = 5
num_cols = 6
rank = 4
iterations = 500

frobenius <- c()
spectral <- c()

for (i in 1:iterations) {
  mat_a <- generate_matrix(num_rows, num_cols, rank)
  frobenius <- c(frobenius, frob_norm(mat_a))
  spectral <- c(spectral, spec_norm(mat_a))
}

df <- data.frame(spectral = spectral, frobenius = frobenius)
plot <- ggplot(df, aes(x = spectral, frobenius)) + 
  labs (
    title = "Mối liên hệ giữa chuẩn Frobenius và  chuẩn phổ của ma trận",
    x = "Frobenius",
    y = "Spectral"
  ) +
  geom_point(color = "blue") + 
  stat_function(fun = d1, linetype = "dashed", color = "red", size = 1) + 
  stat_function(fun = d2, linetype = "dashed", color = "red", size = 1)

print(plot)


#Exercise 4

library(ggplot2)

x <- seq(0, 2*pi, length.out = 200)
h <- 0.01

f <- sin(x)
f_approx <- (sin(x + h) - sin(x)) / h
f_true <- cos(x)

data <- data.frame(x = x, approx = f_approx, true = f_true)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = approx), color = "blue", linewidth = 1) +
  geom_line(aes(y = true), color = "red", linewidth = 0.5, linetype = "dashed")+
  labs(
    title = "So sanh dao ham xap xi va dao ham that cua sin(x)",
    y = "Gia tri dao ham",
    x = "x"
  ) +
  theme_gray(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )


# Exercise 5

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


#Exrercise 6

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



#Exercise 7

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


#Exercise 8

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


#Exercise 9
library(ggplot2)

# Du lieu
colour <- c("Do","Xanh","Xanh","Vang","Do","Tim","Xanh","Do","Do","Cam",
            "Vang","Xanh","Do","Tim","Xanh","Do","Xanh","Xanh","Do","Cam",
            "Do","Xanh","Vang","Do","Xanh","Tim","Do","Xanh","Do","Xanh")

# a) Trung binh
cat("Mean: khong co y nghia voi du lieu nominal.\n")

# b) Ham mode cho du lieu dinh danh
mode_nominal <- function(x) {
  tb <- table(x)
  names(tb)[tb == max(tb)]
}

modes <- mode_nominal(colour)
cat("Mode(s):", paste(modes, collapse = ", "), "\n")

# Bang tan so
print(table(colour))

# c) Bieu do cot + chu thich mode
df <- as.data.frame(table(colour), stringsAsFactors = FALSE)

ggplot(df, aes(x = colour, y = Freq, fill = colour)) +
  geom_col() +
  geom_text(aes(label = Freq), vjust = -0.4, size = 4) +
  labs(title = "Phan phoi mau sac yeu thich",
       subtitle = paste0("Mode: ", paste(modes, collapse = ", ")),
       x = "Mau sac", y = "So luong") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
#Ex 9.2

# data
satisfaction_lvl <- c(4, 5, 3, 4, 5, 2, 4, 5, 4, 5, 4, 3,
                      5, 4, 3, 5, 4, 5, 4, 3, 5, 4, 5, 4,
                      3, 4, 5, 4, 3, 5, 4)


satisfaction_lvl <- factor(
  satisfaction_lvl,
  levels = 1:5,
  labels = c("Rat khong hai long", "Khong hai long",
             "Binh thuong", "Hai long", "Rat hai long"),
  ordered = TRUE
)

# a) Mean (tren coding 1..5)
mean_numeric <- mean(as.integer(satisfaction_lvl))
cat("Mean (using scores 1..5):", mean_numeric, "\n")

#b) Median cho ordinal
median_ord <- function(f) {
  idx <- median(as.integer(f))
  levels(f)[[idx]]
}
median_label <- median_ord(satisfaction_lvl)
cat("Median (ordinal):", median_label, "\n")

#c ) Mode
mode_factor <- function(f) {
  tb <- table(f)
  names(tb)[tb == max(tb)]
}
modes <- mode_factor(satisfaction_lvl)
cat("Mode(s):", paste(modes, collapse = ", "), "\n")

# Bang tan so
print(table(satisfaction_lvl))

# bar  vs  median line
df <- as.data.frame(table(satisfaction_lvl), stringsAsFactors = FALSE)
names(df) <- c("lvl", "Freq")

# vi tri ua median tren truc x
med_idx <- which(levels(satisfaction_lvl) == median_label)

ggplot(df, aes(x = lvl, y = Freq, fill = lvl)) +
  geom_col() +
  geom_text(aes(label = Freq), vjust = -0.4, size = 4) +
  geom_vline(xintercept = med_idx, linetype = "dashed") +
  labs(title = "Phan phoi muc do hai long",
       subtitle = paste0("Median: ", median_label,
                         "   |   Mode: ", paste(modes, collapse = ", ")),
       x = "Muc do hai long (ordinal)", y = "So luong") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#Exercise 10

coBan <- c(4, 15, 24, 10, 1, 27, 31, 5, 20, 29, 15, 7, 32, 36, 14, 2, 16, 32, 7)
Moi <- c(15, 7, 32, 36, 17, 15, 19, 35, 10, 16, 39, 29, 6, 12, 18, 14, 15, 18, 21)

# Câu a:

# vẽ biểu đồ
par(mfrow = c(2, 2))

hist(coBan,
     main = "Biểu đồ tần số - Phương pháp cơ bản",
     xlab = "Thời gian sống sót (tháng)",
     ylab = "Tần số",
     col = "skyblue", border = "black")

hist(Moi,
     main = "Biểu đồ tần số - Phương pháp mới",
     xlab = "Thời gian sống sót (tháng)",
     ylab = "Tần số",
     col = "lightgreen", border = "black")

hist(coBan, 
     probability = TRUE, 
     col = "skyblue", 
     border = "black",
     main = "Biểu đồ tần suất - Phương pháp cơ bản",
     xlab = "Thời gian sống sót(tháng)",
     ylab = "Tần suất")
lines(density(coBan), col = "red", lwd = 2)

hist(Moi, 
     probability = TRUE, 
     col = "lightgreen", 
     border = "black",
     main = "Biểu đồ tần suất - Phương pháp mới",
     xlab = "Thời gian sống sót(tháng)",
     ylab = "Tần suất")
lines(density(Moi), col = "red", lwd = 2)

#So sánh trung bình
mean_coBan <- mean(coBan)
mean_Moi <- mean(Moi)
cat("Trung bình thời gian sống sót:\n")
cat("Phương pháp cơ bản:", mean_coBan, "\n")
cat("Phương pháp mới:", mean_Moi, "\n")
if (mean_Moi > mean_coBan) {
  cat("Phương pháp MỚI có thời gian sống sót TRUNG BÌNH cao hơn.\n")
} else {
  cat("Phương pháp CƠ BẢN có thời gian sống sót TRUNG BÌNH cao hơn.\n")
}

# Câu b:
all <- c(coBan, Moi)
nhan <- c(rep("Cơ bản", length(coBan)), rep("Mới", length(Moi)))

library(ggplot2)
df <- data.frame(
  ThoiGian = all,
  PhuongPhap = nhan
)

ggplot(df, aes(x = ThoiGian, fill = PhuongPhap)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 10, color = "white") +
  labs(
    title = "So sánh tần số thời gian sống sót",
    x = "Thời gian sống sót (tháng)",
    y = "Tần số",
    fill = "Phương pháp"
  ) +
  scale_fill_manual(values = c("Cơ bản" = "pink", "Mới" = "skyblue")) +
  theme_minimal(base_size = 14)

ggplot(df, aes(x = ThoiGian, fill = PhuongPhap, color = PhuongPhap)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "So sánh tần suất thời gian sống sót",
    x = "Thời gian sống sót (tháng)",
    y = "Tần suất",
    fill = "Phương pháp",
    color = "Phương pháp"
  ) +
  scale_fill_manual(values = c("Cơ bản" = "pink", "Mới" = "skyblue")) +
  theme_minimal(base_size = 14)


#Exercise 11

data <- c(55, 85, 90, 50, 110, 115, 75, 85, 8, 23,
          70, 65, 50, 60, 90, 90, 55, 70, 5, 31)

# Câu a:

my_mean <- function(x) {
  tong <- sum(x)
  n <- length(x)
  return(tong / n)
}

my_median <- function(x) {
  n <- length(x)
  x_sorted <- sort(x)
  if (n %% 2 == 1) { 
    return(x_sorted[(n + 1) / 2])
  } else {
    return((x_sorted[n / 2] + x_sorted[n / 2 + 1]) / 2)
  }
}

cat("Trung binh =", my_mean(data), "\n")
cat("Trung vi =", my_median(data), "\n")

# Câu b:

freq_data <- table(data)
print(freq_data)

mode_value <- as.numeric(names(freq_data)[freq_data == max(freq_data)])
cat("Mode = ", mode_value, "\n")

# Câu c:
data_new <- data
data_new[data_new == 110] <- 345
data_new[data_new == 115] <- 467

cat("\nSau khi thay 110 = 345 và 115 = 467:\n")
cat("Trung binh moi =", my_mean(data_new), "\n")
cat("Trung vi moi =", my_median(data_new), "\n")

freq_data_new <- table(data_new)
mode_value_new <- as.numeric(names(freq_data_new)[freq_data_new == max(freq_data_new)])
cat("Mode moi = ", mode_value_new, "\n")

cat("\nNhan xet:\n")
cat("Sau khi thay cac gia tri lon bat thuong (345, 467), trung binh tang manh do bi anh huong boi gia tri ngoai le.\n")
cat("Trung vi va mode it thay doi, vi chung phu thuoc vao vi tri va tan so hon la do lon gia tri.\n")

