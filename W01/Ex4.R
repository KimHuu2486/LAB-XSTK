#EX4
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