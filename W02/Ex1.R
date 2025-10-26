library(ggplot2)

prob_theoretical <- function(n) {
  1 - prod(365:(365 - (n - 1))) / (365^n)
}

sim_birthday <- function(n, m = 1000, days = 365) {
  mean(replicate(m, {
    birthdays <- sample.int(days, n, replace = TRUE)
    any(duplicated(birthdays))
  }))
}

n_values <- 1:100
df <- data.frame(
  n = n_values,
  Theoretical = sapply(n_values, prob_theoretical),
  Simulated = sapply(n_values, sim_birthday)
)

ggplot(df_long, aes(x = n, y = Probability, color = Type)) +
  geom_line(size = 1) +
  labs(
    title = "Bài toán ngày sinh (Birthday Problem)",
    x = "Số người trong nhóm (n)",
    y = "Xác suất có ít nhất hai người cùng ngày sinh",
    color = "Loại xác suất"
  )