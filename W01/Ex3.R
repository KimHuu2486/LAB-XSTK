# EX3
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