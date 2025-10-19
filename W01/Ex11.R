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
