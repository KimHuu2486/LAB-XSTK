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

