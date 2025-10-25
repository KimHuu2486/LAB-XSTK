#Ex9
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