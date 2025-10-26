#Xác suất độc lập

n <- 100000
die_coin <- data.frame(
  die = sample(1: 6, n, replace = TRUE),
  coin = sample(c("H", "T"), n, replace = TRUE)
)
whr <- with (die_coin, die == 6 & coin == "H")
proportions(table(whr))

#

deck <- expand.grid(
  rank = c('A', 2:10, 'J', 'Q', 'K'),
  suit = c('hearts', 'diamonds', 'clubs', 'spades')
)
n <- 100000
two_cards <- sapply(1:n, FUN = function(x) sample(1:52, 2, replace = FALSE))
whr <- deck[two_cards[1, ], 'suit'] %in% c('diamonds', 'hearts') &
  deck[two_cards[2, ], 'suit'] %in% c('diamonds', 'hearts')
proportions(table(whr))

#

n <- 100000
die_rolls <- sample(1:6, n, replace = TRUE)
whr <- die_rolls %in% c(2,5)
proportions(table(whr))

#
library(ggplot2)

compute_prop <- function(n) {
  # Die rolls
  die_rolls <- sample(1:6, size = n, replace = TRUE)
  whr1 <- die_rolls %in% c(2, 5)
  emp_prop1 <- mean(whr1)
  # Card draws (with replacement)
  deck <- expand.grid(
    rank = c('A', 2:10, 'J', 'Q', 'K'),
    suit = c('Club', 'Diamond', 'Heart', 'Spade'),
    stringsAsFactors = FALSE
  )
  one_card <- sample(1:52, size = n, replace = TRUE)
  suit_is_club <- deck[one_card, 'suit'] == 'Club'
  rank_is_king <- deck[one_card, 'rank'] == 'K'
  whr2 <- suit_is_club | rank_is_king
  emp_prop2 <- mean(whr2)
  return(c(die = emp_prop1, card = emp_prop2))
}
sample_size <- c(100, 200, 500, 1000, 10000, 100000, 1e6)
res_mat <- sapply(sample_size, compute_prop)
df_die <- data.frame(n = sample_size, prop = as.numeric(res_mat["die", ]))
df_card <- data.frame(n = sample_size, prop = as.numeric(res_mat["card", ]))
# Theoretical probs
p_die_true <- 2/6 # = 1/3
p_card_true <- 4/13 # P(Club | King) = 1/4 + 1/13 - 1/52 = 4/13
p1 <- ggplot(df_die, aes(x=n, y=prop)) +
  geom_line(color='blue', linewidth=0.5) +
  geom_point(size=1) +
  geom_hline(yintercept=1/3, linetype='dashed', color='red') +
  scale_x_continuous(trans = "log10", breaks = sample_size) +
  ylim(0, 1) +
  labs(
    title = "Empirical Proportion: Die event {2 or 5}",
    x = "Sample size (log scale)",
    y = "Empirical proportion"
  )
p2 <- ggplot(df_card, aes(x = n, y = prop)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_hline(yintercept = p_card_true, linetype = 'dashed', color = 'red') +
  scale_x_continuous(trans = "log10", breaks = sample_size) +
  ylim(0, 1) +
  labs(
    title = "Empirical Proportion: Card event {Club or King}",
    x = "Sample size (log scale)",
    y = "Empirical proportion"
  )
p1 / p2

