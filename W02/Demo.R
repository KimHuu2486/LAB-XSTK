library(ggplot2)
library(dplyr)
library(patchwork)

## Birthday problem 
# Part (a)
sample_size <- c(30, 50, 100)

props_zero <- sapply(sample_size, function(n) prod(365:(365-(n-1))) / (365^n))

props <- 1-props_zero

df <- data.frame(n=sample_size, emp_prop=props)

p1 <- ggplot(df, aes(n, emp_prop)) + 
  geom_line(color='blue', linewidth=0.5) + 
  geom_point(size=1) + 
  scale_x_continuous(breaks = sample_size) +
  ylim(0, 1) +
  labs(
    title = "thực nghiệm Proportion: Birthday problem",
    x = "Sample size",
    y = "thực nghiệm proportion"
  )

# Part (b)
exact_birthday_problem <- function(n){
  1-prod(365:(365-(n-1))) / (365^n)
}

simulate_birthday_problem <- function(n_people, n_sim) {
  num_matches <- 0
  for (i in 1:n_sim) {
    birthdays <- sample(1:365, n_people, replace = TRUE)
    # here we can use any(duplicated(birthdays))
    if (length(birthdays) != length(unique(birthdays))) {
      num_matches <- num_matches + 1
    }
  }
  prop <- num_matches / n_sim
  return(prop)
}

simulate_birthday_problem(23, 100000)

exact_birthday_problem(23)

n_people <- 23

sample_size <- c(100, 250, 500, 1000, 100000)

props <- sapply(sample_size, simulate_birthday_problem, n_people=n_people)

df <- data.frame(n=sample_size, emp_prop=props)

p2 <- ggplot(df, aes(n, emp_prop)) + 
  geom_line(color='blue', linewidth=0.5) + 
  geom_point(size=1) + 
  geom_hline(yintercept = exact_birthday_problem(n_people), 
             linetype="dashed", color='red') + 
  scale_x_continuous(trans="log10", breaks = sample_size) +
  ylim(0, 1) +
  labs(
    title = paste0("thực nghiệm Proportion for ", n_people),
    x = "Sample size",
    y = "thực nghiệm proportion"
  )

p1 / p2


## Rolling dice 
set.seed(123)
n_sims <- c(100, 1000, 10000)
results_dice <- data.frame()

for (n_sim in n_sims) {
  dice_rolls <- sample(1:6, size = n_sim, replace = TRUE)
  emp_mean <- mean(dice_rolls)
  emp_var <- var(dice_rolls)
  results_dice <- rbind(results_dice, data.frame(
    n_sim = n_sim,
    emp_mean = emp_mean,
    emp_var = emp_var
  ))
}

# or a tidyverse-like version
# results_dice <- tibble(n_sim = n_sims) |>
#   mutate(
#     rolls = map(n_sim, ~ sample(1:6, size = .x, replace = TRUE)),
#     emp_mean = map_dbl(rolls, mean),
#     emp_var = map_dbl(rolls, var)
#   ) |>
#   select(-rolls)

# or a apply version
# dice_rolls <- lapply(n_sims, function(n) sample(1:6, size = n, replace = TRUE))
# 
# emp_mean <- vapply(dice_rolls, mean, numeric(1))
# emp_var  <- vapply(dice_rolls, var,  numeric(1))
# 
# results_dice <- data.frame(
#   n_sim = n_sims,
#   emp_mean = emp_mean,
#   emp_var = emp_var
# )

theo_mean_dice <- 3.5
theo_var_dice <- (35/12)

p1 <- ggplot(results_dice, aes(x = n_sim)) +
  geom_line(aes(y = emp_mean, color = "Mean"), size = 1) +
  geom_line(aes(y = emp_var, color = "Variance"), size = 1) +
  geom_hline(yintercept = theo_mean_dice, linetype = "dashed", color = "red") +
  geom_hline(yintercept = theo_var_dice, linetype = "dashed", color = "blue") +
  scale_x_continuous(trans = "log10", breaks = n_sim)
labs(title = "Hội tụ của Mean và Var (Xúc xắc)",
     x = "log10(n_sim)", y = "Giá trị",
     color = "Thống kê") +
  scale_color_manual(values = c("Mean" = "red", "Variance" = "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Part 2: binomial with p=0.5
n <- 10
p <- 0.5
n_sim_binom <- 10000
binom_samples <- rbinom(n_sim_binom, n, p)

# thực nghiệm PMF
emp_pmf <- sapply(0:n, function(k) mean(binom_samples == k))
theo_pmf <- dbinom(0:n, n, p)

pmf_data <- data.frame(
  k = rep(0:n, 2),
  prob = c(emp_pmf, theo_pmf),
  type = rep(c("thực nghiệm", "Theoretical"), each = n+1)
)

p2 <- ggplot(pmf_data, aes(x = factor(k), y = prob, fill = type)) +
  geom_col(position = "dodge", alpha = 0.7) +
  labs(title = "PMF thực nghiệm vs. Lý thuyết (Binom n=10, p=0.5)",
       x = "k", y = "Xác suất",
       fill = "Loại") +
  scale_fill_manual(values = c("thực nghiệm" = "skyblue", "Theoretical" = "darkgreen")) +
  theme_bw() +
  theme(legend.position = "bottom")


p1 / p2


## conditional probability of rolling dice
rolls <- sample(1:6, size = 1000000, replace = TRUE)
P_A <- mean(rolls %% 2 == 0)             # Even
P_B <- mean(rolls > 3)                   # Greater than 3
P_A_and_B <- mean(rolls %% 2 == 0 & rolls > 3)
P_A_given_B <- P_A_and_B / P_B

P_A_given_B



## Disease testing 
# P(D=1) = 0.01
# Sensitivity: P(+ | D=1) = 0.95
# False positive rate: P(+ | D=0) = 0.10
# P(D=1 | + ) = P(+ | D=1) x P(D=1) / P(+)

n <- 1e6
p <- 0.01 

disease <- sample(0:1, size = n, replace = TRUE, prob = c(1 - p, p))

# Use 10% false positives for healthy, 95% positives for diseased
test <- ifelse(disease == 1,
               sample(0:1, size = n, replace = TRUE, prob = c(0.05, 0.95)),
               sample(0:1, size = n, replace = TRUE, prob = c(0.90, 0.10)))

mean(test == 1)             
mean(disease[test == 1])  


## Choosing dice and play game 
n <- 1e6

# which die (1 = fair, 2 = biased)
dice_choice <- sample(c(1, 2), size = n, replace = TRUE, prob = c(0.3, 0.7))

# Roll according to chosen die
roll <- numeric(n)

# Fair die
fair_faces <- 1:6
biased_faces <- 1:6
# Biased dice
biased_probs <- c(rep(0.1, 5), 0.5)

roll[dice_choice == 1] <- sample(fair_faces, sum(dice_choice == 1), replace = TRUE)
roll[dice_choice == 2] <- sample(biased_faces, sum(dice_choice == 2), replace = TRUE, prob = biased_probs)

mean(roll == 6)




## Rain and bring umbrella now 
n <- 10000

# Rain indicator (1 = rain, 0 = no rain)
rain <- sample(0:1, size = n, replace = TRUE, prob = c(0.7, 0.3))

# Umbrella decision depends on rain
umbrella <- ifelse(rain == 1,
                   sample(0:1, size = n, replace = TRUE, prob = c(0.1, 0.9)),  # bring umbrella if raining
                   sample(0:1, size = n, replace = TRUE, prob = c(0.8, 0.2)))  # bring umbrella if no rain

mean(umbrella)


## Rolling dice and play game 
n <- 100000
die <- sample(1:6, n, replace = TRUE)
win <- ifelse(die <= 3,
              sample(0:1, n, prob=c(0.6, 0.4), replace=T),
              sample(0:1, n, prob=c(0.3, 0.7), replace=T))
mean(win)


set.seed(42)
n <- 10000
die <- sample(1:6, n, replace = TRUE)
win <- ifelse(die < 3,
              rbinom(n, 1, 0.4),  # Game A
              rbinom(n, 1, 0.7))  # Game B
mean(win)


## Alice and Bob games 
simulate_game <- function(n) {
  die <- sample(1:6, n, replace = TRUE)
  win_alice <- win_bob <- logical(n)
  
  # Group by condition
  idx_alice <- die <= 3
  idx_bob   <- die > 3
  
  # Alice's fair roll
  roll_alice <- sample(1:6, sum(idx_alice), replace = TRUE)
  win_alice[idx_alice] <- roll_alice %% 2 == 0
  
  # Bob's biased roll
  roll_bob <- sample(1:6, sum(idx_bob), replace = TRUE,
                     prob = c(rep(0.1,5), 0.5))
  win_bob[idx_bob] <- roll_bob == 6
  
  # Empirical probabilities
  c(
    Alice = mean(win_alice),
    Bob   = mean(win_bob),
    Any   = mean(win_alice | win_bob)
  )
}



## Factory, workshops and defective items 
p_workshop   <- c(0.4, 0.3, 0.3)   # production shares
p_good_given <- c(0.94, 0.96, 0.95)
p_def_given  <- 1 - p_good_given   # defect probabilities

n <- 1e6  # number of simulated products
labs <- c("I", "II", "III")

# Randomly choose workshop for each item
workshop <- sample(labs, size = n, replace = TRUE, prob = p_workshop)

# For each group, assign whether defective
is_def <- logical(n) 

idx1 <- workshop == "I"
idx2 <- workshop == "II"
idx3 <- workshop == "III"

is_def[idx1] <- sample(c(0,1), size = sum(idx1), replace = TRUE,
                       prob = c(1 - p_def_given[1], p_def_given[1]))
is_def[idx2] <- sample(c(0,1), size = sum(idx2), replace = TRUE,
                       prob = c(1 - p_def_given[2], p_def_given[2]))
is_def[idx3] <- sample(c(0,1), size = sum(idx3), replace = TRUE,
                       prob = c(1 - p_def_given[3], p_def_given[3]))

# Estimate probabilities
P_G_hat <- mean(!is_def)  # estimated good rate
P_D_hat <- mean(is_def)   # estimated defect rate

# proportion of defective items from each workshop
tab_def  <- table(workshop[is_def])
post_hat <- as.numeric(tab_def) / sum(tab_def)
names(post_hat) <- names(tab_def)

round(P_G_hat, 4)
round(P_D_hat, 4)
print(round(post_hat, 4))

