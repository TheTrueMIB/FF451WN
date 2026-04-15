install.packages("rstan")
install.packages("brms")
install.packages(c("data.table","dplyr","ggplot2"))

library(rstan)
library(brms)
library(data.table)
library(dplyr)
library(ggplot2)

# rstan settings (VERY important)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


df_raw <- fread("C:/Users/Jackw/OneDrive/桌面/451 Bayesian/final project/btc_usdt_live_prices.csv")

df_raw <- df_raw %>%
  rename(
    Timestamp = 1,
    Price = 2
  ) %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, tz = "UTC"),
    Price = as.numeric(Price)
  ) %>%
  arrange(Timestamp)

# take last 200 obs
df <- df_raw %>%
  tail(200) %>%
  mutate(t_index = row_number())



df_lag <- df %>%
  mutate(
    P_lag1 = lag(Price, 1),
    P_lag2 = lag(Price, 2),
    P_lag3 = lag(Price, 3),
    P_lag4 = lag(Price, 4),
    P_lag5 = lag(Price, 5)
  ) %>%
  na.omit()

# standardize for GP
lag_mat <- scale(df_lag[, c("P_lag1","P_lag2","P_lag3","P_lag4","P_lag5")])

df_lag$P_lag1_s <- lag_mat[,1]
df_lag$P_lag2_s <- lag_mat[,2]
df_lag$P_lag3_s <- lag_mat[,3]
df_lag$P_lag4_s <- lag_mat[,4]
df_lag$P_lag5_s <- lag_mat[,5]




y <- df$Price
N <- length(y)

sigma_known <- sd(y)
mu0 <- mean(y)
tau0 <- 5 * sigma_known

stan_data <- list(
  N = N,
  y = as.vector(y),
  sigma = sigma_known,
  mu0 = mu0,
  tau0 = tau0
)

fit_baseline <- stan(
  file = "C:/Users/Jackw/OneDrive/桌面/451 Bayesian/final project/gg_price_baseline.stan",
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 123
)

print(fit_baseline, pars = c("mu","y_next"), digits = 4)


# IMPORTANT: force brms to use rstan backend
options(brms.backend = "rstan")

fit_gp <- brm(
  Price ~ gp(P_lag1_s, P_lag2_s, P_lag3_s, P_lag4_s, P_lag5_s),
  data = df_lag,
  family = student(),   # heavy-tailed likelihood
  chains = 4,
  cores = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123,
  control = list(adapt_delta = 0.95)
)

summary(fit_gp)







