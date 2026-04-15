library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


btc_usdt_live_prices <- read_csv("C:/Users/Jackw/OneDrive/桌面/451 Bayesian/final project/btc_usdt_live_prices.csv")



btc_usdt_live_prices <- btc_usdt_live_prices[order(btc_usdt_live_prices$Timestamp), ]

# make sure Price is numeric
btc_usdt_live_prices$Price <- as.numeric(btc_usdt_live_prices$Price)

# keep only valid rows
btc_usdt_live_prices <- btc_usdt_live_prices[!is.na(btc_usdt_live_prices$Price), ]

# optional: only use the most recent 200 observations
price <- tail(btc_usdt_live_prices$Price, 200)

# quick diagnostics
summary(price)
sd(price)
range(price)
length(price)

# known sigma plug-in
sigma_known <- sd(price)

# prior centered at sample mean
mu0 <- mean(price)

# prior sd: make it somewhat weak relative to data scale
tau0 <- 10 * sigma_known

stan_data <- list(
  N = length(price),
  y = as.vector(price),
  mu0 = mu0,
  tau0 = tau0,
  sigma = sigma_known
)

fit <- stan(
  file = "C:/Users/Jackw/OneDrive/桌面/451 Bayesian/final project/GG.stan",
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

