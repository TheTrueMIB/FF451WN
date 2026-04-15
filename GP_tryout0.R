library(rstan)
library(brms)
library(dplyr)
library(ggplot2)

# rstan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(brms.backend = "rstan")

# ---------------------------------
# 0. Build final modeling data from df_feat
#    (no reading CSV again)
# ---------------------------------
model_data <- df_feat %>%
  select(
    Timestamp,
    Price,
    R_lag1_s, R_lag2_s, R_lag3_s,
    RollVol_10_s,
    Volume24h_s,
    OpenCloseDiff_pct_s,
    HighGap_pct_s,
    LowGap_pct_s,
    t_index_s
  ) %>%
  na.omit()

# ---------------------------------
# 1. Baseline Gaussian-Gaussian model in Stan
# ---------------------------------
y <- model_data$Price
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

print(fit_baseline, pars = c("mu", "y_next"), digits = 4)

# ---------------------------------
# 2. GP + Student-t likelihood in brms
# ---------------------------------
fit_gp <- brm(
  formula = bf(
    Price ~ gp(
      R_lag1_s, R_lag2_s, R_lag3_s,
      RollVol_10_s,
      Volume24h_s,
      OpenCloseDiff_pct_s,
      HighGap_pct_s,
      LowGap_pct_s,
      t_index_s
    )
  ),
  data = model_data,
  family = student(),
  chains = 4,
  cores = 4,
  iter = 1500,
  warmup = 700,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit_gp)

# ---------------------------------
# 3. Fitted values plot
# ---------------------------------
model_data$fitted_gp <- fitted(fit_gp)[, "Estimate"]

ggplot(model_data, aes(x = Timestamp)) +
  geom_line(aes(y = Price), linewidth = 0.7) +
  geom_line(aes(y = fitted_gp), linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Observed Price vs GP Fitted Price",
    x = "Timestamp",
    y = "BTC Price"
  ) +
  theme_minimal()

# ---------------------------------
# 4. Posterior predictive check
# ---------------------------------
pp_check(fit_gp)

# ---------------------------------
# 5. One-step-ahead prediction
# ---------------------------------
newdata_last <- model_data %>%
  slice(n()) %>%
  select(
    R_lag1_s, R_lag2_s, R_lag3_s,
    RollVol_10_s,
    Volume24h_s,
    OpenCloseDiff_pct_s,
    HighGap_pct_s,
    LowGap_pct_s,
    t_index_s
  )

predict(fit_gp, newdata = newdata_last)