library(dplyr)
library(ggplot2)
library(brms)
library(rstan)

# rstan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(brms.backend = "rstan")

# --------------------------------------------------
# 1. Build candidate feature table from df_feat
# --------------------------------------------------
cand_data <- df_feat %>%
  select(
    Timestamp,
    Price,
    R_lag1_s, R_lag2_s, R_lag3_s,
    RollVol_10_s,
    Volume24h_s,
    VolumePctChange_s,
    Open24h_s,
    OpenCloseDiff_s,
    OpenCloseDiff_pct_s,
    HighGap_pct_s,
    LowGap_pct_s,
    Range24h_s,
    Range24h_pct_s,
    t_index_s
  ) %>%
  na.omit()

# --------------------------------------------------
# 2. Correlation matrix for screening
# --------------------------------------------------
num_data <- cand_data %>%
  select(-Timestamp)

cor_mat <- cor(num_data, use = "complete.obs")

print(round(cor_mat, 3))

# correlations with Price only
price_corr <- sort(cor_mat[, "Price"], decreasing = FALSE)
print(round(price_corr, 3))

# optional: show absolute correlations with Price
price_corr_abs <- sort(abs(cor_mat[, "Price"]), decreasing = TRUE)
print(round(price_corr_abs, 3))

# --------------------------------------------------
# 3. Choose a smaller set of predictors
#    Keep 2 lagged features + strongest non-redundant variables
# --------------------------------------------------
# Recommended starting set:
#   R_lag1_s, R_lag2_s,
#   RollVol_10_s,
#   Volume24h_s,
#   OpenCloseDiff_pct_s,
#   t_index_s
#
# You can swap one of these out after checking correlations.

model_data_small <- cand_data %>%
  select(
    Timestamp,
    Price,
    R_lag1_s,
    R_lag2_s,
    RollVol_10_s,
    Volume24h_s,
    OpenCloseDiff_pct_s,
    t_index_s
  ) %>%
  na.omit()

# --------------------------------------------------
# 4. Baseline Gaussian-Gaussian model in Stan
# --------------------------------------------------
y <- model_data_small$Price
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

# --------------------------------------------------
# 5. Smaller GP + Student-t model
# --------------------------------------------------
fit_gp_small <- brm(
  formula = bf(
    Price ~ gp(
      R_lag1_s,
      Volume24h_s,
      OpenCloseDiff_pct_s,
      t_index_s
    )
  ),
  data = model_data_small,
  family = student(),
  chains = 4,
  cores = 4,
  iter = 1200,
  warmup = 600,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  refresh = 100
)

summary(fit_gp_small)

# --------------------------------------------------
# 6. Fitted values plot
# --------------------------------------------------
model_data_small$fitted_gp <- fitted(fit_gp_small)[, "Estimate"]

ggplot(model_data_small, aes(x = Timestamp)) +
  geom_line(aes(y = Price), linewidth = 0.7) +
  geom_line(aes(y = fitted_gp), linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Observed Price vs Smaller GP Fitted Price",
    x = "Timestamp",
    y = "BTC Price"
  ) +
  theme_minimal()

# --------------------------------------------------
# 7. Posterior predictive check
# --------------------------------------------------
pp_check(fit_gp_small)

# --------------------------------------------------
# 8. One-step-ahead prediction
# --------------------------------------------------
newdata_last <- model_data_small %>%
  slice(n()) %>%
  select(
    R_lag1_s,
    R_lag2_s,
    RollVol_10_s,
    Volume24h_s,
    OpenCloseDiff_pct_s,
    t_index_s
  )

predict(fit_gp_small, newdata = newdata_last)