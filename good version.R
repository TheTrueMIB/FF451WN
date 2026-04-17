# =========================================================
# BTC Bayesian Regression Project: Final Clean Version
# with correlation screening + frequentist comparison
# =========================================================

library(brms)
library(data.table)
library(dplyr)
library(ggplot2)
library(glmnet)

options(mc.cores = parallel::detectCores())
options(brms.backend = "rstan")

# =========================================================
# 1. READ DATA
# =========================================================

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

# local short window
df <- df_raw %>%
  tail(200) %>%
  mutate(t_index = row_number())

# =========================================================
# 2. FEATURE ENGINEERING
# =========================================================

df2 <- df %>%
  mutate(
    R = c(NA, diff(Price)),
    R_lag1 = lag(R, 1),
    R_lag2 = lag(R, 2),
    R_lag3 = lag(R, 3),
    AbsReturn = abs(R),
    OpenCloseDiff_pct = 100 * R / lag(Price, 1),
    Price_lag1 = lag(Price, 1),
    Price_lag2 = lag(Price, 2),
    Price_lag3 = lag(Price, 3)
  ) %>%
  na.omit()

# =========================================================
# 3. STANDARDIZE CANDIDATE PREDICTORS
# =========================================================

model_data_full <- df2 %>%
  mutate(
    R_lag1_s = as.numeric(scale(R_lag1)),
    R_lag2_s = as.numeric(scale(R_lag2)),
    R_lag3_s = as.numeric(scale(R_lag3)),
    AbsReturn_s = as.numeric(scale(AbsReturn)),
    OpenCloseDiff_pct_s = as.numeric(scale(OpenCloseDiff_pct)),
    Price_lag1_s = as.numeric(scale(Price_lag1)),
    Price_lag2_s = as.numeric(scale(Price_lag2)),
    Price_lag3_s = as.numeric(scale(Price_lag3)),
    t_index_s = as.numeric(scale(t_index))
  )

# =========================================================
# 4. CORRELATION SCREENING FOR PREDICTORS
# =========================================================

candidate_predictors <- model_data_full %>%
  select(
    R_lag1_s, R_lag2_s, R_lag3_s,
    AbsReturn_s, OpenCloseDiff_pct_s,
    Price_lag1_s, Price_lag2_s, Price_lag3_s,
    t_index_s
  )

cor_mat <- cor(candidate_predictors, use = "complete.obs")
print(round(cor_mat, 3))

# visualize if you want
corrplot_data <- as.data.frame(as.table(cor_mat))
colnames(corrplot_data) <- c("Var1", "Var2", "Correlation")

ggplot(corrplot_data, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Predictor Correlation Matrix", x = "", y = "")

# =========================================================
# 5. DROP HIGHLY CORRELATED PREDICTORS
# =========================================================
# Rule:
# if |correlation| > 0.85, remove one of the two variables
# Keep variables with clearer interpretation

# Example reasoning:
# - Price lags are usually highly correlated with each other
# - OpenCloseDiff_pct is closely tied to returns
# - keep simpler / more interpretable subset

model_data <- model_data_full %>%
  select(
    Price,
    R_lag1_s,
    R_lag2_s,
    AbsReturn_s,
    OpenCloseDiff_pct_s,
    t_index_s
  )

# if OpenCloseDiff_pct_s is too correlated with R_lag1_s or AbsReturn_s in your data,
# you can drop it too and refit with a smaller set.

# =========================================================
# 6. TRAIN / TEST SPLIT
# =========================================================

train_data <- model_data %>% slice(1:(n() - 1))
test_data  <- model_data %>% slice(n())

cat("Real next price =", test_data$Price, "\n")

# =========================================================
# 7. FREQUENTIST OLS + RIDGE
# =========================================================

freq_formula <- Price ~ R_lag1_s + R_lag2_s + AbsReturn_s + OpenCloseDiff_pct_s + t_index_s

lm_fit <- lm(freq_formula, data = train_data)
summary(lm_fit)

x_train <- model.matrix(freq_formula, data = train_data)[, -1]
y_train <- train_data$Price

ridge_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0
)

ridge_fit <- glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,
  lambda = ridge_cv$lambda.min
)

cat("Best ridge lambda =", ridge_cv$lambda.min, "\n")

# =========================================================
# 8. BAYESIAN REGRESSION
# =========================================================

fit_gaussian <- brm(
  formula = freq_formula,
  data = train_data,
  family = gaussian(),
  chains = 2,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

fit_student <- brm(
  formula = freq_formula,
  data = train_data,
  family = student(),
  chains = 2,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

summary(fit_gaussian)
summary(fit_student)

# =========================================================
# 9. RESIDUAL NORMALITY CHECK
# =========================================================

res_lm <- resid(lm_fit)

par(mfrow = c(1, 2))
hist(res_lm, breaks = 20, main = "OLS Residuals", xlab = "Residual")
qqnorm(res_lm, main = "OLS Residual Q-Q Plot")
qqline(res_lm)

if (length(res_lm) >= 3 && length(res_lm) <= 5000) {
  print(shapiro.test(res_lm))
}

# =========================================================
# 10. ONE-STEP-AHEAD PREDICTION
# =========================================================

# OLS
lm_pred <- predict(lm_fit, newdata = test_data, interval = "prediction")

# Ridge
x_test <- model.matrix(freq_formula, data = test_data)[, -1]
ridge_pred <- predict(ridge_fit, newx = x_test)

# Bayesian Gaussian
pred_g <- posterior_predict(fit_gaussian, newdata = test_data)
bayes_g_mean <- mean(pred_g)
bayes_g_ci <- quantile(pred_g, probs = c(0.025, 0.975))

# Bayesian Student-t
pred_t <- posterior_predict(fit_student, newdata = test_data)
bayes_t_mean <- mean(pred_t)
bayes_t_ci <- quantile(pred_t, probs = c(0.025, 0.975))

cat("OLS predicted next price =", lm_pred[1, "fit"], "\n")
cat("Ridge predicted next price =", as.numeric(ridge_pred), "\n")
cat("Bayesian Gaussian predicted next price =", bayes_g_mean, "\n")
cat("Bayesian Gaussian 95% PI =", bayes_g_ci, "\n")
cat("Bayesian Student-t predicted next price =", bayes_t_mean, "\n")
cat("Bayesian Student-t 95% PI =", bayes_t_ci, "\n")
cat("Real next price =", test_data$Price, "\n")

# =========================================================
# 11. COMPARISON TABLE
# =========================================================

comparison_tbl <- data.frame(
  Model = c("OLS", "Ridge", "Bayesian Gaussian", "Bayesian Student-t"),
  Predicted_Next_Price = c(
    lm_pred[1, "fit"],
    as.numeric(ridge_pred),
    bayes_g_mean,
    bayes_t_mean
  ),
  Real_Next_Price = rep(test_data$Price, 4),
  Abs_Error = c(
    abs(lm_pred[1, "fit"] - test_data$Price),
    abs(as.numeric(ridge_pred) - test_data$Price),
    abs(bayes_g_mean - test_data$Price),
    abs(bayes_t_mean - test_data$Price)
  )
)

print(comparison_tbl)

# =========================================================
# 12. PREDICTION COMPARISON PLOT
# =========================================================

ggplot(comparison_tbl, aes(x = Model, y = Predicted_Next_Price)) +
  geom_point(size = 3) +
  geom_hline(yintercept = test_data$Price, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Next-Price Prediction Comparison",
    x = "",
    y = "Predicted Price"
  )

# =========================================================
# 13. COEFFICIENT COMPARISON
# =========================================================

cat("\n===== OLS coefficients =====\n")
print(coef(summary(lm_fit)))

cat("\n===== Bayesian Gaussian coefficients =====\n")
print(fixef(fit_gaussian))

cat("\n===== Bayesian Student-t coefficients =====\n")
print(fixef(fit_student))


# 1. Raw price
shapiro.test(df$Price)

# 2. Returns
shapiro.test(df2$R)

# 3. Residuals (MOST IMPORTANT)
shapiro.test(res_lm)

