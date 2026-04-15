fit_final <- brm(
  Price ~ R_lag1_s + R_lag2_s + Volume24h_s + OpenCloseDiff_pct_s + t_index_s,
  data = model_data_small,
  family = student(),
  chains = 2,
  cores = 2,
  iter = 1200,
  warmup = 600,
  seed = 123
)


summary(fit_final)

fit_final2 <- brm(
  Price ~ R_lag1_s + R_lag2_s + Volume24h_s + OpenCloseDiff_pct_s + t_index_s,
  data = model_data_small,
  family = gaussian(),
  chains = 2,
  cores = 2,
  iter = 1200,
  warmup = 600,
  seed = 123
)


summary(fit_final2)

# Trace + density
plot(fit_final)
plot(fit_final2)

res <- residuals(fit_final)

hist(res[, "Estimate"], breaks = 30, main = "Residuals Histogram")


yrep_t  <- posterior_predict(fit_final, ndraws = 100)
yrep_g  <- posterior_predict(fit_final2, ndraws = 100)

plot(yrep_t)
plot(yrep_g)
