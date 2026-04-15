library(data.table)
library(dplyr)

# -----------------------------
# 1. Read and clean data
# -----------------------------
df_raw <- fread("C:/Users/Jackw/OneDrive/桌面/451 Bayesian/final project/btc_usdt_live_prices.csv")

df <- df_raw %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, tz = "UTC"),
    Price = as.numeric(Price),
    Volume24h = as.numeric(Volume24h),
    High24h = as.numeric(High24h),
    Low24h = as.numeric(Low24h),
    Open24h = as.numeric(Open24h)
  ) %>%
  arrange(Timestamp) %>%
  distinct(Timestamp, .keep_all = TRUE) %>%
  mutate(t_index = row_number())

# -----------------------------
# 2. Create return-based features
# -----------------------------
df_feat <- df %>%
  mutate(
    Return = log(Price / lag(Price)),
    R_lag1 = lag(Return, 1),
    R_lag2 = lag(Return, 2),
    R_lag3 = lag(Return, 3)
  )

# -----------------------------
# 3. Rolling volatility (last 10 returns)
#    done with dplyr::lag inside rowwise logic
# -----------------------------
df_feat <- df_feat %>%
  mutate(
    RollVol_10 = sapply(seq_along(Return), function(i) {
      if (i < 10) {
        NA_real_
      } else {
        sd(Return[(i-9):i], na.rm = TRUE)
      }
    })
  )

# -----------------------------
# 4. Features we can derive from 24h fields
# -----------------------------
df_feat <- df_feat %>%
  mutate(
    Range24h = High24h - Low24h,
    Range24h_pct = (High24h - Low24h) / Open24h,
    OpenCloseDiff = Price - Open24h,
    OpenCloseDiff_pct = (Price - Open24h) / Open24h,
    HighGap_pct = (High24h - Price) / Price,
    LowGap_pct = (Price - Low24h) / Price,
    VolumeChange = Volume24h - lag(Volume24h),
    VolumePctChange = (Volume24h - lag(Volume24h)) / lag(Volume24h)
  )

# -----------------------------
# 5. Safe scaling function
#    avoids NA when a column is constant
# -----------------------------
safe_scale <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    rep(0, length(x))
  } else {
    (x - m) / s
  }
}

# -----------------------------
# 6. Standardize predictors for GP
# -----------------------------
df_feat <- df_feat %>%
  mutate(
    R_lag1_s = safe_scale(R_lag1),
    R_lag2_s = safe_scale(R_lag2),
    R_lag3_s = safe_scale(R_lag3),
    RollVol_10_s = safe_scale(RollVol_10),
    
    Volume24h_s = safe_scale(Volume24h),
    VolumePctChange_s = safe_scale(VolumePctChange),
    
    Open24h_s = safe_scale(Open24h),
    OpenCloseDiff_s = safe_scale(OpenCloseDiff),
    OpenCloseDiff_pct_s = safe_scale(OpenCloseDiff_pct),
    
    HighGap_pct_s = safe_scale(HighGap_pct),
    LowGap_pct_s = safe_scale(LowGap_pct),
    
    Range24h_s = safe_scale(Range24h),
    Range24h_pct_s = safe_scale(Range24h_pct),
    
    t_index_s = safe_scale(t_index)
  )

# -----------------------------
# 7. Final modeling dataset
#    keep response as Price
# -----------------------------
gp_data <- df_feat %>%
  select(
    Timestamp,
    Price,
    R_lag1_s, R_lag2_s, R_lag3_s,
    RollVol_10_s,
    Volume24h_s, VolumePctChange_s,
    Open24h_s,
    OpenCloseDiff_s, OpenCloseDiff_pct_s,
    HighGap_pct_s, LowGap_pct_s,
    Range24h_s, Range24h_pct_s,
    t_index_s
  ) %>%
  na.omit()

# -----------------------------
# 8. Quick checks
# -----------------------------
glimpse(gp_data)
summary(gp_data)
colSums(is.na(gp_data))
head(gp_data)