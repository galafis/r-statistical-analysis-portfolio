#' Sample datasets for demonstration and testing
#'
#' This script creates sample datasets that are used in examples and vignettes.
#' These datasets demonstrate various statistical analysis techniques.
#'

# Create a sample regression dataset
set.seed(123)
n <- 1000

# Create predictors
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- rnorm(n, mean = 100, sd = 20)
x3 <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
x4 <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
x5 <- runif(n, min = 0, max = 100)
x6 <- rpois(n, lambda = 5)

# Create response variable with known relationships
y <- 2 * x1 - 0.5 * x2 + 10 * (x3 == "A") - 5 * (x3 == "B") + 
     0.7 * x5 + 2 * x6 + rnorm(n, mean = 0, sd = 15)

# Create binary target
y_binary <- ifelse(y > median(y), 1, 0)

# Create categorical target
y_cat <- cut(y, breaks = 3, labels = c("Low", "Medium", "High"))

# Create dates
dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n)

# Combine into a data frame
regression_data <- data.frame(
  id = 1:n,
  date = dates,
  numeric_var1 = x1,
  numeric_var2 = x2,
  numeric_var3 = x5,
  numeric_var4 = x6,
  categorical_var1 = factor(x3),
  categorical_var2 = factor(x4),
  target_numeric = y,
  target_binary = factor(y_binary, labels = c("No", "Yes")),
  target_categorical = y_cat
)

# Add some missing values
set.seed(456)
missing_indices <- sample(1:n, size = n * 0.05)
regression_data$numeric_var1[missing_indices] <- NA

missing_indices <- sample(1:n, size = n * 0.03)
regression_data$categorical_var1[missing_indices] <- NA

# Create a time series dataset
set.seed(789)
n_ts <- 120  # 10 years of monthly data

# Create time components
time <- 1:n_ts
month <- rep(1:12, n_ts/12)

# Create trend, seasonality, and noise
trend <- 0.2 * time
seasonality <- 15 * sin(2 * pi * month / 12)
noise <- rnorm(n_ts, mean = 0, sd = 5)

# Combine components
ts_values <- 100 + trend + seasonality + noise

# Create dates for time series
ts_dates <- seq(as.Date("2015-01-01"), by = "month", length.out = n_ts)

# Create time series data frame
time_series_data <- data.frame(
  date = ts_dates,
  value = ts_values,
  month = month,
  year = rep(2015:2024, each = 12)
)

# Create a time series object
time_series_ts <- ts(ts_values, start = c(2015, 1), frequency = 12)

# Save the datasets
usethis::use_data(regression_data, overwrite = TRUE)
usethis::use_data(time_series_data, overwrite = TRUE)
usethis::use_data(time_series_ts, overwrite = TRUE)

