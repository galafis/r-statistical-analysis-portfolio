# Test suite for time series functions

library(testthat)
library(rstatportfolio)

# Skip tests if forecast package is not available
skip_if_not_installed("forecast")

# Sample data for testing
set.seed(123)
sample_ts <- stats::ts(rnorm(48, 100, 10) + 1:48 + 10 * sin(2 * pi * 1:48 / 12), 
                      frequency = 12, 
                      start = c(2020, 1))

# Add some trend and seasonality
for (i in 1:4) {
  season <- rep(c(-5, 0, 5, 10, 5, 0, -5, -10, -5, 0, 5, 0), i)
  sample_ts[(i-1)*12 + 1:12] <- sample_ts[(i-1)*12 + 1:12] + season
}

# Sample data frame for testing
sample_df <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "month", length.out = 48),
  value = as.numeric(sample_ts)
)

test_that("convert_to_ts works correctly", {
  # Test with time series input
  ts_result <- convert_to_ts(sample_ts)
  expect_identical(ts_result, sample_ts)
  
  # Test with data frame input
  ts_from_df <- convert_to_ts(sample_df, date_col = "date", value_col = "value", frequency = 12)
  expect_true(stats::is.ts(ts_from_df))
  expect_equal(frequency(ts_from_df), 12)
  expect_equal(length(ts_from_df), 48)
})

test_that("convert_to_ts handles errors", {
  # Test with missing columns
  expect_error(convert_to_ts(sample_df, date_col = "wrong_col", value_col = "value"))
  expect_error(convert_to_ts(sample_df, date_col = "date", value_col = "wrong_col"))
  
  # Test with invalid input
  expect_error(convert_to_ts(list()))
})

test_that("decompose_time_series works", {
  skip_if_not_installed("forecast")
  
  # Test decomposition
  decomp_result <- decompose_time_series(sample_ts)
  
  # Check if results have expected structure
  expect_true("additive" %in% names(decomp_result))
  expect_true("multiplicative" %in% names(decomp_result))
  
  # Check if additive decomposition has expected components
  expect_true("seasonal" %in% names(decomp_result$additive))
  expect_true("trend" %in% names(decomp_result$additive))
  expect_true("random" %in% names(decomp_result$additive))
})

test_that("check_stationarity works", {
  skip_if_not_installed("tseries")
  
  # Test stationarity check
  stat_result <- check_stationarity(sample_ts)
  
  # Check if results have expected structure
  expect_true("is_stationary" %in% names(stat_result))
  expect_true(is.logical(stat_result$is_stationary))
  
  # Check if test results are included when tseries is available
  if (requireNamespace("tseries", quietly = TRUE)) {
    expect_true("adf_test" %in% names(stat_result))
    expect_true("kpss_test" %in% names(stat_result))
  }
})

test_that("calculate_acf_pacf works", {
  # Test ACF/PACF calculation
  acf_result <- calculate_acf_pacf(sample_ts)
  
  # Check if results have expected structure
  expect_true("acf" %in% names(acf_result))
  expect_true("pacf" %in% names(acf_result))
  
  # Check if ACF and PACF have expected attributes
  expect_true("acf" %in% names(acf_result$acf))
  expect_true("lag" %in% names(acf_result$acf))
  expect_true("acf" %in% names(acf_result$pacf))
  expect_true("lag" %in% names(acf_result$pacf))
})

test_that("fit_forecast_model works for ARIMA", {
  skip_if_not_installed("forecast")
  
  # Split data
  train_ts <- window(sample_ts, end = c(2022, 12))
  test_ts <- window(sample_ts, start = c(2023, 1))
  
  # Test ARIMA model
  model_result <- fit_forecast_model(
    ts_data = train_ts,
    model_type = "arima",
    h = length(test_ts)
  )
  
  # Check if results have expected structure
  expect_true("model" %in% names(model_result))
  expect_true("forecast" %in% names(model_result))
  expect_true("accuracy" %in% names(model_result))
  
  # Check if model is of correct class
  expect_s3_class(model_result$model, "Arima")
  
  # Check if forecast has expected length
  expect_equal(length(model_result$forecast$mean), length(test_ts))
})

test_that("analyze_time_series runs without errors", {
  skip_if_not_installed("forecast")
  
  # Test with minimal parameters
  expect_no_error(
    ts_results <- analyze_time_series(
      sample_ts,
      h = 12,
      models = c("arima")
    )
  )
  
  # Check if results have expected structure
  expect_true("data" %in% names(ts_results))
  expect_true("decomposition" %in% names(ts_results))
  expect_true("stationarity" %in% names(ts_results))
  expect_true("acf_pacf" %in% names(ts_results))
  expect_true("models" %in% names(ts_results))
  expect_true("forecasts" %in% names(ts_results))
  
  # Test with data frame input
  expect_no_error(
    ts_results_df <- analyze_time_series(
      sample_df,
      date_col = "date",
      value_col = "value",
      frequency = 12,
      h = 12,
      models = c("arima")
    )
  )
})

# Note: Advanced models like prophet, tbats, and nnetar are not tested here
# as they require additional packages that might not be available in all environments

