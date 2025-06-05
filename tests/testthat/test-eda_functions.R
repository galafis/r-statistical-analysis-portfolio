# Test suite for EDA functions

library(testthat)
library(rstatportfolio)

# Sample data for testing
sample_data <- data.frame(
  id = 1:10,
  category = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  value = rnorm(10, 100, 10),
  date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
  logical = sample(c(TRUE, FALSE), 10, replace = TRUE)
)

sample_data$value[3] <- NA
sample_data$category[5] <- NA

test_that("identify_variable_types works correctly", {
  var_types <- identify_variable_types(sample_data)
  
  expect_equal(var_types$categorical, c("category", "logical"))
  expect_equal(var_types$numerical, c("id", "value"))
  expect_equal(var_types$dates, "date")
})

test_that("perform_eda runs without errors", {
  expect_no_error(perform_eda(sample_data))
  expect_no_error(perform_eda(sample_data, target = "value"))
  expect_no_error(perform_eda(sample_data, id_vars = "id"))
})

test_that("perform_eda handles empty data", {
  empty_data <- data.frame()
  expect_error(perform_eda(empty_data))
})

test_that("analyze_categorical_vars works", {
  cat_analysis <- analyze_categorical_vars(sample_data, c("category", "logical"))
  
  expect_true("category" %in% names(cat_analysis))
  expect_true("logical" %in% names(cat_analysis))
  expect_equal(cat_analysis$category$missing, 1)
})

test_that("analyze_numerical_vars works", {
  num_analysis <- analyze_numerical_vars(sample_data, c("id", "value"))
  
  expect_true("id" %in% names(num_analysis))
  expect_true("value" %in% names(num_analysis))
  expect_equal(num_analysis$value$missing, 1)
})

test_that("analyze_date_vars works", {
  date_analysis <- analyze_date_vars(sample_data, "date")
  
  expect_true("date" %in% names(date_analysis))
  expect_equal(date_analysis$date$min_date, as.Date("2024-01-01"))
  expect_equal(date_analysis$date$max_date, as.Date("2024-01-10"))
})

test_that("analyze_correlations works", {
  cor_matrix <- analyze_correlations(sample_data, c("id", "value"))
  
  expect_true(is.matrix(cor_matrix))
  expect_equal(dim(cor_matrix), c(2, 2))
})

test_that("analyze_target_relationships works", {
  target_analysis <- analyze_target_relationships(sample_data, "value", c("category"), c("id"))
  
  expect_equal(target_analysis$target_type, "numeric")
  expect_true("correlations" %in% names(target_analysis))
  expect_true("anova" %in% names(target_analysis))
})

# Note: Plotting functions are not tested here as they require visual inspection


