# Test suite for modeling functions

library(testthat)
library(rstatportfolio)

# Sample data for testing
set.seed(123)
sample_data <- data.frame(
  x1 = rnorm(50, 10, 2),
  x2 = rnorm(50, 20, 5),
  x3 = sample(c("A", "B", "C"), 50, replace = TRUE),
  y = numeric(50)
)

# Create a linear relationship
sample_data$y <- 2 * sample_data$x1 - 0.5 * sample_data$x2 + rnorm(50, 0, 3)

# Create binary target for classification
sample_data$y_class <- factor(ifelse(sample_data$y > median(sample_data$y), "high", "low"))

test_that("check_required_packages works", {
  expect_true(check_required_packages("linear"))
  expect_true(check_required_packages("logistic"))
})

test_that("build_regression_models runs without errors", {
  skip_if_not_installed("randomForest")
  
  # Test with minimal parameters
  expect_no_error(
    model_results <- build_regression_models(
      sample_data, 
      target = "y", 
      predictors = c("x1", "x2"),
      model_types = c("linear")
    )
  )
  
  # Check if results have expected structure
  expect_true("models" %in% names(model_results))
  expect_true("performance" %in% names(model_results))
  expect_true("variable_importance" %in% names(model_results))
  expect_true("comparison" %in% names(model_results))
})

test_that("build_single_model works for linear regression", {
  # Split data
  set.seed(123)
  test_indices <- sample(1:nrow(sample_data), 10)
  train_data <- sample_data[-test_indices, ]
  test_data <- sample_data[test_indices, ]
  
  # Test linear model
  model_result <- build_single_model(
    train_data = train_data,
    test_data = test_data,
    target = "y",
    predictors = c("x1", "x2"),
    model_type = "linear"
  )
  
  # Check if results have expected structure
  expect_true("model" %in% names(model_result))
  expect_true("performance" %in% names(model_result))
  expect_true("variable_importance" %in% names(model_result))
  expect_true("test_predictions" %in% names(model_result))
  
  # Check if model is of correct class
  expect_s3_class(model_result$model, "lm")
})

test_that("build_classification_models runs without errors", {
  skip_if_not_installed("randomForest")
  
  # Test with minimal parameters
  expect_no_error(
    model_results <- build_classification_models(
      sample_data, 
      target = "y_class", 
      predictors = c("x1", "x2"),
      model_types = c("logistic")
    )
  )
  
  # Check if results have expected structure
  expect_true("models" %in% names(model_results))
  expect_true("performance" %in% names(model_results))
  expect_true("variable_importance" %in% names(model_results))
  expect_true("comparison" %in% names(model_results))
})

test_that("build_single_classification_model works for logistic regression", {
  # Split data
  set.seed(123)
  test_indices <- sample(1:nrow(sample_data), 10)
  train_data <- sample_data[-test_indices, ]
  test_data <- sample_data[test_indices, ]
  
  # Test logistic model
  model_result <- build_single_classification_model(
    train_data = train_data,
    test_data = test_data,
    target = "y_class",
    predictors = c("x1", "x2"),
    model_type = "logistic"
  )
  
  # Check if results have expected structure
  expect_true("model" %in% names(model_result))
  expect_true("performance" %in% names(model_result))
  expect_true("variable_importance" %in% names(model_result))
  expect_true("test_predictions" %in% names(model_result))
  
  # Check if model is of correct class
  expect_s3_class(model_result$model, "glm")
})

# Note: Advanced models like random_forest and xgboost are not tested here
# as they require additional packages that might not be available in all environments

