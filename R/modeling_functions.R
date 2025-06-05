#' Statistical Modeling Functions
#'
#' @description A collection of functions for statistical modeling and analysis
#' @author Gabriel Demetrios Lafis
#' @name modeling_functions
NULL

#' Build and evaluate multiple regression models
#'
#' @param data A data frame containing the variables
#' @param target The name of the target variable
#' @param predictors Character vector of predictor variable names
#' @param model_types Types of models to build ("linear", "ridge", "lasso", "elastic_net", "random_forest", "xgboost")
#' @param test_size Proportion of data to use for testing (0 to 1)
#' @param seed Random seed for reproducibility
#' @param cv_folds Number of cross-validation folds
#' @param tune_hyperparameters Whether to tune hyperparameters
#'
#' @return A list containing model results and comparisons
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' model_results <- build_regression_models(
#'   mtcars, 
#'   target = "mpg", 
#'   predictors = c("cyl", "disp", "hp", "wt")
#' )
#' }
build_regression_models <- function(data, 
                                   target, 
                                   predictors = NULL,
                                   model_types = c("linear", "random_forest"),
                                   test_size = 0.2,
                                   seed = 42,
                                   cv_folds = 5,
                                   tune_hyperparameters = FALSE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!target %in% names(data)) {
    stop("Target variable not found in data")
  }
  
  if (!is.numeric(data[[target]])) {
    stop("Target variable must be numeric for regression")
  }
  
  # If predictors not specified, use all numeric variables except target
  if (is.null(predictors)) {
    predictors <- names(data)[sapply(data, is.numeric)]
    predictors <- setdiff(predictors, target)
  } else {
    # Validate that all predictors exist in the data
    missing_predictors <- setdiff(predictors, names(data))
    if (length(missing_predictors) > 0) {
      stop("The following predictors are not in the data: ", 
           paste(missing_predictors, collapse = ", "))
    }
  }
  
  # Check if we have enough predictors
  if (length(predictors) == 0) {
    stop("No valid predictors found")
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Split data into training and testing sets
  n <- nrow(data)
  test_indices <- sample(1:n, size = round(test_size * n))
  train_data <- data[-test_indices, ]
  test_data <- data[test_indices, ]
  
  # Initialize results list
  results <- list(
    models = list(),
    performance = list(),
    variable_importance = list(),
    test_predictions = list(),
    comparison = NULL
  )
  
  # Build models
  for (model_type in model_types) {
    # Check if required packages are available
    if (!check_required_packages(model_type)) {
      warning(paste("Skipping", model_type, "model due to missing package dependencies"))
      next
    }
    
    # Build the model
    model_result <- tryCatch({
      build_single_model(
        train_data = train_data,
        test_data = test_data,
        target = target,
        predictors = predictors,
        model_type = model_type,
        cv_folds = cv_folds,
        tune_hyperparameters = tune_hyperparameters
      )
    }, error = function(e) {
      warning(paste("Error building", model_type, "model:", e$message))
      return(NULL)
    })
    
    # Store results if model was built successfully
    if (!is.null(model_result)) {
      results$models[[model_type]] <- model_result$model
      results$performance[[model_type]] <- model_result$performance
      results$variable_importance[[model_type]] <- model_result$variable_importance
      results$test_predictions[[model_type]] <- model_result$test_predictions
    }
  }
  
  # Compare models
  if (length(results$performance) > 0) {
    performance_metrics <- do.call(rbind, lapply(names(results$performance), function(model_name) {
      perf <- results$performance[[model_name]]
      data.frame(
        model = model_name,
        rmse = perf$test_rmse,
        mae = perf$test_mae,
        r_squared = perf$test_r_squared,
        train_rmse = perf$train_rmse,
        train_r_squared = perf$train_r_squared
      )
    }))
    
    # Sort by test RMSE
    results$comparison <- performance_metrics[order(performance_metrics$rmse), ]
  }
  
  # Add class for custom printing
  class(results) <- c("model_comparison", "list")
  
  return(results)
}

#' Build a single regression model
#'
#' @param train_data Training data frame
#' @param test_data Test data frame
#' @param target Target variable name
#' @param predictors Character vector of predictor names
#' @param model_type Type of model to build
#' @param cv_folds Number of cross-validation folds
#' @param tune_hyperparameters Whether to tune hyperparameters
#'
#' @return A list containing the model and performance metrics
#' @keywords internal
build_single_model <- function(train_data, 
                              test_data, 
                              target, 
                              predictors,
                              model_type = "linear",
                              cv_folds = 5,
                              tune_hyperparameters = FALSE) {
  
  # Create formula
  formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Initialize result list
  result <- list(
    model = NULL,
    performance = list(),
    variable_importance = NULL,
    test_predictions = NULL
  )
  
  # Build model based on type
  if (model_type == "linear") {
    # Linear regression
    model <- lm(formula_obj, data = train_data)
    
    # Get variable importance (standardized coefficients)
    coefs <- summary(model)$coefficients[-1, 1]  # Exclude intercept
    var_imp <- abs(coefs)
    var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
    result$variable_importance <- data.frame(
      variable = names(var_imp),
      importance = as.numeric(var_imp)
    )
    result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    
  } else if (model_type == "ridge" || model_type == "lasso" || model_type == "elastic_net") {
    # Regularized regression using glmnet
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Package 'glmnet' is required for ridge, lasso, or elastic net regression")
    }
    
    # Prepare data matrix
    x_train <- as.matrix(train_data[, predictors])
    y_train <- train_data[[target]]
    x_test <- as.matrix(test_data[, predictors])
    
    # Set alpha based on model type
    alpha <- switch(model_type,
                   "ridge" = 0,
                   "lasso" = 1,
                   "elastic_net" = 0.5)
    
    if (tune_hyperparameters) {
      # Use cross-validation to find optimal lambda
      cv_model <- glmnet::cv.glmnet(x_train, y_train, alpha = alpha, nfolds = cv_folds)
      lambda <- cv_model$lambda.min
    } else {
      # Use default lambda sequence
      lambda <- NULL
    }
    
    # Fit final model
    model <- glmnet::glmnet(x_train, y_train, alpha = alpha, lambda = lambda)
    
    # Get variable importance
    coefs <- as.matrix(coefs(model, s = "lambda.min"))[-1, ]  # Exclude intercept
    var_imp <- abs(coefs)
    var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
    result$variable_importance <- data.frame(
      variable = rownames(coefs),
      importance = as.numeric(var_imp)
    )
    result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    
  } else if (model_type == "random_forest") {
    # Random Forest regression
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("Package 'randomForest' is required for random forest regression")
    }
    
    if (tune_hyperparameters && requireNamespace("caret", quietly = TRUE)) {
      # Set up tuning grid
      tuning_grid <- expand.grid(
        mtry = seq(max(1, floor(length(predictors) / 3)), min(length(predictors), 10), by = 1)
      )
      
      # Set up train control
      train_control <- caret::trainControl(
        method = "cv",
        number = cv_folds,
        verboseIter = FALSE
      )
      
      # Train model with tuning
      model <- caret::train(
        formula_obj,
        data = train_data,
        method = "rf",
        trControl = train_control,
        tuneGrid = tuning_grid,
        importance = TRUE
      )
      
      # Extract final model
      final_model <- model$finalModel
      
      # Get variable importance
      var_imp <- randomForest::importance(final_model, type = 1)  # %IncMSE
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = rownames(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
      
    } else {
      # Train model without tuning
      model <- randomForest::randomForest(
        formula_obj,
        data = train_data,
        importance = TRUE
      )
      
      # Get variable importance
      var_imp <- randomForest::importance(model, type = 1)  # %IncMSE
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = rownames(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    }
    
  } else if (model_type == "xgboost") {
    # XGBoost regression
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("Package 'xgboost' is required for XGBoost regression")
    }
    
    # Prepare data matrix
    x_train <- as.matrix(train_data[, predictors])
    y_train <- train_data[[target]]
    x_test <- as.matrix(test_data[, predictors])
    
    if (tune_hyperparameters && requireNamespace("caret", quietly = TRUE)) {
      # Set up tuning grid
      tuning_grid <- expand.grid(
        nrounds = c(50, 100, 150),
        max_depth = c(3, 6, 9),
        eta = c(0.01, 0.1, 0.3),
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
      
      # Set up train control
      train_control <- caret::trainControl(
        method = "cv",
        number = cv_folds,
        verboseIter = FALSE
      )
      
      # Train model with tuning
      model <- caret::train(
        x = x_train,
        y = y_train,
        method = "xgbTree",
        trControl = train_control,
        tuneGrid = tuning_grid,
        objective = "reg:squarederror",
        verbose = 0
      )
      
      # Extract final model
      final_model <- model$finalModel
      
      # Get variable importance
      var_imp <- xgboost::xgb.importance(feature_names = predictors, model = final_model)
      result$variable_importance <- data.frame(
        variable = var_imp$Feature,
        importance = var_imp$Gain * 100
      )
      
    } else {
      # Train model without tuning
      params <- list(
        objective = "reg:squarederror",
        max_depth = 6,
        eta = 0.1
      )
      
      model <- xgboost::xgboost(
        data = x_train,
        label = y_train,
        params = params,
        nrounds = 100,
        verbose = 0
      )
      
      # Get variable importance
      var_imp <- xgboost::xgb.importance(feature_names = predictors, model = model)
      result$variable_importance <- data.frame(
        variable = var_imp$Feature,
        importance = var_imp$Gain * 100
      )
    }
  } else {
    stop("Unsupported model type: ", model_type)
  }
  
  # Store model
  result$model <- model
  
  # Make predictions
  if (model_type %in% c("ridge", "lasso", "elastic_net")) {
    # For glmnet models
    x_train <- as.matrix(train_data[, predictors])
    x_test <- as.matrix(test_data[, predictors])
    
    train_pred <- predict(model, newx = x_train, s = "lambda.min")
    test_pred <- predict(model, newx = x_test, s = "lambda.min")
  } else if (model_type == "xgboost" && !inherits(model, "train")) {
    # For xgboost models (not from caret)
    x_train <- as.matrix(train_data[, predictors])
    x_test <- as.matrix(test_data[, predictors])
    
    train_pred <- predict(model, newdata = x_train)
    test_pred <- predict(model, newdata = x_test)
  } else {
    # For other models
    train_pred <- predict(model, newdata = train_data)
    test_pred <- predict(model, newdata = test_data)
  }
  
  # Calculate performance metrics
  train_rmse <- sqrt(mean((train_pred - train_data[[target]])^2))
  train_mae <- mean(abs(train_pred - train_data[[target]]))
  train_r_squared <- 1 - sum((train_data[[target]] - train_pred)^2) / 
    sum((train_data[[target]] - mean(train_data[[target]]))^2)
  
  test_rmse <- sqrt(mean((test_pred - test_data[[target]])^2))
  test_mae <- mean(abs(test_pred - test_data[[target]]))
  test_r_squared <- 1 - sum((test_data[[target]] - test_pred)^2) / 
    sum((test_data[[target]] - mean(test_data[[target]]))^2)
  
  # Store performance metrics
  result$performance <- list(
    train_rmse = train_rmse,
    train_mae = train_mae,
    train_r_squared = train_r_squared,
    test_rmse = test_rmse,
    test_mae = test_mae,
    test_r_squared = test_r_squared
  )
  
  # Store test predictions
  result$test_predictions <- data.frame(
    actual = test_data[[target]],
    predicted = test_pred
  )
  
  return(result)
}

#' Build and evaluate multiple classification models
#'
#' @param data A data frame containing the variables
#' @param target The name of the target variable
#' @param predictors Character vector of predictor variable names
#' @param model_types Types of models to build ("logistic", "decision_tree", "random_forest", "xgboost")
#' @param test_size Proportion of data to use for testing (0 to 1)
#' @param seed Random seed for reproducibility
#' @param cv_folds Number of cross-validation folds
#' @param tune_hyperparameters Whether to tune hyperparameters
#'
#' @return A list containing model results and comparisons
#' @export
#'
#' @examples
#' \dontrun{
#' data(iris)
#' # Convert to binary classification problem
#' iris$is_setosa <- as.factor(ifelse(iris$Species == "setosa", "yes", "no"))
#' model_results <- build_classification_models(
#'   iris, 
#'   target = "is_setosa", 
#'   predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' )
#' }
build_classification_models <- function(data, 
                                       target, 
                                       predictors = NULL,
                                       model_types = c("logistic", "random_forest"),
                                       test_size = 0.2,
                                       seed = 42,
                                       cv_folds = 5,
                                       tune_hyperparameters = FALSE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!target %in% names(data)) {
    stop("Target variable not found in data")
  }
  
  # Ensure target is a factor
  if (!is.factor(data[[target]])) {
    warning("Converting target variable to factor")
    data[[target]] <- as.factor(data[[target]])
  }
  
  # If predictors not specified, use all numeric variables
  if (is.null(predictors)) {
    predictors <- names(data)[sapply(data, is.numeric)]
  } else {
    # Validate that all predictors exist in the data
    missing_predictors <- setdiff(predictors, names(data))
    if (length(missing_predictors) > 0) {
      stop("The following predictors are not in the data: ", 
           paste(missing_predictors, collapse = ", "))
    }
  }
  
  # Check if we have enough predictors
  if (length(predictors) == 0) {
    stop("No valid predictors found")
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Split data into training and testing sets
  n <- nrow(data)
  test_indices <- sample(1:n, size = round(test_size * n))
  train_data <- data[-test_indices, ]
  test_data <- data[test_indices, ]
  
  # Initialize results list
  results <- list(
    models = list(),
    performance = list(),
    variable_importance = list(),
    test_predictions = list(),
    comparison = NULL
  )
  
  # Build models
  for (model_type in model_types) {
    # Check if required packages are available
    if (!check_required_packages(model_type)) {
      warning(paste("Skipping", model_type, "model due to missing package dependencies"))
      next
    }
    
    # Build the model
    model_result <- tryCatch({
      build_single_classification_model(
        train_data = train_data,
        test_data = test_data,
        target = target,
        predictors = predictors,
        model_type = model_type,
        cv_folds = cv_folds,
        tune_hyperparameters = tune_hyperparameters
      )
    }, error = function(e) {
      warning(paste("Error building", model_type, "model:", e$message))
      return(NULL)
    })
    
    # Store results if model was built successfully
    if (!is.null(model_result)) {
      results$models[[model_type]] <- model_result$model
      results$performance[[model_type]] <- model_result$performance
      results$variable_importance[[model_type]] <- model_result$variable_importance
      results$test_predictions[[model_type]] <- model_result$test_predictions
    }
  }
  
  # Compare models
  if (length(results$performance) > 0) {
    performance_metrics <- do.call(rbind, lapply(names(results$performance), function(model_name) {
      perf <- results$performance[[model_name]]
      data.frame(
        model = model_name,
        accuracy = perf$test_accuracy,
        precision = perf$test_precision,
        recall = perf$test_recall,
        f1_score = perf$test_f1,
        auc = perf$test_auc
      )
    }))
    
    # Sort by accuracy
    results$comparison <- performance_metrics[order(performance_metrics$accuracy, decreasing = TRUE), ]
  }
  
  # Add class for custom printing
  class(results) <- c("model_comparison", "list")
  
  return(results)
}

#' Build a single classification model
#'
#' @param train_data Training data frame
#' @param test_data Test data frame
#' @param target Target variable name
#' @param predictors Character vector of predictor names
#' @param model_type Type of model to build
#' @param cv_folds Number of cross-validation folds
#' @param tune_hyperparameters Whether to tune hyperparameters
#'
#' @return A list containing the model and performance metrics
#' @keywords internal
build_single_classification_model <- function(train_data, 
                                            test_data, 
                                            target, 
                                            predictors,
                                            model_type = "logistic",
                                            cv_folds = 5,
                                            tune_hyperparameters = FALSE) {
  
  # Create formula
  formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Initialize result list
  result <- list(
    model = NULL,
    performance = list(),
    variable_importance = NULL,
    test_predictions = NULL
  )
  
  # Build model based on type
  if (model_type == "logistic") {
    # Logistic regression
    model <- glm(formula_obj, data = train_data, family = binomial())
    
    # Get variable importance (standardized coefficients)
    coefs <- summary(model)$coefficients[-1, 1]  # Exclude intercept
    var_imp <- abs(coefs)
    var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
    result$variable_importance <- data.frame(
      variable = names(var_imp),
      importance = as.numeric(var_imp)
    )
    result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    
  } else if (model_type == "decision_tree") {
    # Decision tree
    if (!requireNamespace("rpart", quietly = TRUE)) {
      stop("Package 'rpart' is required for decision tree classification")
    }
    
    if (tune_hyperparameters && requireNamespace("caret", quietly = TRUE)) {
      # Set up tuning grid
      tuning_grid <- expand.grid(
        cp = seq(0.01, 0.3, by = 0.01)
      )
      
      # Set up train control
      train_control <- caret::trainControl(
        method = "cv",
        number = cv_folds,
        verboseIter = FALSE
      )
      
      # Train model with tuning
      model <- caret::train(
        formula_obj,
        data = train_data,
        method = "rpart",
        trControl = train_control,
        tuneGrid = tuning_grid
      )
      
      # Extract final model
      final_model <- model$finalModel
      
      # Get variable importance
      var_imp <- final_model$variable.importance
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = names(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
      
    } else {
      # Train model without tuning
      model <- rpart::rpart(
        formula_obj,
        data = train_data,
        method = "class"
      )
      
      # Get variable importance
      var_imp <- model$variable.importance
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = names(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    }
    
  } else if (model_type == "random_forest") {
    # Random Forest classification
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("Package 'randomForest' is required for random forest classification")
    }
    
    if (tune_hyperparameters && requireNamespace("caret", quietly = TRUE)) {
      # Set up tuning grid
      tuning_grid <- expand.grid(
        mtry = seq(max(1, floor(length(predictors) / 3)), min(length(predictors), 10), by = 1)
      )
      
      # Set up train control
      train_control <- caret::trainControl(
        method = "cv",
        number = cv_folds,
        verboseIter = FALSE
      )
      
      # Train model with tuning
      model <- caret::train(
        formula_obj,
        data = train_data,
        method = "rf",
        trControl = train_control,
        tuneGrid = tuning_grid,
        importance = TRUE
      )
      
      # Extract final model
      final_model <- model$finalModel
      
      # Get variable importance
      var_imp <- randomForest::importance(final_model)
      var_imp <- var_imp[, "MeanDecreaseGini"]
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = names(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
      
    } else {
      # Train model without tuning
      model <- randomForest::randomForest(
        formula_obj,
        data = train_data,
        importance = TRUE
      )
      
      # Get variable importance
      var_imp <- randomForest::importance(model)
      var_imp <- var_imp[, "MeanDecreaseGini"]
      var_imp <- var_imp / sum(var_imp) * 100  # Convert to percentage
      result$variable_importance <- data.frame(
        variable = names(var_imp),
        importance = as.numeric(var_imp)
      )
      result$variable_importance <- result$variable_importance[order(result$variable_importance$importance, decreasing = TRUE), ]
    }
    
  } else if (model_type == "xgboost") {
    # XGBoost classification
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("Package 'xgboost' is required for XGBoost classification")
    }
    
    # Prepare data matrix
    x_train <- as.matrix(train_data[, predictors])
    y_train <- as.integer(train_data[[target]]) - 1  # Convert to 0-based index
    x_test <- as.matrix(test_data[, predictors])
    
    if (tune_hyperparameters && requireNamespace("caret", quietly = TRUE)) {
      # Set up tuning grid
      tuning_grid <- expand.grid(
        nrounds = c(50, 100, 150),
        max_depth = c(3, 6, 9),
        eta = c(0.01, 0.1, 0.3),
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
      
      # Set up train control
      train_control <- caret::trainControl(
        method = "cv",
        number = cv_folds,
        verboseIter = FALSE
      )
      
      # Train model with tuning
      model <- caret::train(
        x = x_train,
        y = as.factor(train_data[[target]]),
        method = "xgbTree",
        trControl = train_control,
        tuneGrid = tuning_grid,
        objective = "binary:logistic",
        verbose = 0
      )
      
      # Extract final model
      final_model <- model$finalModel
      
      # Get variable importance
      var_imp <- xgboost::xgb.importance(feature_names = predictors, model = final_model)
      result$variable_importance <- data.frame(
        variable = var_imp$Feature,
        importance = var_imp$Gain * 100
      )
      
    } else {
      # Train model without tuning
      params <- list(
        objective = "binary:logistic",
        max_depth = 6,
        eta = 0.1
      )
      
      model <- xgboost::xgboost(
        data = x_train,
        label = y_train,
        params = params,
        nrounds = 100,
        verbose = 0
      )
      
      # Get variable importance
      var_imp <- xgboost::xgb.importance(feature_names = predictors, model = model)
      result$variable_importance <- data.frame(
        variable = var_imp$Feature,
        importance = var_imp$Gain * 100
      )
    }
  } else {
    stop("Unsupported model type: ", model_type)
  }
  
  # Store model
  result$model <- model
  
  # Make predictions
  if (model_type == "logistic") {
    # For logistic regression
    train_pred_prob <- predict(model, newdata = train_data, type = "response")
    train_pred_class <- ifelse(train_pred_prob > 0.5, levels(train_data[[target]])[2], levels(train_data[[target]])[1])
    
    test_pred_prob <- predict(model, newdata = test_data, type = "response")
    test_pred_class <- ifelse(test_pred_prob > 0.5, levels(test_data[[target]])[2], levels(test_data[[target]])[1])
    
  } else if (model_type == "xgboost" && !inherits(model, "train")) {
    # For xgboost models (not from caret)
    x_train <- as.matrix(train_data[, predictors])
    x_test <- as.matrix(test_data[, predictors])
    
    train_pred_prob <- predict(model, newdata = x_train)
    train_pred_class <- ifelse(train_pred_prob > 0.5, levels(train_data[[target]])[2], levels(train_data[[target]])[1])
    
    test_pred_prob <- predict(model, newdata = x_test)
    test_pred_class <- ifelse(test_pred_prob > 0.5, levels(test_data[[target]])[2], levels(test_data[[target]])[1])
    
  } else {
    # For other models
    train_pred_class <- predict(model, newdata = train_data, type = "class")
    train_pred_prob <- predict(model, newdata = train_data, type = "prob")[, 2]
    
    test_pred_class <- predict(model, newdata = test_data, type = "class")
    test_pred_prob <- predict(model, newdata = test_data, type = "prob")[, 2]
  }
  
  # Calculate performance metrics
  train_cm <- table(Predicted = train_pred_class, Actual = train_data[[target]])
  train_accuracy <- sum(diag(train_cm)) / sum(train_cm)
  
  test_cm <- table(Predicted = test_pred_class, Actual = test_data[[target]])
  test_accuracy <- sum(diag(test_cm)) / sum(test_cm)
  
  # Calculate precision, recall, F1 score
  if (dim(test_cm)[1] == 2 && dim(test_cm)[2] == 2) {
    # Binary classification
    test_precision <- test_cm[2, 2] / sum(test_cm[2, ])
    test_recall <- test_cm[2, 2] / sum(test_cm[, 2])
    test_f1 <- 2 * test_precision * test_recall / (test_precision + test_recall)
    
    # Calculate AUC if possible
    if (requireNamespace("pROC", quietly = TRUE)) {
      test_auc <- pROC::auc(pROC::roc(test_data[[target]], test_pred_prob, quiet = TRUE))
    } else {
      test_auc <- NA
    }
  } else {
    # Multiclass - use macro averaging
    test_precision <- NA
    test_recall <- NA
    test_f1 <- NA
    test_auc <- NA
  }
  
  # Store performance metrics
  result$performance <- list(
    train_accuracy = train_accuracy,
    train_confusion_matrix = train_cm,
    test_accuracy = test_accuracy,
    test_confusion_matrix = test_cm,
    test_precision = test_precision,
    test_recall = test_recall,
    test_f1 = test_f1,
    test_auc = test_auc
  )
  
  # Store test predictions
  result$test_predictions <- data.frame(
    actual = test_data[[target]],
    predicted_class = test_pred_class,
    predicted_prob = test_pred_prob
  )
  
  return(result)
}

#' Check if required packages are available for a model type
#'
#' @param model_type Type of model
#' @return Logical indicating if required packages are available
#' @keywords internal
check_required_packages <- function(model_type) {
  required_packages <- switch(model_type,
                             "linear" = c(),
                             "logistic" = c(),
                             "ridge" = c("glmnet"),
                             "lasso" = c("glmnet"),
                             "elastic_net" = c("glmnet"),
                             "decision_tree" = c("rpart"),
                             "random_forest" = c("randomForest"),
                             "xgboost" = c("xgboost"),
                             c())
  
  all(sapply(required_packages, requireNamespace, quietly = TRUE))
}

#' Print method for model_comparison objects
#'
#' @param x An object of class model_comparison
#' @param ... Additional arguments
#' @export
#' @keywords internal
print.model_comparison <- function(x, ...) {
  cat("Model Comparison Results\n")
  cat("======================\n\n")
  
  if (!is.null(x$comparison)) {
    # Print model comparison table
    cat("Model Performance Comparison:\n")
    print(x$comparison)
    cat("\n")
    
    # Print best model
    best_model <- x$comparison$model[1]
    cat("Best model:", best_model, "\n\n")
    
    # Print variable importance for best model
    if (!is.null(x$variable_importance[[best_model]])) {
      cat("Top 5 important variables for best model:\n")
      print(head(x$variable_importance[[best_model]], 5))
      cat("\n")
    }
  } else {
    cat("No models were successfully built.\n")
  }
  
  cat("Use summary() for more detailed information.\n")
}

#' Summary method for model_comparison objects
#'
#' @param object An object of class model_comparison
#' @param ... Additional arguments
#' @export
#' @keywords internal
summary.model_comparison <- function(object, ...) {
  print(object, ...)
  
  # Print more detailed information
  cat("\nDetailed Model Information:\n")
  cat("=========================\n\n")
  
  for (model_name in names(object$models)) {
    cat("Model:", model_name, "\n")
    cat("-----", paste(rep("-", nchar(model_name)), collapse = ""), "\n")
    
    # Print performance metrics
    perf <- object$performance[[model_name]]
    cat("Performance metrics:\n")
    
    # Check if regression or classification
    if (!is.null(perf$test_rmse)) {
      # Regression metrics
      cat("  Test RMSE:", round(perf$test_rmse, 4), "\n")
      cat("  Test MAE:", round(perf$test_mae, 4), "\n")
      cat("  Test R-squared:", round(perf$test_r_squared, 4), "\n")
      cat("  Train RMSE:", round(perf$train_rmse, 4), "\n")
      cat("  Train R-squared:", round(perf$train_r_squared, 4), "\n")
    } else {
      # Classification metrics
      cat("  Test Accuracy:", round(perf$test_accuracy, 4), "\n")
      
      if (!is.na(perf$test_precision)) {
        cat("  Test Precision:", round(perf$test_precision, 4), "\n")
        cat("  Test Recall:", round(perf$test_recall, 4), "\n")
        cat("  Test F1 Score:", round(perf$test_f1, 4), "\n")
      }
      
      if (!is.na(perf$test_auc)) {
        cat("  Test AUC:", round(perf$test_auc, 4), "\n")
      }
      
      cat("\n  Confusion Matrix:\n")
      print(perf$test_confusion_matrix)
    }
    
    # Print variable importance
    if (!is.null(object$variable_importance[[model_name]])) {
      cat("\nVariable Importance:\n")
      print(head(object$variable_importance[[model_name]], 10))
    }
    
    cat("\n\n")
  }
  
  invisible(object)
}

#' Plot model comparison results
#'
#' @param x A model_comparison object
#' @param metric Performance metric to plot
#' @param ... Additional arguments
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' model_results <- build_regression_models(
#'   mtcars, 
#'   target = "mpg", 
#'   predictors = c("cyl", "disp", "hp", "wt")
#' )
#' plot(model_results)
#' }
plot.model_comparison <- function(x, metric = NULL, ...) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }
  
  if (is.null(x$comparison)) {
    stop("No models to plot")
  }
  
  # Determine if regression or classification
  is_regression <- "rmse" %in% names(x$comparison)
  
  # Set default metric if not provided
  if (is.null(metric)) {
    metric <- if (is_regression) "rmse" else "accuracy"
  }
  
  # Check if metric exists
  if (!metric %in% names(x$comparison)) {
    stop("Metric '", metric, "' not found in model comparison")
  }
  
  # Create plot
  p <- ggplot2::ggplot(x$comparison, ggplot2::aes_string(x = "model", y = metric)) +
    ggplot2::geom_bar(stat = "identity", fill = "#4e79a7") +
    ggplot2::labs(title = paste("Model Comparison by", toupper(metric)),
                 x = "Model",
                 y = toupper(metric)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  # For metrics where lower is better, flip the y-axis
  if (metric %in% c("rmse", "mae")) {
    p <- p + ggplot2::scale_y_reverse()
  }
  
  return(p)
}

