#' Time Series Analysis Functions
#'
#' @description A collection of functions for time series analysis and forecasting
#' @author Gabriel Demetrios Lafis
#' @name time_series_functions
NULL

#' Analyze and forecast time series data
#'
#' @param data A time series object or data frame with time and value columns
#' @param date_col Name of the date/time column if data is a data frame
#' @param value_col Name of the value column if data is a data frame
#' @param frequency Frequency of the time series (e.g., 12 for monthly data)
#' @param h Forecast horizon (number of periods to forecast)
#' @param models Vector of models to fit ("auto", "arima", "ets", "prophet", "tbats", "nnetar")
#' @param seasonal Logical, whether to include seasonal components
#' @param xreg External regressors (optional)
#' @param lambda Box-Cox transformation parameter (NULL for automatic)
#'
#' @return A list containing analysis results and forecasts
#' @export
#'
#' @examples
#' \dontrun{
#' data(AirPassengers)
#' ts_results <- analyze_time_series(AirPassengers, h = 12)
#' }
analyze_time_series <- function(data, 
                               date_col = NULL,
                               value_col = NULL,
                               frequency = NULL,
                               h = 12,
                               models = c("auto", "arima", "ets"),
                               seasonal = TRUE,
                               xreg = NULL,
                               lambda = NULL) {
  
  # Check if required packages are available
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required for time series analysis")
  }
  
  # Convert data frame to time series if needed
  ts_data <- convert_to_ts(data, date_col, value_col, frequency)
  
  # Initialize results list
  results <- list(
    data = ts_data,
    decomposition = NULL,
    stationarity = NULL,
    acf_pacf = NULL,
    models = list(),
    forecasts = list(),
    accuracy = list(),
    best_model = NULL
  )
  
  # Perform time series decomposition
  if (frequency(ts_data) > 1) {
    tryCatch({
      results$decomposition <- decompose_time_series(ts_data, type = "both")
    }, error = function(e) {
      warning("Time series decomposition failed: ", e$message)
    })
  }
  
  # Check stationarity
  results$stationarity <- check_stationarity(ts_data)
  
  # Calculate ACF and PACF
  results$acf_pacf <- calculate_acf_pacf(ts_data)
  
  # Fit models and generate forecasts
  for (model_type in models) {
    tryCatch({
      model_result <- fit_forecast_model(
        ts_data = ts_data,
        model_type = model_type,
        h = h,
        seasonal = seasonal,
        xreg = xreg,
        lambda = lambda
      )
      
      results$models[[model_type]] <- model_result$model
      results$forecasts[[model_type]] <- model_result$forecast
      results$accuracy[[model_type]] <- model_result$accuracy
      
    }, error = function(e) {
      warning(paste("Error fitting", model_type, "model:", e$message))
    })
  }
  
  # Determine best model based on AIC or accuracy
  if (length(results$models) > 0) {
    best_model <- find_best_model(results$models, results$accuracy)
    results$best_model <- best_model
  }
  
  # Add class for custom printing
  class(results) <- c("ts_analysis", "list")
  
  return(results)
}

#' Convert data to time series object
#'
#' @param data A time series object or data frame with time and value columns
#' @param date_col Name of the date/time column if data is a data frame
#' @param value_col Name of the value column if data is a data frame
#' @param frequency Frequency of the time series
#'
#' @return A time series object
#' @keywords internal
convert_to_ts <- function(data, date_col = NULL, value_col = NULL, frequency = NULL) {
  # Check if data is already a time series object
  if (stats::is.ts(data)) {
    return(data)
  }
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a time series object or a data frame")
  }
  
  # Check if date_col and value_col are provided
  if (is.null(date_col) || is.null(value_col)) {
    stop("Both date_col and value_col must be provided for data frames")
  }
  
  # Check if columns exist
  if (!date_col %in% names(data)) {
    stop("Date column '", date_col, "' not found in data")
  }
  
  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }
  
  # Convert date column to Date if it's not already
  if (!inherits(data[[date_col]], c("Date", "POSIXct", "POSIXlt"))) {
    data[[date_col]] <- as.Date(data[[date_col]])
  }
  
  # Sort data by date
  data <- data[order(data[[date_col]]), ]
  
  # Determine frequency if not provided
  if (is.null(frequency)) {
    # Try to infer frequency from data
    dates <- data[[date_col]]
    
    if (length(dates) > 1) {
      # Calculate the most common difference between consecutive dates
      date_diffs <- diff(as.numeric(dates))
      most_common_diff <- as.numeric(names(sort(table(date_diffs), decreasing = TRUE)[1]))
      
      # Infer frequency based on most common difference
      if (most_common_diff == 1) {
        # Daily data
        frequency <- 7  # Weekly seasonality
      } else if (most_common_diff %in% c(7, 28, 30, 31)) {
        # Weekly or monthly data
        frequency <- 12  # Yearly seasonality
      } else if (most_common_diff %in% c(90, 91, 92)) {
        # Quarterly data
        frequency <- 4  # Yearly seasonality
      } else {
        # Default to 1 (no seasonality)
        frequency <- 1
      }
    } else {
      frequency <- 1
    }
  }
  
  # Create time series object
  ts_data <- stats::ts(data[[value_col]], frequency = frequency)
  
  return(ts_data)
}

#' Decompose time series into trend, seasonal, and remainder components
#'
#' @param ts_data A time series object
#' @param type Type of decomposition ("additive", "multiplicative", or "both")
#'
#' @return A list containing decomposition results
#' @keywords internal
decompose_time_series <- function(ts_data, type = "both") {
  # Check if time series has enough data for decomposition
  if (length(ts_data) < 2 * frequency(ts_data)) {
    warning("Time series too short for decomposition")
    return(NULL)
  }
  
  results <- list()
  
  # Perform additive decomposition
  if (type %in% c("additive", "both")) {
    tryCatch({
      results$additive <- stats::decompose(ts_data, type = "additive")
    }, error = function(e) {
      warning("Additive decomposition failed: ", e$message)
    })
  }
  
  # Perform multiplicative decomposition
  if (type %in% c("multiplicative", "both")) {
    # Check for non-positive values
    if (min(ts_data, na.rm = TRUE) > 0) {
      tryCatch({
        results$multiplicative <- stats::decompose(ts_data, type = "multiplicative")
      }, error = function(e) {
        warning("Multiplicative decomposition failed: ", e$message)
      })
    } else {
      warning("Cannot perform multiplicative decomposition with non-positive values")
    }
  }
  
  # Try STL decomposition if available
  if (requireNamespace("forecast", quietly = TRUE)) {
    tryCatch({
      results$stl <- forecast::stl(ts_data, s.window = "periodic")
    }, error = function(e) {
      warning("STL decomposition failed: ", e$message)
    })
  }
  
  return(results)
}

#' Check stationarity of a time series
#'
#' @param ts_data A time series object
#'
#' @return A list containing stationarity test results
#' @keywords internal
check_stationarity <- function(ts_data) {
  results <- list(
    adf_test = NULL,
    kpss_test = NULL,
    pp_test = NULL,
    is_stationary = FALSE
  )
  
  # Augmented Dickey-Fuller test
  if (requireNamespace("tseries", quietly = TRUE)) {
    tryCatch({
      adf_result <- tseries::adf.test(ts_data)
      results$adf_test <- list(
        statistic = adf_result$statistic,
        p_value = adf_result$p.value,
        is_stationary = adf_result$p.value < 0.05
      )
    }, error = function(e) {
      warning("ADF test failed: ", e$message)
    })
    
    # KPSS test
    tryCatch({
      kpss_result <- tseries::kpss.test(ts_data)
      results$kpss_test <- list(
        statistic = kpss_result$statistic,
        p_value = kpss_result$p.value,
        is_stationary = kpss_result$p.value >= 0.05
      )
    }, error = function(e) {
      warning("KPSS test failed: ", e$message)
    })
    
    # Phillips-Perron test
    tryCatch({
      pp_result <- tseries::pp.test(ts_data)
      results$pp_test <- list(
        statistic = pp_result$statistic,
        p_value = pp_result$p.value,
        is_stationary = pp_result$p.value < 0.05
      )
    }, error = function(e) {
      warning("PP test failed: ", e$message)
    })
  }
  
  # Determine overall stationarity
  if (!is.null(results$adf_test) && !is.null(results$kpss_test)) {
    results$is_stationary <- results$adf_test$is_stationary && results$kpss_test$is_stationary
  } else if (!is.null(results$adf_test)) {
    results$is_stationary <- results$adf_test$is_stationary
  } else if (!is.null(results$kpss_test)) {
    results$is_stationary <- results$kpss_test$is_stationary
  }
  
  return(results)
}

#' Calculate ACF and PACF for a time series
#'
#' @param ts_data A time series object
#' @param max_lag Maximum lag to calculate
#'
#' @return A list containing ACF and PACF values
#' @keywords internal
calculate_acf_pacf <- function(ts_data, max_lag = NULL) {
  # Set default max_lag if not provided
  if (is.null(max_lag)) {
    max_lag <- min(length(ts_data) - 1, 3 * frequency(ts_data))
  }
  
  # Calculate ACF
  acf_values <- stats::acf(ts_data, lag.max = max_lag, plot = FALSE)
  
  # Calculate PACF
  pacf_values <- stats::pacf(ts_data, lag.max = max_lag, plot = FALSE)
  
  # Return results
  return(list(
    acf = acf_values,
    pacf = pacf_values
  ))
}

#' Fit a time series model and generate forecasts
#'
#' @param ts_data A time series object
#' @param model_type Type of model to fit
#' @param h Forecast horizon
#' @param seasonal Whether to include seasonal components
#' @param xreg External regressors
#' @param lambda Box-Cox transformation parameter
#'
#' @return A list containing the model, forecast, and accuracy
#' @keywords internal
fit_forecast_model <- function(ts_data, 
                              model_type = "auto", 
                              h = 12,
                              seasonal = TRUE,
                              xreg = NULL,
                              lambda = NULL) {
  
  # Check if forecast package is available
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required for time series forecasting")
  }
  
  # Initialize result list
  result <- list(
    model = NULL,
    forecast = NULL,
    accuracy = NULL
  )
  
  # Fit model based on type
  if (model_type == "auto") {
    # Auto ARIMA
    model <- forecast::auto.arima(
      ts_data,
      seasonal = seasonal,
      xreg = xreg,
      lambda = lambda
    )
    
  } else if (model_type == "arima") {
    # Manual ARIMA
    # Try to determine best order using auto.arima first
    auto_model <- forecast::auto.arima(
      ts_data,
      seasonal = seasonal,
      xreg = xreg,
      lambda = lambda,
      stepwise = FALSE,
      approximation = FALSE
    )
    
    # Extract order and seasonal order
    p <- auto_model$arma[1]
    d <- auto_model$arma[6]
    q <- auto_model$arma[2]
    P <- auto_model$arma[3]
    D <- auto_model$arma[7]
    Q <- auto_model$arma[4]
    
    # Fit ARIMA model with determined order
    model <- forecast::Arima(
      ts_data,
      order = c(p, d, q),
      seasonal = if (seasonal) list(order = c(P, D, Q), period = frequency(ts_data)) else NULL,
      xreg = xreg,
      lambda = lambda
    )
    
  } else if (model_type == "ets") {
    # Exponential smoothing state space model
    model <- forecast::ets(
      ts_data,
      lambda = lambda
    )
    
  } else if (model_type == "prophet") {
    # Prophet model
    if (!requireNamespace("prophet", quietly = TRUE)) {
      stop("Package 'prophet' is required for Prophet forecasting")
    }
    
    # Convert ts to data frame for prophet
    dates <- stats::time(ts_data)
    values <- as.numeric(ts_data)
    
    # Create prophet data frame
    df <- data.frame(
      ds = as.Date(paste(floor(dates), 
                        round((dates %% 1) * 12) + 1, 
                        "01", sep = "-")),
      y = values
    )
    
    # Fit prophet model
    model <- prophet::prophet(df, seasonality.mode = if (seasonal) "additive" else "none")
    
    # Create future data frame for forecasting
    future <- prophet::make_future_dataframe(model, periods = h, freq = "month")
    
    # Generate forecast
    prophet_forecast <- prophet::predict(model, future)
    
    # Convert prophet forecast to forecast object
    forecast_values <- stats::ts(
      prophet_forecast$yhat[(nrow(prophet_forecast) - h + 1):nrow(prophet_forecast)],
      start = stats::end(ts_data) + 1/frequency(ts_data),
      frequency = frequency(ts_data)
    )
    
    # Create a custom forecast object
    result$forecast <- list(
      mean = forecast_values,
      lower = stats::ts(
        prophet_forecast$yhat_lower[(nrow(prophet_forecast) - h + 1):nrow(prophet_forecast)],
        start = stats::end(ts_data) + 1/frequency(ts_data),
        frequency = frequency(ts_data)
      ),
      upper = stats::ts(
        prophet_forecast$yhat_upper[(nrow(prophet_forecast) - h + 1):nrow(prophet_forecast)],
        start = stats::end(ts_data) + 1/frequency(ts_data),
        frequency = frequency(ts_data)
      ),
      x = ts_data,
      method = "Prophet"
    )
    class(result$forecast) <- "forecast"
    
    # Calculate accuracy
    result$accuracy <- forecast::accuracy(model)
    
    # Return early for prophet
    return(result)
    
  } else if (model_type == "tbats") {
    # TBATS model
    model <- forecast::tbats(
      ts_data,
      use.box.cox = !is.null(lambda),
      use.trend = TRUE,
      use.damped.trend = NULL,
      seasonal.periods = if (seasonal) frequency(ts_data) else NULL
    )
    
  } else if (model_type == "nnetar") {
    # Neural Network AR model
    model <- forecast::nnetar(
      ts_data,
      lambda = lambda
    )
    
  } else {
    stop("Unsupported model type: ", model_type)
  }
  
  # Store model
  result$model <- model
  
  # Generate forecast
  result$forecast <- forecast::forecast(model, h = h, xreg = xreg)
  
  # Calculate accuracy
  result$accuracy <- forecast::accuracy(model)
  
  return(result)
}

#' Find the best time series model
#'
#' @param models List of fitted models
#' @param accuracy List of accuracy metrics
#'
#' @return Name of the best model
#' @keywords internal
find_best_model <- function(models, accuracy) {
  # Initialize variables
  best_model <- NULL
  best_aic <- Inf
  best_rmse <- Inf
  
  # Check each model
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    # Try to get AIC
    model_aic <- tryCatch({
      stats::AIC(model)
    }, error = function(e) {
      NA
    })
    
    # Get RMSE
    model_rmse <- accuracy[[model_name]]["Training set", "RMSE"]
    
    # Update best model based on AIC if available
    if (!is.na(model_aic) && model_aic < best_aic) {
      best_model <- model_name
      best_aic <- model_aic
    } else if (is.na(model_aic) && model_rmse < best_rmse) {
      # Use RMSE if AIC is not available
      best_model <- model_name
      best_rmse <- model_rmse
    }
  }
  
  return(best_model)
}

#' Plot time series analysis results
#'
#' @param x A ts_analysis object
#' @param plot_type Type of plot to generate
#' @param ... Additional arguments
#' @return A plot object
#' @export
#'
#' @examples
#' \dontrun{
#' data(AirPassengers)
#' ts_results <- analyze_time_series(AirPassengers, h = 12)
#' plot(ts_results, "forecast")
#' }
plot.ts_analysis <- function(x, plot_type = "forecast", ...) {
  # Check if required packages are available
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required for plotting")
  }
  
  # Determine plot type
  if (plot_type == "data") {
    # Plot original time series
    return(forecast::autoplot(x$data) + 
             ggplot2::ggtitle("Original Time Series") +
             ggplot2::theme_minimal())
    
  } else if (plot_type == "decomposition") {
    # Plot decomposition if available
    if (!is.null(x$decomposition$stl)) {
      return(forecast::autoplot(x$decomposition$stl) + 
               ggplot2::ggtitle("STL Decomposition") +
               ggplot2::theme_minimal())
    } else if (!is.null(x$decomposition$additive)) {
      return(forecast::autoplot(x$decomposition$additive) + 
               ggplot2::ggtitle("Additive Decomposition") +
               ggplot2::theme_minimal())
    } else {
      stop("No decomposition available to plot")
    }
    
  } else if (plot_type == "acf") {
    # Plot ACF
    if (!is.null(x$acf_pacf$acf)) {
      return(forecast::autoplot(x$acf_pacf$acf) + 
               ggplot2::ggtitle("Autocorrelation Function") +
               ggplot2::theme_minimal())
    } else {
      stop("No ACF available to plot")
    }
    
  } else if (plot_type == "pacf") {
    # Plot PACF
    if (!is.null(x$acf_pacf$pacf)) {
      return(forecast::autoplot(x$acf_pacf$pacf) + 
               ggplot2::ggtitle("Partial Autocorrelation Function") +
               ggplot2::theme_minimal())
    } else {
      stop("No PACF available to plot")
    }
    
  } else if (plot_type == "forecast") {
    # Plot forecast from best model if available
    if (!is.null(x$best_model) && !is.null(x$forecasts[[x$best_model]])) {
      return(forecast::autoplot(x$forecasts[[x$best_model]]) + 
               ggplot2::ggtitle(paste("Forecast using", x$best_model, "model")) +
               ggplot2::theme_minimal())
    } else if (length(x$forecasts) > 0) {
      # Use the first available forecast
      model_name <- names(x$forecasts)[1]
      return(forecast::autoplot(x$forecasts[[model_name]]) + 
               ggplot2::ggtitle(paste("Forecast using", model_name, "model")) +
               ggplot2::theme_minimal())
    } else {
      stop("No forecasts available to plot")
    }
    
  } else if (plot_type == "all_forecasts") {
    # Plot all forecasts for comparison
    if (length(x$forecasts) == 0) {
      stop("No forecasts available to plot")
    }
    
    # Create a list to store plots
    plots <- list()
    
    # Generate a plot for each forecast
    for (model_name in names(x$forecasts)) {
      plots[[model_name]] <- forecast::autoplot(x$forecasts[[model_name]]) + 
        ggplot2::ggtitle(paste("Forecast using", model_name, "model")) +
        ggplot2::theme_minimal()
    }
    
    # Return the list of plots
    return(plots)
    
  } else {
    stop("Unsupported plot type: ", plot_type)
  }
}

#' Print method for ts_analysis objects
#'
#' @param x An object of class ts_analysis
#' @param ... Additional arguments
#' @export
#' @keywords internal
print.ts_analysis <- function(x, ...) {
  cat("Time Series Analysis Results\n")
  cat("==========================\n\n")
  
  # Print basic information
  cat("Time Series Information:\n")
  cat("  Frequency:", frequency(x$data), "\n")
  cat("  Start time:", start(x$data)[1], "\n")
  cat("  End time:", end(x$data)[1], "\n")
  cat("  Number of observations:", length(x$data), "\n\n")
  
  # Print stationarity results
  if (!is.null(x$stationarity)) {
    cat("Stationarity Tests:\n")
    cat("  Is stationary:", x$stationarity$is_stationary, "\n")
    
    if (!is.null(x$stationarity$adf_test)) {
      cat("  ADF test p-value:", round(x$stationarity$adf_test$p_value, 4), 
          "(", ifelse(x$stationarity$adf_test$is_stationary, "stationary", "non-stationary"), ")\n")
    }
    
    if (!is.null(x$stationarity$kpss_test)) {
      cat("  KPSS test p-value:", round(x$stationarity$kpss_test$p_value, 4), 
          "(", ifelse(x$stationarity$kpss_test$is_stationary, "stationary", "non-stationary"), ")\n")
    }
    cat("\n")
  }
  
  # Print model information
  if (length(x$models) > 0) {
    cat("Fitted Models:\n")
    for (model_name in names(x$models)) {
      cat("  -", model_name, "\n")
    }
    cat("\n")
    
    # Print best model
    if (!is.null(x$best_model)) {
      cat("Best Model:", x$best_model, "\n\n")
    }
  } else {
    cat("No models were successfully fitted.\n\n")
  }
  
  cat("Use summary() for more detailed information.\n")
}

#' Summary method for ts_analysis objects
#'
#' @param object An object of class ts_analysis
#' @param ... Additional arguments
#' @export
#' @keywords internal
summary.ts_analysis <- function(object, ...) {
  print(object, ...)
  
  # Print more detailed information
  cat("\nDetailed Model Information:\n")
  cat("=========================\n\n")
  
  # Print accuracy metrics for all models
  if (length(object$accuracy) > 0) {
    cat("Model Accuracy Metrics:\n")
    
    # Create a data frame to store accuracy metrics
    accuracy_df <- do.call(rbind, lapply(names(object$accuracy), function(model_name) {
      acc <- object$accuracy[[model_name]]["Training set", ]
      data.frame(
        Model = model_name,
        RMSE = acc["RMSE"],
        MAE = acc["MAE"],
        MAPE = acc["MAPE"],
        MASE = acc["MASE"],
        row.names = NULL
      )
    }))
    
    # Sort by RMSE
    accuracy_df <- accuracy_df[order(accuracy_df$RMSE), ]
    
    # Print accuracy table
    print(accuracy_df)
    cat("\n")
  }
  
  # Print best model details
  if (!is.null(object$best_model)) {
    cat("Best Model Details (", object$best_model, "):\n", sep = "")
    
    # Print model summary
    model <- object$models[[object$best_model]]
    
    if (object$best_model == "arima" || object$best_model == "auto") {
      cat("ARIMA Order: (", model$arma[1], ",", model$arma[6], ",", model$arma[2], ")\n", sep = "")
      
      if (frequency(object$data) > 1) {
        cat("Seasonal Order: (", model$arma[3], ",", model$arma[7], ",", model$arma[4], 
            ")[", frequency(object$data), "]\n", sep = "")
      }
      
      cat("AIC:", round(stats::AIC(model), 2), "\n")
      cat("BIC:", round(stats::BIC(model), 2), "\n")
      
      # Print coefficients
      cat("\nCoefficients:\n")
      coef_table <- summary(model)$coef
      print(coef_table)
      
    } else if (object$best_model == "ets") {
      cat("ETS Model: ", model$method, "\n", sep = "")
      cat("AIC:", round(stats::AIC(model), 2), "\n")
      cat("BIC:", round(stats::BIC(model), 2), "\n")
      
      # Print components
      cat("\nComponents:\n")
      cat("  Error:", model$components[1], "\n")
      cat("  Trend:", model$components[2], "\n")
      cat("  Season:", model$components[3], "\n")
      
      # Print parameters
      cat("\nParameters:\n")
      print(model$par)
      
    } else {
      # For other model types
      cat("Model summary:\n")
      print(summary(model))
    }
    
    cat("\n")
  }
  
  # Print forecast details for best model
  if (!is.null(object$best_model) && !is.null(object$forecasts[[object$best_model]])) {
    cat("Forecast Details (", object$best_model, "):\n", sep = "")
    
    # Get forecast
    forecast <- object$forecasts[[object$best_model]]
    
    # Print forecast summary
    cat("Forecast horizon:", length(forecast$mean), "periods\n")
    cat("Forecast start:", forecast$mean[1], "\n")
    cat("Forecast end:", forecast$mean[length(forecast$mean)], "\n")
    
    # Print point forecasts and prediction intervals
    cat("\nPoint Forecasts and Prediction Intervals:\n")
    forecast_table <- data.frame(
      Point_Forecast = forecast$mean,
      Lo_80 = forecast$lower[, 1],
      Hi_80 = forecast$upper[, 1],
      Lo_95 = forecast$lower[, 2],
      Hi_95 = forecast$upper[, 2]
    )
    print(forecast_table)
    
    cat("\n")
  }
  
  invisible(object)
}

