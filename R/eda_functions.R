#' Exploratory Data Analysis Functions
#'
#' @description A collection of functions for exploratory data analysis
#' @author Gabriel Demetrios Lafis
#' @name eda_functions
NULL

#' Perform comprehensive exploratory data analysis on a dataset
#'
#' @param data A data frame to analyze
#' @param target Optional target variable name for supervised learning contexts
#' @param cat_vars Optional character vector of categorical variable names
#' @param num_vars Optional character vector of numerical variable names
#' @param date_vars Optional character vector of date variable names
#' @param id_vars Optional character vector of ID variables to exclude from analysis
#' @param max_cat_levels Maximum number of levels to display for categorical variables
#' @param correlation_method Method for correlation calculation ("pearson", "spearman", "kendall")
#' @param plots Logical, whether to generate plots
#'
#' @return A list containing EDA results
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' eda_results <- perform_eda(mtcars, target = "mpg")
#' }
perform_eda <- function(data, 
                       target = NULL,
                       cat_vars = NULL,
                       num_vars = NULL,
                       date_vars = NULL,
                       id_vars = NULL,
                       max_cat_levels = 10,
                       correlation_method = "pearson",
                       plots = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Initialize results list
  results <- list(
    summary = list(),
    variables = list(),
    relationships = list(),
    plots = list()
  )
  
  # Basic dataset information
  results$summary$dimensions <- dim(data)
  results$summary$column_types <- sapply(data, class)
  results$summary$missing_values <- colSums(is.na(data))
  results$summary$missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
  
  # Identify variable types if not provided
  if (is.null(cat_vars) && is.null(num_vars) && is.null(date_vars)) {
    var_types <- identify_variable_types(data)
    cat_vars <- var_types$categorical
    num_vars <- var_types$numerical
    date_vars <- var_types$dates
  }
  
  # Remove ID variables if specified
  if (!is.null(id_vars)) {
    cat_vars <- setdiff(cat_vars, id_vars)
    num_vars <- setdiff(num_vars, id_vars)
    date_vars <- setdiff(date_vars, id_vars)
  }
  
  # Analyze categorical variables
  if (length(cat_vars) > 0) {
    results$variables$categorical <- analyze_categorical_vars(
      data, cat_vars, max_cat_levels, target
    )
    
    if (plots) {
      results$plots$categorical <- plot_categorical_vars(
        data, cat_vars, max_cat_levels, target
      )
    }
  }
  
  # Analyze numerical variables
  if (length(num_vars) > 0) {
    results$variables$numerical <- analyze_numerical_vars(
      data, num_vars, target
    )
    
    if (plots) {
      results$plots$numerical <- plot_numerical_vars(
        data, num_vars, target
      )
    }
  }
  
  # Analyze date variables
  if (length(date_vars) > 0) {
    results$variables$dates <- analyze_date_vars(
      data, date_vars, target
    )
    
    if (plots) {
      results$plots$dates <- plot_date_vars(
        data, date_vars, target
      )
    }
  }
  
  # Analyze relationships between variables
  if (length(num_vars) >= 2) {
    results$relationships$correlation <- analyze_correlations(
      data, num_vars, method = correlation_method
    )
    
    if (plots) {
      results$plots$correlation <- plot_correlation_matrix(
        data, num_vars, method = correlation_method
      )
    }
  }
  
  # If target is specified, analyze its relationship with other variables
  if (!is.null(target)) {
    results$relationships$target <- analyze_target_relationships(
      data, target, cat_vars, num_vars
    )
  }
  
  # Return results
  class(results) <- c("eda_results", "list")
  return(results)
}

#' Identify variable types in a dataset
#'
#' @param data A data frame
#' @return A list with categorical, numerical, and date variables
#' @keywords internal
identify_variable_types <- function(data) {
  # Initialize empty vectors
  categorical <- character(0)
  numerical <- character(0)
  dates <- character(0)
  
  # Check each column
  for (col_name in names(data)) {
    col <- data[[col_name]]
    
    # Check for date types
    if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
      dates <- c(dates, col_name)
    }
    # Check for numerical types
    else if (is.numeric(col)) {
      # If few unique values relative to total, might be categorical
      if (length(unique(col)) <= min(10, nrow(data) * 0.05)) {
        categorical <- c(categorical, col_name)
      } else {
        numerical <- c(numerical, col_name)
      }
    }
    # Check for factors and characters
    else if (is.factor(col) || is.character(col) || is.logical(col)) {
      categorical <- c(categorical, col_name)
    }
    # Default to categorical for other types
    else {
      categorical <- c(categorical, col_name)
    }
  }
  
  return(list(
    categorical = categorical,
    numerical = numerical,
    dates = dates
  ))
}

#' Analyze categorical variables
#'
#' @param data A data frame
#' @param cat_vars Character vector of categorical variable names
#' @param max_cat_levels Maximum number of levels to display
#' @param target Optional target variable name
#' @return A list with analysis results for categorical variables
#' @keywords internal
analyze_categorical_vars <- function(data, cat_vars, max_cat_levels = 10, target = NULL) {
  results <- list()
  
  for (var in cat_vars) {
    if (!var %in% names(data)) next
    
    # Get frequency table
    freq_table <- table(data[[var]], useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    # Combine into a data frame
    var_summary <- data.frame(
      Level = names(freq_table),
      Count = as.numeric(freq_table),
      Percentage = as.numeric(prop_table)
    )
    
    # Handle unnamed levels (NAs)
    if (any(is.na(var_summary$Level))) {
      var_summary$Level[is.na(var_summary$Level)] <- "NA"
    }
    
    # Sort by frequency
    var_summary <- var_summary[order(var_summary$Count, decreasing = TRUE), ]
    
    # Limit to max_cat_levels
    if (nrow(var_summary) > max_cat_levels) {
      other_count <- sum(var_summary$Count[(max_cat_levels+1):nrow(var_summary)])
      other_percent <- sum(var_summary$Percentage[(max_cat_levels+1):nrow(var_summary)])
      
      var_summary <- var_summary[1:max_cat_levels, ]
      var_summary <- rbind(var_summary, 
                          data.frame(Level = "Other", 
                                    Count = other_count,
                                    Percentage = other_percent))
    }
    
    # Add target relationship if target is provided and is different from current variable
    if (!is.null(target) && var != target && target %in% names(data)) {
      if (is.numeric(data[[target]])) {
        # For numeric target, calculate mean by category
        target_stats <- aggregate(data[[target]], by = list(data[[var]]), 
                                 FUN = function(x) {
                                   c(mean = mean(x, na.rm = TRUE),
                                     median = median(x, na.rm = TRUE),
                                     sd = sd(x, na.rm = TRUE))
                                 })
        names(target_stats) <- c("Level", "Target_Stats")
        
        # Extract statistics
        target_stats$Target_Mean <- sapply(target_stats$Target_Stats, function(x) x["mean"])
        target_stats$Target_Median <- sapply(target_stats$Target_Stats, function(x) x["median"])
        target_stats$Target_SD <- sapply(target_stats$Target_Stats, function(x) x["sd"])
        target_stats$Target_Stats <- NULL
        
        # Merge with var_summary
        var_summary <- merge(var_summary, target_stats, by = "Level", all.x = TRUE)
      }
    }
    
    # Store results
    results[[var]] <- list(
      summary = var_summary,
      n_levels = length(freq_table),
      n_unique = length(unique(data[[var]])),
      missing = sum(is.na(data[[var]])),
      missing_pct = mean(is.na(data[[var]])) * 100
    )
  }
  
  return(results)
}

#' Analyze numerical variables
#'
#' @param data A data frame
#' @param num_vars Character vector of numerical variable names
#' @param target Optional target variable name
#' @return A list with analysis results for numerical variables
#' @keywords internal
analyze_numerical_vars <- function(data, num_vars, target = NULL) {
  results <- list()
  
  for (var in num_vars) {
    if (!var %in% names(data)) next
    
    # Calculate statistics
    var_data <- data[[var]]
    
    stats <- c(
      min = min(var_data, na.rm = TRUE),
      q1 = quantile(var_data, 0.25, na.rm = TRUE),
      median = median(var_data, na.rm = TRUE),
      mean = mean(var_data, na.rm = TRUE),
      q3 = quantile(var_data, 0.75, na.rm = TRUE),
      max = max(var_data, na.rm = TRUE),
      sd = sd(var_data, na.rm = TRUE),
      iqr = IQR(var_data, na.rm = TRUE),
      skewness = if (requireNamespace("moments", quietly = TRUE)) {
        moments::skewness(var_data, na.rm = TRUE)
      } else {
        NA
      },
      kurtosis = if (requireNamespace("moments", quietly = TRUE)) {
        moments::kurtosis(var_data, na.rm = TRUE)
      } else {
        NA
      }
    )
    
    # Detect outliers using IQR method
    q1 <- stats["q1"]
    q3 <- stats["q3"]
    iqr <- stats["iqr"]
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- var_data[var_data < lower_bound | var_data > upper_bound]
    
    # Add target relationship if target is provided and is different from current variable
    target_correlation <- NULL
    if (!is.null(target) && var != target && target %in% names(data) && is.numeric(data[[target]])) {
      target_correlation <- cor(data[[var]], data[[target]], 
                               use = "pairwise.complete.obs", 
                               method = "pearson")
    }
    
    # Store results
    results[[var]] <- list(
      summary = as.list(stats),
      missing = sum(is.na(var_data)),
      missing_pct = mean(is.na(var_data)) * 100,
      zeros = sum(var_data == 0, na.rm = TRUE),
      zeros_pct = mean(var_data == 0, na.rm = TRUE) * 100,
      negative = sum(var_data < 0, na.rm = TRUE),
      negative_pct = mean(var_data < 0, na.rm = TRUE) * 100,
      outliers = length(outliers),
      outliers_pct = length(outliers) / sum(!is.na(var_data)) * 100,
      target_correlation = target_correlation
    )
  }
  
  return(results)
}

#' Analyze date variables
#'
#' @param data A data frame
#' @param date_vars Character vector of date variable names
#' @param target Optional target variable name
#' @return A list with analysis results for date variables
#' @keywords internal
analyze_date_vars <- function(data, date_vars, target = NULL) {
  results <- list()
  
  for (var in date_vars) {
    if (!var %in% names(data)) next
    
    # Extract date variable
    var_data <- data[[var]]
    
    # Calculate basic statistics
    stats <- list(
      min_date = min(var_data, na.rm = TRUE),
      max_date = max(var_data, na.rm = TRUE),
      range_days = as.numeric(difftime(max(var_data, na.rm = TRUE), 
                                      min(var_data, na.rm = TRUE), 
                                      units = "days")),
      n_unique = length(unique(var_data)),
      missing = sum(is.na(var_data)),
      missing_pct = mean(is.na(var_data)) * 100
    )
    
    # Add time components if available
    if (inherits(var_data, c("POSIXct", "POSIXlt"))) {
      has_time <- any(format(var_data, "%H:%M:%S") != "00:00:00", na.rm = TRUE)
      stats$has_time_component <- has_time
    } else {
      stats$has_time_component <- FALSE
    }
    
    # Store results
    results[[var]] <- stats
  }
  
  return(results)
}

#' Analyze correlations between numerical variables
#'
#' @param data A data frame
#' @param num_vars Character vector of numerical variable names
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return A correlation matrix
#' @keywords internal
analyze_correlations <- function(data, num_vars, method = "pearson") {
  # Subset data to include only specified numerical variables
  num_data <- data[, num_vars, drop = FALSE]
  
  # Calculate correlation matrix
  cor_matrix <- cor(num_data, use = "pairwise.complete.obs", method = method)
  
  return(cor_matrix)
}

#' Analyze relationships between target and other variables
#'
#' @param data A data frame
#' @param target Target variable name
#' @param cat_vars Character vector of categorical variable names
#' @param num_vars Character vector of numerical variable names
#' @return A list with target relationship analysis
#' @keywords internal
analyze_target_relationships <- function(data, target, cat_vars, num_vars) {
  results <- list()
  
  # Check if target exists in the data
  if (!target %in% names(data)) {
    return(results)
  }
  
  target_data <- data[[target]]
  target_type <- if (is.numeric(target_data)) "numeric" else "categorical"
  
  results$target_type <- target_type
  
  # Analyze relationship with numerical variables
  if (target_type == "numeric" && length(num_vars) > 0) {
    # Calculate correlations with numerical variables
    num_vars_subset <- setdiff(num_vars, target)
    if (length(num_vars_subset) > 0) {
      correlations <- sapply(num_vars_subset, function(var) {
        cor(data[[var]], target_data, use = "pairwise.complete.obs", method = "pearson")
      })
      
      # Sort by absolute correlation
      correlations <- correlations[order(abs(correlations), decreasing = TRUE)]
      results$correlations <- correlations
    }
  }
  
  # Analyze relationship with categorical variables
  if (length(cat_vars) > 0) {
    if (target_type == "numeric") {
      # For numeric target, calculate ANOVA for each categorical variable
      anova_results <- lapply(cat_vars, function(var) {
        if (var == target) return(NULL)
        
        # Create formula for ANOVA
        formula <- as.formula(paste(target, "~", var))
        
        # Try to fit ANOVA model
        tryCatch({
          model <- aov(formula, data = data)
          summary_stats <- summary(model)[[1]]
          
          # Extract F value and p-value
          f_value <- summary_stats$`F value`[1]
          p_value <- summary_stats$`Pr(>F)`[1]
          
          # Calculate eta squared (effect size)
          ss_total <- sum(summary_stats$`Sum Sq`)
          eta_squared <- summary_stats$`Sum Sq`[1] / ss_total
          
          return(list(
            variable = var,
            f_value = f_value,
            p_value = p_value,
            eta_squared = eta_squared
          ))
        }, error = function(e) {
          return(NULL)
        })
      })
      
      # Remove NULL results and convert to data frame
      anova_results <- Filter(Negate(is.null), anova_results)
      
      if (length(anova_results) > 0) {
        anova_df <- do.call(rbind, lapply(anova_results, function(x) {
          data.frame(
            variable = x$variable,
            f_value = x$f_value,
            p_value = x$p_value,
            eta_squared = x$eta_squared
          )
        }))
        
        # Sort by effect size
        anova_df <- anova_df[order(anova_df$eta_squared, decreasing = TRUE), ]
        results$anova <- anova_df
      }
    } else {
      # For categorical target, calculate chi-square tests
      chi_square_results <- lapply(cat_vars, function(var) {
        if (var == target) return(NULL)
        
        # Create contingency table
        cont_table <- table(data[[var]], data[[target]])
        
        # Try to perform chi-square test
        tryCatch({
          test <- chisq.test(cont_table, simulate.p.value = TRUE)
          
          # Calculate Cramer's V
          n <- sum(cont_table)
          k <- min(nrow(cont_table), ncol(cont_table))
          cramers_v <- sqrt(test$statistic / (n * (k - 1)))
          
          return(list(
            variable = var,
            chi_square = test$statistic,
            p_value = test$p.value,
            cramers_v = cramers_v
          ))
        }, error = function(e) {
          return(NULL)
        })
      })
      
      # Remove NULL results and convert to data frame
      chi_square_results <- Filter(Negate(is.null), chi_square_results)
      
      if (length(chi_square_results) > 0) {
        chi_df <- do.call(rbind, lapply(chi_square_results, function(x) {
          data.frame(
            variable = x$variable,
            chi_square = x$chi_square,
            p_value = x$p_value,
            cramers_v = x$cramers_v
          )
        }))
        
        # Sort by Cramer's V
        chi_df <- chi_df[order(chi_df$cramers_v, decreasing = TRUE), ]
        results$chi_square <- chi_df
      }
    }
  }
  
  return(results)
}

#' Plot categorical variables
#'
#' @param data A data frame
#' @param cat_vars Character vector of categorical variable names
#' @param max_cat_levels Maximum number of levels to display
#' @param target Optional target variable name
#' @return A list of ggplot objects
#' @keywords internal
plot_categorical_vars <- function(data, cat_vars, max_cat_levels = 10, target = NULL) {
  plots <- list()
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for plotting.")
    return(plots)
  }
  
  for (var in cat_vars) {
    if (!var %in% names(data)) next
    
    # Get frequency table
    freq_table <- table(data[[var]], useNA = "ifany")
    
    # Convert to data frame for plotting
    plot_data <- data.frame(
      Level = names(freq_table),
      Count = as.numeric(freq_table)
    )
    
    # Handle unnamed levels (NAs)
    if (any(is.na(plot_data$Level))) {
      plot_data$Level[is.na(plot_data$Level)] <- "NA"
    }
    
    # Sort by frequency
    plot_data <- plot_data[order(plot_data$Count, decreasing = TRUE), ]
    
    # Limit to max_cat_levels
    if (nrow(plot_data) > max_cat_levels) {
      other_count <- sum(plot_data$Count[(max_cat_levels+1):nrow(plot_data)])
      plot_data <- plot_data[1:max_cat_levels, ]
      plot_data <- rbind(plot_data, 
                        data.frame(Level = "Other", Count = other_count))
    }
    
    # Create bar plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = stats::reorder(Level, Count), y = Count)) +
      ggplot2::geom_bar(stat = "identity", fill = "#4e79a7") +
      ggplot2::labs(title = paste("Distribution of", var),
                   x = var,
                   y = "Count") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    plots[[paste0(var, "_bar")]] <- p
    
    # If target is provided and is numeric, create box plots
    if (!is.null(target) && var != target && target %in% names(data) && is.numeric(data[[target]])) {
      # Create a subset for plotting
      plot_data_target <- data[, c(var, target)]
      names(plot_data_target) <- c("Variable", "Target")
      
      # Handle factor levels
      if (!is.factor(plot_data_target$Variable)) {
        plot_data_target$Variable <- factor(plot_data_target$Variable)
      }
      
      # Limit levels for plotting
      if (length(levels(plot_data_target$Variable)) > max_cat_levels) {
        # Get top levels by frequency
        top_levels <- names(sort(table(plot_data_target$Variable), decreasing = TRUE)[1:max_cat_levels])
        plot_data_target$Variable <- factor(plot_data_target$Variable, 
                                          levels = c(top_levels, "Other"))
        plot_data_target$Variable[!(plot_data_target$Variable %in% top_levels)] <- "Other"
      }
      
      # Create box plot
      p <- ggplot2::ggplot(plot_data_target, ggplot2::aes(x = Variable, y = Target)) +
        ggplot2::geom_boxplot(fill = "#4e79a7", alpha = 0.7) +
        ggplot2::labs(title = paste(var, "vs", target),
                     x = var,
                     y = target) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plots[[paste0(var, "_vs_", target)]] <- p
    }
  }
  
  return(plots)
}

#' Plot numerical variables
#'
#' @param data A data frame
#' @param num_vars Character vector of numerical variable names
#' @param target Optional target variable name
#' @return A list of ggplot objects
#' @keywords internal
plot_numerical_vars <- function(data, num_vars, target = NULL) {
  plots <- list()
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for plotting.")
    return(plots)
  }
  
  for (var in num_vars) {
    if (!var %in% names(data)) next
    
    # Create histogram
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = var)) +
      ggplot2::geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.7) +
      ggplot2::labs(title = paste("Distribution of", var),
                   x = var,
                   y = "Count") +
      ggplot2::theme_minimal()
    
    plots[[paste0(var, "_hist")]] <- p
    
    # Create density plot
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = var)) +
      ggplot2::geom_density(fill = "#4e79a7", alpha = 0.7) +
      ggplot2::labs(title = paste("Density of", var),
                   x = var,
                   y = "Density") +
      ggplot2::theme_minimal()
    
    plots[[paste0(var, "_density")]] <- p
    
    # Create box plot
    p <- ggplot2::ggplot(data, ggplot2::aes_string(y = var)) +
      ggplot2::geom_boxplot(fill = "#4e79a7", alpha = 0.7) +
      ggplot2::labs(title = paste("Boxplot of", var),
                   y = var) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank())
    
    plots[[paste0(var, "_box")]] <- p
    
    # If target is provided and is numeric, create scatter plots
    if (!is.null(target) && var != target && target %in% names(data) && is.numeric(data[[target]])) {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = var, y = target)) +
        ggplot2::geom_point(alpha = 0.5, color = "#4e79a7") +
        ggplot2::geom_smooth(method = "loess", color = "#e15759", se = TRUE) +
        ggplot2::labs(title = paste(var, "vs", target),
                     x = var,
                     y = target) +
        ggplot2::theme_minimal()
      
      plots[[paste0(var, "_vs_", target)]] <- p
    }
  }
  
  return(plots)
}

#' Plot date variables
#'
#' @param data A data frame
#' @param date_vars Character vector of date variable names
#' @param target Optional target variable name
#' @return A list of ggplot objects
#' @keywords internal
plot_date_vars <- function(data, date_vars, target = NULL) {
  plots <- list()
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for plotting.")
    return(plots)
  }
  
  for (var in date_vars) {
    if (!var %in% names(data)) next
    
    # Create timeline plot
    if (!is.null(target) && target %in% names(data) && is.numeric(data[[target]])) {
      # Time series plot with target
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = var, y = target)) +
        ggplot2::geom_line(color = "#4e79a7") +
        ggplot2::labs(title = paste(target, "over time"),
                     x = var,
                     y = target) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plots[[paste0(var, "_timeline")]] <- p
    } else {
      # Distribution of dates
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = var)) +
        ggplot2::geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.7) +
        ggplot2::labs(title = paste("Distribution of", var),
                     x = var,
                     y = "Count") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plots[[paste0(var, "_hist")]] <- p
    }
  }
  
  return(plots)
}

#' Plot correlation matrix
#'
#' @param data A data frame
#' @param num_vars Character vector of numerical variable names
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return A ggplot object
#' @keywords internal
plot_correlation_matrix <- function(data, num_vars, method = "pearson") {
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) || 
      !requireNamespace("reshape2", quietly = TRUE)) {
    warning("Packages 'ggplot2' and 'reshape2' are required for correlation plots.")
    return(NULL)
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data[, num_vars, drop = FALSE], 
                   use = "pairwise.complete.obs", 
                   method = method)
  
  # Convert to long format for ggplot
  cor_long <- reshape2::melt(cor_matrix)
  names(cor_long) <- c("Var1", "Var2", "Correlation")
  
  # Create correlation heatmap
  p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#e15759", mid = "white", high = "#4e79a7", 
                                midpoint = 0, limits = c(-1, 1)) +
    ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), 
                      color = "black", size = 3) +
    ggplot2::labs(title = paste("Correlation Matrix (", method, ")"),
                 x = "",
                 y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Print method for eda_results objects
#'
#' @param x An object of class eda_results
#' @param ... Additional arguments
#' @export
#' @keywords internal
print.eda_results <- function(x, ...) {
  cat("Exploratory Data Analysis Results\n")
  cat("================================\n\n")
  
  # Print dataset summary
  cat("Dataset Dimensions:", x$summary$dimensions[1], "rows,", x$summary$dimensions[2], "columns\n\n")
  
  # Print variable counts
  cat("Variable Types:\n")
  if (!is.null(x$variables$categorical)) {
    cat("  Categorical:", length(x$variables$categorical), "variables\n")
  }
  if (!is.null(x$variables$numerical)) {
    cat("  Numerical:", length(x$variables$numerical), "variables\n")
  }
  if (!is.null(x$variables$dates)) {
    cat("  Dates:", length(x$variables$dates), "variables\n")
  }
  cat("\n")
  
  # Print missing values summary
  missing_vals <- x$summary$missing_values
  if (sum(missing_vals) > 0) {
    cat("Missing Values:\n")
    for (i in seq_along(missing_vals)) {
      if (missing_vals[i] > 0) {
        cat("  ", names(missing_vals)[i], ":", missing_vals[i], 
            "(", round(x$summary$missing_percentage[i], 2), "%)\n")
      }
    }
    cat("\n")
  } else {
    cat("No missing values found.\n\n")
  }
  
  # Print target variable info if available
  if (!is.null(x$relationships$target)) {
    cat("Target Variable Analysis:\n")
    cat("  Target type:", x$relationships$target$target_type, "\n")
    
    if (x$relationships$target$target_type == "numeric" && !is.null(x$relationships$target$correlations)) {
      cat("  Top correlations with target:\n")
      top_cors <- head(x$relationships$target$correlations, 5)
      for (i in seq_along(top_cors)) {
        cat("    ", names(top_cors)[i], ":", round(top_cors[i], 3), "\n")
      }
    }
    cat("\n")
  }
  
  cat("Use summary() for more detailed information.\n")
}

#' Summary method for eda_results objects
#'
#' @param object An object of class eda_results
#' @param ... Additional arguments
#' @export
#' @keywords internal
summary.eda_results <- function(object, ...) {
  print(object, ...)
  
  # Print more detailed information
  cat("\nDetailed Variable Summaries:\n")
  cat("==========================\n\n")
  
  # Numerical variables
  if (!is.null(object$variables$numerical)) {
    cat("Numerical Variables:\n")
    cat("-------------------\n")
    
    for (var_name in names(object$variables$numerical)) {
      var_info <- object$variables$numerical[[var_name]]
      cat(var_name, ":\n")
      cat("  Min:", round(var_info$summary$min, 2), 
          "| Q1:", round(var_info$summary$q1, 2),
          "| Median:", round(var_info$summary$median, 2), "\n")
      cat("  Mean:", round(var_info$summary$mean, 2),
          "| Q3:", round(var_info$summary$q3, 2),
          "| Max:", round(var_info$summary$max, 2), "\n")
      cat("  SD:", round(var_info$summary$sd, 2),
          "| IQR:", round(var_info$summary$iqr, 2), "\n")
      
      if (!is.null(var_info$target_correlation)) {
        cat("  Correlation with target:", round(var_info$target_correlation, 3), "\n")
      }
      
      cat("  Missing:", var_info$missing, "(", round(var_info$missing_pct, 2), "%)\n")
      cat("  Outliers:", var_info$outliers, "(", round(var_info$outliers_pct, 2), "%)\n")
      cat("\n")
    }
  }
  
  # Categorical variables (top 5 only)
  if (!is.null(object$variables$categorical)) {
    cat("Categorical Variables (Top 5 levels):\n")
    cat("----------------------------------\n")
    
    for (var_name in names(object$variables$categorical)) {
      var_info <- object$variables$categorical[[var_name]]
      cat(var_name, ":\n")
      cat("  Levels:", var_info$n_levels, 
          "| Unique:", var_info$n_unique, 
          "| Missing:", var_info$missing, "(", round(var_info$missing_pct, 2), "%)\n")
      
      # Print top 5 levels
      top_levels <- head(var_info$summary, 5)
      for (i in 1:nrow(top_levels)) {
        cat("  ", top_levels$Level[i], ":", top_levels$Count[i], 
            "(", round(top_levels$Percentage[i], 2), "%)\n")
      }
      cat("\n")
    }
  }
  
  # Date variables
  if (!is.null(object$variables$dates)) {
    cat("Date Variables:\n")
    cat("--------------\n")
    
    for (var_name in names(object$variables$dates)) {
      var_info <- object$variables$dates[[var_name]]
      cat(var_name, ":\n")
      cat("  Range:", as.character(var_info$min_date), "to", as.character(var_info$max_date), "\n")
      cat("  Duration:", round(var_info$range_days, 1), "days\n")
      cat("  Unique values:", var_info$n_unique, "\n")
      cat("  Missing:", var_info$missing, "(", round(var_info$missing_pct, 2), "%)\n")
      cat("  Has time component:", var_info$has_time_component, "\n")
      cat("\n")
    }
  }
  
  invisible(object)
}

