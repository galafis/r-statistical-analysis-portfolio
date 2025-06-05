#' Interactive Shiny App for R Statistical Analysis Portfolio
#'
#' This Shiny app demonstrates the key features of the rstatportfolio package.
#' It provides an interactive interface for exploratory data analysis,
#' statistical modeling, and time series forecasting.
#'
#' @author Gabriel Demetrios Lafis
#' @version 1.2.0

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatportfolio)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "R Statistical Analysis Portfolio"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to R Statistical Analysis Portfolio",
            status = "primary",
            solidHeader = TRUE,
            h3("Interactive Statistical Analysis Dashboard"),
            p("This dashboard demonstrates the key features of the rstatportfolio package."),
            p("Use the sidebar menu to navigate between different modules:"),
            tags$ul(
              tags$li(strong("Data Explorer:"), "Load and explore datasets"),
              tags$li(strong("EDA:"), "Perform exploratory data analysis"),
              tags$li(strong("Modeling:"), "Build and compare statistical models"),
              tags$li(strong("Time Series:"), "Analyze and forecast time series data")
            ),
            p("The dashboard includes sample datasets for demonstration purposes.")
          )
        ),
        fluidRow(
          valueBox(
            "Data Explorer", 
            "Explore and visualize datasets", 
            icon = icon("table"), 
            color = "blue",
            width = 3
          ),
          valueBox(
            "EDA", 
            "Exploratory Data Analysis", 
            icon = icon("chart-bar"), 
            color = "green",
            width = 3
          ),
          valueBox(
            "Modeling", 
            "Statistical Modeling", 
            icon = icon("cogs"), 
            color = "purple",
            width = 3
          ),
          valueBox(
            "Time Series", 
            "Time Series Analysis", 
            icon = icon("chart-line"), 
            color = "red",
            width = 3
          )
        )
      ),
      
      # Data Explorer tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 3,
            title = "Dataset Selection",
            status = "primary",
            solidHeader = TRUE,
            radioButtons(
              "dataset",
              "Select Dataset:",
              choices = c(
                "mtcars" = "mtcars",
                "iris" = "iris",
                "diamonds (sample)" = "diamonds",
                "AirPassengers" = "AirPassengers"
              ),
              selected = "mtcars"
            ),
            hr(),
            checkboxInput("show_summary", "Show Summary Statistics", value = TRUE),
            checkboxInput("show_structure", "Show Data Structure", value = TRUE)
          ),
          tabBox(
            width = 9,
            title = "Data View",
            tabPanel(
              "Data Table",
              br(),
              DTOutput("data_table")
            ),
            tabPanel(
              "Summary",
              verbatimTextOutput("data_summary")
            ),
            tabPanel(
              "Structure",
              verbatimTextOutput("data_structure")
            )
          )
        )
      ),
      
      # EDA tab
      tabItem(
        tabName = "eda",
        fluidRow(
          box(
            width = 3,
            title = "EDA Options",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("variable_selector"),
            selectInput(
              "plot_type",
              "Plot Type:",
              choices = c(
                "Histogram" = "hist",
                "Density Plot" = "density",
                "Box Plot" = "box",
                "Scatter Plot" = "scatter",
                "Correlation Plot" = "correlation"
              ),
              selected = "hist"
            ),
            conditionalPanel(
              condition = "input.plot_type == 'scatter'",
              uiOutput("y_variable_selector")
            ),
            hr(),
            actionButton("run_eda", "Run EDA", icon = icon("play"), 
                        class = "btn-primary")
          ),
          tabBox(
            width = 9,
            title = "EDA Results",
            tabPanel(
              "Plots",
              plotlyOutput("eda_plot", height = "500px")
            ),
            tabPanel(
              "Statistics",
              verbatimTextOutput("eda_stats")
            )
          )
        )
      ),
      
      # Modeling tab
      tabItem(
        tabName = "modeling",
        fluidRow(
          box(
            width = 3,
            title = "Modeling Options",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("target_selector"),
            uiOutput("predictors_selector"),
            selectInput(
              "model_type",
              "Model Type:",
              choices = c(
                "Regression" = "regression",
                "Classification" = "classification"
              ),
              selected = "regression"
            ),
            checkboxGroupInput(
              "models",
              "Models to Include:",
              choices = c(
                "Linear/Logistic" = "linear",
                "Random Forest" = "random_forest"
              ),
              selected = c("linear", "random_forest")
            ),
            sliderInput(
              "test_size",
              "Test Size (%):",
              min = 10,
              max = 50,
              value = 20
            ),
            hr(),
            actionButton("run_model", "Run Models", icon = icon("play"), 
                        class = "btn-primary")
          ),
          tabBox(
            width = 9,
            title = "Model Results",
            tabPanel(
              "Model Comparison",
              plotlyOutput("model_comparison", height = "300px"),
              hr(),
              DTOutput("model_metrics")
            ),
            tabPanel(
              "Variable Importance",
              plotlyOutput("var_importance", height = "500px")
            ),
            tabPanel(
              "Predictions",
              plotlyOutput("predictions_plot", height = "500px")
            )
          )
        )
      ),
      
      # Time Series tab
      tabItem(
        tabName = "timeseries",
        fluidRow(
          box(
            width = 3,
            title = "Time Series Options",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "ts_dataset",
              "Select Dataset:",
              choices = c(
                "AirPassengers" = "AirPassengers"
              ),
              selected = "AirPassengers"
            ),
            numericInput(
              "forecast_horizon",
              "Forecast Horizon:",
              value = 12,
              min = 1,
              max = 48
            ),
            checkboxGroupInput(
              "ts_models",
              "Models to Include:",
              choices = c(
                "Auto ARIMA" = "auto",
                "ETS" = "ets"
              ),
              selected = c("auto", "ets")
            ),
            checkboxInput("seasonal", "Include Seasonality", value = TRUE),
            hr(),
            actionButton("run_ts", "Run Analysis", icon = icon("play"), 
                        class = "btn-primary")
          ),
          tabBox(
            width = 9,
            title = "Time Series Results",
            tabPanel(
              "Data & Forecast",
              plotlyOutput("ts_forecast", height = "500px")
            ),
            tabPanel(
              "Decomposition",
              plotlyOutput("ts_decomposition", height = "500px")
            ),
            tabPanel(
              "Diagnostics",
              plotlyOutput("ts_diagnostics", height = "500px")
            )
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About R Statistical Analysis Portfolio",
            status = "primary",
            solidHeader = TRUE,
            h3("Package Information"),
            p("The rstatportfolio package provides a comprehensive framework for statistical analysis in R."),
            p("Version: 1.2.0"),
            p("Author: Gabriel Demetrios Lafis"),
            p("Developed as part of the UC Davis California's College Town Statistical Computing program."),
            h3("Features"),
            tags$ul(
              tags$li("Exploratory Data Analysis"),
              tags$li("Statistical Modeling"),
              tags$li("Time Series Analysis"),
              tags$li("Interactive Visualization")
            ),
            h3("Resources"),
            tags$ul(
              tags$li(tags$a(href = "https://github.com/galafis/r-statistical-analysis-portfolio", "GitHub Repository")),
              tags$li(tags$a(href = "https://galafis.github.io/r-statistical-analysis-portfolio", "Documentation"))
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    eda_results = NULL,
    model_results = NULL,
    ts_results = NULL
  )
  
  # Load selected dataset
  observe({
    if (input$dataset == "mtcars") {
      rv$data <- mtcars
    } else if (input$dataset == "iris") {
      rv$data <- iris
    } else if (input$dataset == "diamonds") {
      # Load a sample of diamonds dataset
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        rv$data <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]
      }
    } else if (input$dataset == "AirPassengers") {
      # Convert time series to data frame
      ap_df <- data.frame(
        date = time(AirPassengers),
        passengers = as.numeric(AirPassengers)
      )
      rv$data <- ap_df
    }
  })
  
  # Data table output
  output$data_table <- renderDT({
    datatable(rv$data, options = list(pageLength = 10))
  })
  
  # Data summary output
  output$data_summary <- renderPrint({
    if (input$show_summary) {
      summary(rv$data)
    }
  })
  
  # Data structure output
  output$data_structure <- renderPrint({
    if (input$show_structure) {
      str(rv$data)
    }
  })
  
  # Variable selector UI
  output$variable_selector <- renderUI({
    req(rv$data)
    vars <- names(rv$data)
    selectInput("variable", "Select Variable:", choices = vars, selected = vars[1])
  })
  
  # Y variable selector UI for scatter plots
  output$y_variable_selector <- renderUI({
    req(rv$data)
    vars <- names(rv$data)
    selectInput("y_variable", "Select Y Variable:", choices = vars, selected = vars[2])
  })
  
  # Target selector UI
  output$target_selector <- renderUI({
    req(rv$data)
    vars <- names(rv$data)
    selectInput("target", "Select Target Variable:", choices = vars, selected = vars[1])
  })
  
  # Predictors selector UI
  output$predictors_selector <- renderUI({
    req(rv$data, input$target)
    vars <- setdiff(names(rv$data), input$target)
    checkboxGroupInput("predictors", "Select Predictors:", choices = vars, selected = vars[1:min(3, length(vars))])
  })
  
  # EDA plot
  output$eda_plot <- renderPlotly({
    req(rv$data, input$variable)
    
    if (input$plot_type == "hist") {
      # Histogram
      p <- ggplot(rv$data, aes_string(x = input$variable)) +
        geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.7) +
        labs(title = paste("Histogram of", input$variable),
             x = input$variable,
             y = "Count") +
        theme_minimal()
      
    } else if (input$plot_type == "density") {
      # Density plot
      p <- ggplot(rv$data, aes_string(x = input$variable)) +
        geom_density(fill = "#4e79a7", alpha = 0.7) +
        labs(title = paste("Density of", input$variable),
             x = input$variable,
             y = "Density") +
        theme_minimal()
      
    } else if (input$plot_type == "box") {
      # Box plot
      p <- ggplot(rv$data, aes_string(y = input$variable)) +
        geom_boxplot(fill = "#4e79a7", alpha = 0.7) +
        labs(title = paste("Boxplot of", input$variable),
             y = input$variable) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    } else if (input$plot_type == "scatter") {
      # Scatter plot
      req(input$y_variable)
      p <- ggplot(rv$data, aes_string(x = input$variable, y = input$y_variable)) +
        geom_point(alpha = 0.7, color = "#4e79a7") +
        geom_smooth(method = "loess", color = "#e15759", se = TRUE) +
        labs(title = paste(input$variable, "vs", input$y_variable),
             x = input$variable,
             y = input$y_variable) +
        theme_minimal()
      
    } else if (input$plot_type == "correlation") {
      # Correlation plot
      # Get numeric columns
      num_data <- rv$data[, sapply(rv$data, is.numeric), drop = FALSE]
      
      if (ncol(num_data) < 2) {
        return(NULL)
      }
      
      # Calculate correlation matrix
      cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
      
      # Convert to long format for ggplot
      cor_df <- as.data.frame(as.table(cor_matrix))
      names(cor_df) <- c("Var1", "Var2", "Correlation")
      
      # Create correlation heatmap
      p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#e15759", mid = "white", high = "#4e79a7", 
                            midpoint = 0, limits = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), 
                 color = "black", size = 3) +
        labs(title = "Correlation Matrix",
             x = "",
             y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  # EDA statistics
  output$eda_stats <- renderPrint({
    req(rv$data, input$variable)
    
    # Get variable
    var <- rv$data[[input$variable]]
    
    # Print statistics
    cat("Variable:", input$variable, "\n\n")
    
    if (is.numeric(var)) {
      # Numeric variable
      cat("Type: Numeric\n\n")
      cat("Summary Statistics:\n")
      print(summary(var))
      
      cat("\nMissing Values:", sum(is.na(var)), "(", round(mean(is.na(var)) * 100, 2), "%)\n")
      cat("Unique Values:", length(unique(var)), "\n")
      
      # Calculate additional statistics
      cat("\nAdditional Statistics:\n")
      cat("Standard Deviation:", round(sd(var, na.rm = TRUE), 4), "\n")
      cat("Variance:", round(var(var, na.rm = TRUE), 4), "\n")
      cat("Skewness:", round(mean((var - mean(var, na.rm = TRUE))^3, na.rm = TRUE) / 
                            sd(var, na.rm = TRUE)^3, 4), "\n")
      cat("Kurtosis:", round(mean((var - mean(var, na.rm = TRUE))^4, na.rm = TRUE) / 
                           sd(var, na.rm = TRUE)^4, 4), "\n")
      
    } else if (is.factor(var) || is.character(var)) {
      # Categorical variable
      cat("Type:", if(is.factor(var)) "Factor" else "Character", "\n\n")
      cat("Frequency Table:\n")
      print(table(var, useNA = "ifany"))
      
      cat("\nMissing Values:", sum(is.na(var)), "(", round(mean(is.na(var)) * 100, 2), "%)\n")
      cat("Unique Values:", length(unique(var)), "\n")
      
    } else if (inherits(var, c("Date", "POSIXct", "POSIXlt"))) {
      # Date/time variable
      cat("Type: Date/Time\n\n")
      cat("Range:", min(var, na.rm = TRUE), "to", max(var, na.rm = TRUE), "\n")
      cat("Duration:", difftime(max(var, na.rm = TRUE), min(var, na.rm = TRUE), units = "days"), "days\n")
      
      cat("\nMissing Values:", sum(is.na(var)), "(", round(mean(is.na(var)) * 100, 2), "%)\n")
      cat("Unique Values:", length(unique(var)), "\n")
    }
  })
  
  # Run EDA
  observeEvent(input$run_eda, {
    req(rv$data)
    
    # Identify variable types
    cat_vars <- names(rv$data)[sapply(rv$data, function(x) is.factor(x) || is.character(x))]
    num_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    date_vars <- names(rv$data)[sapply(rv$data, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))]
    
    # Run EDA
    withProgress(message = "Running EDA...", {
      rv$eda_results <- tryCatch({
        perform_eda(
          data = rv$data,
          cat_vars = cat_vars,
          num_vars = num_vars,
          date_vars = date_vars,
          plots = FALSE
        )
      }, error = function(e) {
        showNotification(paste("Error in EDA:", e$message), type = "error")
        return(NULL)
      })
    })
    
    if (!is.null(rv$eda_results)) {
      showNotification("EDA completed successfully!", type = "message")
    }
  })
  
  # Run models
  observeEvent(input$run_model, {
    req(rv$data, input$target, input$predictors, input$models)
    
    # Check if we have enough predictors
    if (length(input$predictors) == 0) {
      showNotification("Please select at least one predictor", type = "warning")
      return()
    }
    
    # Check if target is appropriate for model type
    target_var <- rv$data[[input$target]]
    is_categorical <- is.factor(target_var) || is.character(target_var)
    
    if (input$model_type == "regression" && is_categorical) {
      showNotification("Target variable must be numeric for regression", type = "warning")
      return()
    }
    
    if (input$model_type == "classification" && !is_categorical) {
      # Convert numeric target to factor for classification
      rv$data[[input$target]] <- factor(ifelse(target_var > median(target_var, na.rm = TRUE), 
                                             "high", "low"))
      showNotification("Numeric target converted to binary factor", type = "message")
    }
    
    # Run models
    withProgress(message = "Running models...", {
      if (input$model_type == "regression") {
        rv$model_results <- tryCatch({
          build_regression_models(
            data = rv$data,
            target = input$target,
            predictors = input$predictors,
            model_types = input$models,
            test_size = input$test_size / 100,
            seed = 123
          )
        }, error = function(e) {
          showNotification(paste("Error in modeling:", e$message), type = "error")
          return(NULL)
        })
      } else {
        rv$model_results <- tryCatch({
          build_classification_models(
            data = rv$data,
            target = input$target,
            predictors = input$predictors,
            model_types = input$models,
            test_size = input$test_size / 100,
            seed = 123
          )
        }, error = function(e) {
          showNotification(paste("Error in modeling:", e$message), type = "error")
          return(NULL)
        })
      }
    })
    
    if (!is.null(rv$model_results)) {
      showNotification("Models built successfully!", type = "message")
    }
  })
  
  # Model comparison plot
  output$model_comparison <- renderPlotly({
    req(rv$model_results, rv$model_results$comparison)
    
    # Get comparison data
    comp_data <- rv$model_results$comparison
    
    # Determine metric to plot
    if ("rmse" %in% names(comp_data)) {
      # Regression
      metric <- "rmse"
      title <- "Model Comparison by RMSE"
      y_label <- "RMSE (lower is better)"
    } else {
      # Classification
      metric <- "accuracy"
      title <- "Model Comparison by Accuracy"
      y_label <- "Accuracy"
    }
    
    # Create plot
    p <- ggplot(comp_data, aes_string(x = "model", y = metric, fill = "model")) +
      geom_bar(stat = "identity") +
      labs(title = title,
           x = "Model",
           y = y_label) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # For metrics where lower is better, flip the y-axis
    if (metric %in% c("rmse", "mae")) {
      p <- p + scale_y_reverse()
    }
    
    ggplotly(p)
  })
  
  # Model metrics table
  output$model_metrics <- renderDT({
    req(rv$model_results, rv$model_results$comparison)
    datatable(rv$model_results$comparison, options = list(pageLength = 5))
  })
  
  # Variable importance plot
  output$var_importance <- renderPlotly({
    req(rv$model_results, rv$model_results$variable_importance)
    
    # Get best model
    best_model <- rv$model_results$comparison$model[1]
    
    # Get variable importance
    var_imp <- rv$model_results$variable_importance[[best_model]]
    
    if (is.null(var_imp) || nrow(var_imp) == 0) {
      return(NULL)
    }
    
    # Sort by importance
    var_imp <- var_imp[order(var_imp$importance), ]
    
    # Create plot
    p <- ggplot(var_imp, aes(x = reorder(variable, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "#4e79a7") +
      labs(title = paste("Variable Importance for", best_model, "Model"),
           x = "Variable",
           y = "Importance (%)") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Predictions plot
  output$predictions_plot <- renderPlotly({
    req(rv$model_results, rv$model_results$test_predictions)
    
    # Get best model
    best_model <- rv$model_results$comparison$model[1]
    
    # Get predictions
    preds <- rv$model_results$test_predictions[[best_model]]
    
    if (is.null(preds) || nrow(preds) == 0) {
      return(NULL)
    }
    
    # Check if regression or classification
    if ("predicted" %in% names(preds)) {
      # Regression
      p <- ggplot(preds, aes(x = actual, y = predicted)) +
        geom_point(alpha = 0.7, color = "#4e79a7") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = paste("Actual vs Predicted Values for", best_model, "Model"),
             x = "Actual",
             y = "Predicted") +
        theme_minimal()
      
    } else {
      # Classification
      p <- ggplot(preds, aes(x = actual, y = predicted_prob, fill = actual)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Prediction Probabilities by Class for", best_model, "Model"),
             x = "Actual Class",
             y = "Predicted Probability") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Run time series analysis
  observeEvent(input$run_ts, {
    req(input$ts_dataset, input$forecast_horizon, input$ts_models)
    
    # Get time series data
    if (input$ts_dataset == "AirPassengers") {
      ts_data <- AirPassengers
    } else {
      showNotification("Dataset not available", type = "error")
      return()
    }
    
    # Run time series analysis
    withProgress(message = "Running time series analysis...", {
      rv$ts_results <- tryCatch({
        analyze_time_series(
          data = ts_data,
          h = input$forecast_horizon,
          models = input$ts_models,
          seasonal = input$seasonal
        )
      }, error = function(e) {
        showNotification(paste("Error in time series analysis:", e$message), type = "error")
        return(NULL)
      })
    })
    
    if (!is.null(rv$ts_results)) {
      showNotification("Time series analysis completed successfully!", type = "message")
    }
  })
  
  # Time series forecast plot
  output$ts_forecast <- renderPlotly({
    req(rv$ts_results, rv$ts_results$forecasts)
    
    # Get best model
    best_model <- rv$ts_results$best_model
    
    if (is.null(best_model) || is.null(rv$ts_results$forecasts[[best_model]])) {
      # Use first available model
      best_model <- names(rv$ts_results$forecasts)[1]
    }
    
    # Get forecast
    forecast <- rv$ts_results$forecasts[[best_model]]
    
    # Convert to data frame for plotting
    original <- data.frame(
      time = time(forecast$x),
      value = as.numeric(forecast$x),
      type = "Observed"
    )
    
    forecasted <- data.frame(
      time = time(forecast$mean),
      value = as.numeric(forecast$mean),
      type = "Forecast"
    )
    
    lower <- data.frame(
      time = time(forecast$mean),
      value = as.numeric(forecast$lower[, 2]),
      type = "Lower 95%"
    )
    
    upper <- data.frame(
      time = time(forecast$mean),
      value = as.numeric(forecast$upper[, 2]),
      type = "Upper 95%"
    )
    
    # Combine data
    plot_data <- rbind(original, forecasted)
    
    # Create plot
    p <- ggplot() +
      geom_line(data = original, aes(x = time, y = value), color = "#4e79a7") +
      geom_line(data = forecasted, aes(x = time, y = value), color = "#e15759") +
      geom_ribbon(data = forecasted, aes(x = time, ymin = lower$value, ymax = upper$value), 
                 fill = "#e15759", alpha = 0.2) +
      labs(title = paste("Time Series Forecast using", best_model, "model"),
           x = "Time",
           y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Time series decomposition plot
  output$ts_decomposition <- renderPlotly({
    req(rv$ts_results, rv$ts_results$decomposition)
    
    # Get decomposition
    decomp <- NULL
    
    if (!is.null(rv$ts_results$decomposition$stl)) {
      decomp <- rv$ts_results$decomposition$stl
      decomp_type <- "STL"
    } else if (!is.null(rv$ts_results$decomposition$additive)) {
      decomp <- rv$ts_results$decomposition$additive
      decomp_type <- "Additive"
    } else if (!is.null(rv$ts_results$decomposition$multiplicative)) {
      decomp <- rv$ts_results$decomposition$multiplicative
      decomp_type <- "Multiplicative"
    } else {
      return(NULL)
    }
    
    # Convert to data frame for plotting
    if (decomp_type == "STL") {
      # STL decomposition
      plot_data <- data.frame(
        time = rep(time(decomp$time.series), 4),
        value = c(
          as.numeric(decomp$time.series[, "raw"]),
          as.numeric(decomp$time.series[, "trend"]),
          as.numeric(decomp$time.series[, "seasonal"]),
          as.numeric(decomp$time.series[, "remainder"])
        ),
        component = rep(c("Observed", "Trend", "Seasonal", "Remainder"), 
                       each = length(decomp$time.series[, "raw"]))
      )
    } else {
      # Classical decomposition
      plot_data <- data.frame(
        time = rep(time(decomp$x), 4),
        value = c(
          as.numeric(decomp$x),
          as.numeric(decomp$trend),
          as.numeric(decomp$seasonal),
          as.numeric(decomp$random)
        ),
        component = rep(c("Observed", "Trend", "Seasonal", "Remainder"), 
                       each = length(decomp$x))
      )
    }
    
    # Create plot
    p <- ggplot(plot_data, aes(x = time, y = value)) +
      geom_line() +
      facet_wrap(~ component, scales = "free_y", ncol = 1) +
      labs(title = paste(decomp_type, "Decomposition"),
           x = "Time",
           y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Time series diagnostics plot
  output$ts_diagnostics <- renderPlotly({
    req(rv$ts_results, rv$ts_results$acf_pacf)
    
    # Get ACF and PACF
    acf_values <- rv$ts_results$acf_pacf$acf
    pacf_values <- rv$ts_results$acf_pacf$pacf
    
    # Convert ACF to data frame
    acf_df <- data.frame(
      lag = acf_values$lag,
      acf = acf_values$acf,
      type = "ACF"
    )
    
    # Convert PACF to data frame
    pacf_df <- data.frame(
      lag = pacf_values$lag,
      acf = pacf_values$acf,
      type = "PACF"
    )
    
    # Combine data
    plot_data <- rbind(acf_df, pacf_df)
    
    # Create plot
    p <- ggplot(plot_data, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", fill = "#4e79a7") +
      geom_hline(yintercept = c(0, qnorm(0.975) / sqrt(length(rv$ts_results$data)), 
                               -qnorm(0.975) / sqrt(length(rv$ts_results$data))), 
                linetype = c("solid", "dashed", "dashed"), 
                color = c("black", "blue", "blue")) +
      facet_wrap(~ type, ncol = 1) +
      labs(title = "ACF and PACF",
           x = "Lag",
           y = "Correlation") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

