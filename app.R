library(shiny)
library(bslib)
library(DT)
library(plotly)
library(fable)
library(fabletools)
library(tsibble)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Load and prepare data
load_wine_data <- function() {
  data <- read_csv("AustralianWines.csv", show_col_types = FALSE) %>%
    mutate(
      Month = as.Date(paste0("01-", Month), format = "%d-%b-%y"),
      Rose = as.numeric(Rose)  
    ) %>%
    tidyr::pivot_longer(
      cols = c(Fortified, Red, Rose, sparkling, `Sweet white`, `Dry white`),
      names_to = "Varietal",
      values_to = "Sales"
    ) %>%
    as_tsibble(index = Month, key = Varietal)
  
  return(data)
}

wine_data <- load_wine_data()

ui <- page_sidebar(
  title = "Australian Wine Sales Forecasting",
  
  sidebar = sidebar(
    selectInput("varietals", "Select Varietal(s):",
                choices = unique(wine_data$Varietal),
                selected = c("Red", "Dry white"),
                multiple = TRUE),
    
    dateRangeInput("date_range", "Date Range:",
                   start = min(wine_data$Month),
                   end = max(wine_data$Month),
                   min = min(wine_data$Month),
                   max = max(wine_data$Month)),
    
    dateInput("train_end", "Training End Date:",
              value = as.Date("2008-01-01"),
              min = min(wine_data$Month),
              max = max(wine_data$Month)),
    
    numericInput("forecast_horizon", "Forecast Horizon (months):",
                 value = 12, min = 1, max = 24)
  ),
  
  navset_card_tab(
    nav_panel("Overview",
              plotlyOutput("overview_plot", height = "500px")
    ),
    
    nav_panel("Forecasts",
              plotlyOutput("forecast_plot", height = "600px")
    ),
    
    nav_panel("Model Specifications",
              DTOutput("model_specs")
    ),
    
    nav_panel("Accuracy Metrics",
              DTOutput("accuracy_table")
    ),
    
    nav_panel("About",
              card(
                card_header("About This App"),
                card_body(
                  h4("Dataset"),
                  p("This app uses Australian wine sales data structured as a monthly tsibble with Month as index and Varietal as key."),
                  
                  h4("Modeling Approach"),
                  tags$ul(
                    tags$li("TSLM: Linear regression with trend and seasonal components"),
                    tags$li("ETS: Exponential smoothing with automatic specification"),
                    tags$li("ARIMA: Auto-regressive integrated moving average with automatic specification")
                  ),
                  
                  h4("Features"),
                  tags$ul(
                    tags$li("Interactive varietal and date range selection"),
                    tags$li("Customizable train/validation split"),
                    tags$li("Multiple forecasting models with automatic specification"),
                    tags$li("Comparative forecast visualization with prediction intervals"),
                    tags$li("Model performance metrics for both training and validation periods"),
                    tags$li("Decomposition view for understanding seasonality patterns")
                  )
                )
              )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$varietals, input$date_range)
    
    wine_data %>%
      filter(Varietal %in% input$varietals,
             Month >= input$date_range[1],
             Month <= input$date_range[2])
  })
  
  # Training and validation splits
  train_data <- reactive({
    req(input$train_end)
    filtered_data() %>%
      filter_index(~ input$train_end)
  })
  
  validation_data <- reactive({
    req(input$train_end)
    filtered_data() %>%
      filter_index(input$train_end + months(1) ~ .)
  })
  
  # Model fitting
  models <- reactive({
    req(train_data())
    
    train_data() %>%
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  # Forecasts
  forecasts <- reactive({
    req(models(), input$forecast_horizon)
    
    models() %>%
      forecast(h = input$forecast_horizon)
  })
  
  # Validation forecasts for accuracy
  validation_forecasts <- reactive({
    req(models(), validation_data())
    
    if(nrow(validation_data()) > 0) {
      val_periods <- nrow(validation_data()) / length(input$varietals)
      models() %>%
        forecast(h = val_periods)
    } else {
      NULL
    }
  })
  
  # Overview plot
  output$overview_plot <- renderPlotly({
    req(filtered_data())
    
    p <- filtered_data() %>%
      ggplot(aes(x = Month, y = Sales, color = Varietal)) +
      geom_line(size = 1) +
      geom_vline(xintercept = as.numeric(as.Date(input$train_end)), 
                 linetype = "dashed", alpha = 0.7) +
      labs(title = "Wine Sales Overview",
           subtitle = "Dashed line shows train/validation split",
           x = "Month", y = "Sales") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if(length(input$varietals) > 1) {
      p <- p + facet_wrap(~Varietal, scales = "free_y")
    }
    
    ggplotly(p)
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    req(forecasts(), filtered_data())
    
    p <- filtered_data() %>%
      autoplot(Sales) +
      autolayer(forecasts(), alpha = 0.7, size = 1) +
      geom_vline(xintercept = as.numeric(as.Date(input$train_end)), 
                 linetype = "dashed", alpha = 0.7) +
      labs(title = "Sales Forecasts Comparison",
           subtitle = "Dashed line shows train/validation split",
           x = "Month", y = "Sales") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if(length(input$varietals) > 1) {
      p <- p + facet_wrap(~Varietal, scales = "free_y")
    }
    
    ggplotly(p)
  })
  
  # Model specifications table
  output$model_specs <- renderDT({
    req(models())
    
    specs <- models() %>%
      pivot_longer(cols = c(TSLM, ETS, ARIMA), names_to = "Model", values_to = "Fit") %>%
      mutate(
        Specification = map_chr(Fit, function(x) {
          if(class(x)[1] == "TSLM") {
            "TSLM(Sales ~ trend() + season())"
          } else if(class(x)[1] == "ETS") {
            paste0("ETS(", x$method, ")")
          } else if(class(x)[1] == "ARIMA") {
            paste0("ARIMA(", paste(x$spec$p, x$spec$d, x$spec$q, sep=","), 
                   ")(", paste(x$spec$P, x$spec$D, x$spec$Q, sep=","), ")[", x$spec$period, "]")
          } else {
            "Unknown"
          }
        })
      ) %>%
      select(Varietal, Model, Specification)
    
    datatable(specs, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Accuracy metrics table
  output$accuracy_table <- renderDT({
    req(models())
    
    # Training accuracy
    train_acc <- models() %>%
      accuracy() %>%
      mutate(Set = "Training") %>%
      select(Varietal, .model, Set, RMSE, MAE, MAPE)
    
    # Validation accuracy
    val_acc <- NULL
    if(!is.null(validation_forecasts()) && nrow(validation_data()) > 0) {
      val_acc <- validation_forecasts() %>%
        accuracy(validation_data()) %>%
        mutate(Set = "Validation") %>%
        select(Varietal, .model, Set, RMSE, MAE, MAPE)
    }
    
    if(!is.null(val_acc)) {
      combined_acc <- bind_rows(train_acc, val_acc)
    } else {
      combined_acc <- train_acc
    }
    
    combined_acc <- combined_acc %>%
      arrange(Varietal, .model, Set) %>%
      mutate(across(c(RMSE, MAE, MAPE), ~round(.x, 3)))
    
    datatable(combined_acc, 
              options = list(pageLength = 15, scrollX = TRUE),
              colnames = c("Varietal", "Model", "Dataset", "RMSE", "MAE", "MAPE"))
  })
}

shinyApp(ui = ui, server = server)
