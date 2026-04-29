library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(rpart)
library(rpart.plot)
# NOTE: UI should have a year selector
# selectInput("year_select", "Select Year:",
#             choices = c(2022, 2023, 2024),
#             selected = 2024)
# NOTE 2: USE THE ACTUAL COLUMN NAMES
# "tbl","Year","quarter","citymarketid_1",
# "citymarketid_2","city1","city2","airportid_1",
# "airportid_2","airport_1","airport_2","nsmiles",
# "passengers","fare","carrier_lg","large_ms",
# "fare_lg","carrier_low","lf_ms","fare_low",
# "Geocoded_City1","Geocoded_City1 (address)",
# "Geocoded_City1 (city)","Geocoded_City1 (state)",
# "Geocoded_City1 (zip)","Geocoded_City2",
# "Geocoded_City2 (address)","Geocoded_City2 (city)",
# "Geocoded_City2 (state)","Geocoded_City2 (zip)",
# "tbl1apk"

# ============================================================================
# DATA LOADING AND CLEANING
# ============================================================================

list.files("app/data/")

load_and_clean <- function() {
  # load data
  airfare_data_a <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  airfare_data_b <- read.csv("data/Consumer_Airfare_Report_Table6_filtered.csv", stringsAsFactors = FALSE)
  
  common_cols <- intersect(names(airfare_data_a), names(airfare_data_b))
  airfare_data <- rbind(airfare_data_a[, common_cols], airfare_data_b[, common_cols])
  # data cleaning
  # 1. remove dollar signs and convert fare to numeric
  airfare_data$fare <- as.numeric(gsub("\\$", "", airfare_data$fare))
  
  # 2. convert quarter to numeric
  airfare_data$quarter <- as.numeric(airfare_data$quarter)
  # 3. convert nsmiles - remove commas and convert to numeric
  airfare_data$nsmiles <- as.numeric(gsub(",", "", airfare_data$nsmiles))
  
  # 4. remove all missing vals in important columns
  airfare_data <- airfare_data[!is.na(airfare_data$fare), ]
  airfare_data <- airfare_data[!is.na(airfare_data$quarter), ]
  airfare_data <- airfare_data[!is.na(airfare_data$nsmiles), ]
  
  # 5. filter for 2022 - 2024 only
  airfare_data <- airfare_data[airfare_data$Year >= 2022 & airfare_data$Year <= 2024, ]
  
  airfare2022 <- airfare_data[airfare_data$Year == 2022, ]
  airfare2023 <- airfare_data[airfare_data$Year == 2023, ]
  airfare2024 <- airfare_data[airfare_data$Year == 2024, ]
  
  # 6. remove bad data
  airfare2022 <- airfare2022[airfare2022$fare > 0, ]
  airfare2022 <- airfare2022[airfare2022$nsmiles > 0, ]
  
  airfare2023 <- airfare2023[airfare2023$fare > 0, ]
  airfare2023 <- airfare2023[airfare2023$nsmiles > 0, ]
  
  airfare2024 <- airfare2024[airfare2024$fare > 0, ]
  airfare2024 <- airfare2024[airfare2024$nsmiles > 0, ]
  
  # 7. remove extreme outliers
  airfare2022 <- airfare2022[airfare2022$fare < 2000, ]
  airfare2023 <- airfare2023[airfare2023$fare < 2000, ]
  airfare2024 <- airfare2024[airfare2024$fare < 2000, ]
  
  # 8. convert quarter to factor for proper ordering
  airfare2022$quarter <- as.factor(airfare2022$quarter)
  airfare2023$quarter <- as.factor(airfare2023$quarter)
  airfare2024$quarter <- as.factor(airfare2024$quarter)
  
  # 9: create quarter labels for readability
  create_quarter_labels <- function(data) {
    data$quarter_label <- NA
    data$quarter_label[data$quarter == 1] <- "Q1 (Jan-Mar)"
    data$quarter_label[data$quarter == 2] <- "Q2 (Apr-Jun)"
    data$quarter_label[data$quarter == 3] <- "Q3 (Jul-Sep)"
    data$quarter_label[data$quarter == 4] <- "Q4 (Oct-Dec)"
    return(data)
  }
  
  airfare2022 <- create_quarter_labels(airfare2022)
  airfare2023 <- create_quarter_labels(airfare2023)
  airfare2024 <- create_quarter_labels(airfare2024)
  
  # 10. add year column back for filtering
  airfare2022$year <- 2022
  airfare2023$year <- 2023
  airfare2024$year <- 2024
  
  # 11. combine all years back together
  airfare_data <- rbind(airfare2022, airfare2023, airfare2024)
  
  return(airfare_data)
}

# ========================================================================
# LOAD DATA FOR BEST TIME TO BOOK (ONLY FROM MAIN CSV)
# ========================================================================

load_best_time_to_book_data <- function() {
  # Load ONLY the main CSV (has airport codes)
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  
  # data cleaning
  airfare_data$fare <- as.numeric(gsub("\\$", "", airfare_data$fare))
  airfare_data$quarter <- as.numeric(airfare_data$quarter)
  airfare_data$nsmiles <- as.numeric(gsub(",", "", airfare_data$nsmiles))
  airfare_data$passengers <- as.numeric(gsub(",", "", airfare_data$passengers))
  airfare_data$large_ms <- as.numeric(airfare_data$large_ms)
  
  # Remove missing values
  airfare_data <- airfare_data[!is.na(airfare_data$fare), ]
  airfare_data <- airfare_data[!is.na(airfare_data$quarter), ]
  airfare_data <- airfare_data[!is.na(airfare_data$nsmiles), ]
  airfare_data <- airfare_data[!is.na(airfare_data$airport_1), ]
  airfare_data <- airfare_data[!is.na(airfare_data$airport_2), ]
  
  # Filter for 2022-2024
  airfare_data <- airfare_data[airfare_data$Year >= 2022 & airfare_data$Year <= 2024, ]
  
  # Remove bad data
  airfare_data <- airfare_data[airfare_data$fare > 0, ]
  airfare_data <- airfare_data[airfare_data$nsmiles > 0, ]
  airfare_data <- airfare_data[airfare_data$fare < 2000, ]
  
  return(airfare_data)
}

# ========================================================================
# NAIVE BAYES CLASSIFIER: PRICE INCREASE PREDICTION
# ========================================================================

library(e1071)

# Enhanced prediction function with detailed breakdown
get_price_increase_probability <- function(filtered_data, all_data) {
  
  if (nrow(filtered_data) == 0) {
    return(list(prob = NULL, breakdown = NULL, history = NULL))
  }
  
  # Convert to numeric
  filtered_data$fare <- as.numeric(gsub("\\$", "", filtered_data$fare))
  filtered_data$large_ms <- as.numeric(filtered_data$large_ms)
  filtered_data$nsmiles <- as.numeric(gsub(",", "", filtered_data$nsmiles))
  
  all_data$fare <- as.numeric(gsub("\\$", "", all_data$fare))
  all_data$large_ms <- as.numeric(all_data$large_ms)
  all_data$nsmiles <- as.numeric(gsub(",", "", all_data$nsmiles))
  
  # Calculate average metrics
  avg_fare <- mean(filtered_data$fare, na.rm = TRUE)
  avg_market_share <- mean(filtered_data$large_ms, na.rm = TRUE)
  avg_distance <- mean(filtered_data$nsmiles, na.rm = TRUE)
  current_quarter <- as.numeric(filtered_data$quarter[1])
  
  # Initialize breakdown
  breakdown <- list()
  prob <- 50  # Start at 50%
  
  # ========== FACTOR 1: Market Share Impact ==========
  market_impact <- 0
  market_severity <- "None"
  
  if (!is.na(avg_market_share)) {
    if (avg_market_share > 0.8) {
      market_impact <- 20
      market_severity <- "Very High (>80%)"
    } else if (avg_market_share > 0.7) {
      market_impact <- 15
      market_severity <- "High (70-80%)"
    } else if (avg_market_share > 0.5) {
      market_impact <- 8
      market_severity <- "Moderate (50-70%)"
    } else {
      market_severity <- "Low (<50%)"
    }
  }
  prob <- prob + market_impact
  breakdown$market_share <- list(
    impact = market_impact,
    severity = market_severity,
    reason = "Less airline competition → higher prices"
  )
  
  # ========== FACTOR 2: Seasonal Impact ==========
  seasonal_impact <- 0
  season_name <- "Off-season"
  
  if (!is.na(current_quarter)) {
    if (current_quarter == 4) {
      seasonal_impact <- 20
      season_name <- "Q4 - Peak Holiday Travel"
    } else if (current_quarter == 1) {
      seasonal_impact <- 10
      season_name <- "Q1 - Post-Holiday"
    } else if (current_quarter == 3) {
      seasonal_impact <- 12
      season_name <- "Q3 - Summer Travel"
    } else {
      season_name <- "Q2 - Lower Demand"
    }
  }
  prob <- prob + seasonal_impact
  breakdown$seasonality <- list(
    impact = seasonal_impact,
    season = season_name,
    reason = "Seasonal demand affects pricing"
  )
  
  # ========== FACTOR 3: Distance Impact ==========
  distance_impact <- 0
  distance_category <- "Unknown"
  
  if (!is.na(avg_distance)) {
    if (avg_distance > 2000) {
      distance_impact <- 12
      distance_category <- "Very Long (>2000 mi)"
    } else if (avg_distance > 1500) {
      distance_impact <- 8
      distance_category <- "Long (1500-2000 mi)"
    } else if (avg_distance > 1000) {
      distance_impact <- 5
      distance_category <- "Medium (1000-1500 mi)"
    } else {
      distance_category <- "Short (<1000 mi)"
    }
  }
  prob <- prob + distance_impact
  breakdown$distance <- list(
    impact = distance_impact,
    category = distance_category,
    reason = "Longer routes have higher volatility"
  )
  
  # Cap probability at 100
  prob <- min(prob, 100)
  
  # ========== HISTORICAL COMPARISON ==========
  # Compare current filtered data to historical average
  historical_avg <- mean(all_data$fare, na.rm = TRUE)
  current_avg <- mean(filtered_data$fare, na.rm = TRUE)
  price_diff <- current_avg - historical_avg
  price_diff_pct <- (price_diff / historical_avg) * 100
  
  history <- list(
    current_avg = round(current_avg, 2),
    historical_avg = round(historical_avg, 2),
    difference = round(price_diff, 2),
    difference_pct = round(price_diff_pct, 1),
    status = if (price_diff > 0) "Above average" else "Below average"
  )
  
  return(list(
    prob = prob,
    breakdown = breakdown,
    history = history,
    metrics = list(
      avg_fare = round(avg_fare, 2),
      avg_market_share = round(avg_market_share, 4),
      avg_distance = round(avg_distance, 0)
    )
  ))
}

# ============================================================================
# SHINY SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Load data once at startup
  airfare_data <- reactive({
    load_and_clean()
  })
  
  # Load best time to book data (separate)
  best_time_book_data <- reactive({
    load_best_time_to_book_data()
  })
  
  # ========================================================================
  # SIMPLE FILTERING
  # ========================================================================
  
  filtered_data <- reactive({
    data <- airfare_data()
    
    # Filter by year
    if (!is.null(input$year_select)) {
      data <- data[data$year == input$year_select, ]
    }
    
    # Filter by origin airport
    if (!is.null(input$origin_airport)) {
      if (input$origin_airport != "All") {
        data <- data[data$airport_1 == input$origin_airport, ]
      }
    }
    
    # Filter by distance range
    if (!is.null(input$distance_range)) {
      min_dist <- input$distance_range[1]
      max_dist <- input$distance_range[2]
      data <- data[data$nsmiles >= min_dist, ]
      data <- data[data$nsmiles <= max_dist, ]
    }
    
    # Filter by airline
    if (!is.null(input$airline_filter)) {
      if (input$airline_filter != "All") {
        data <- data[data$carrier_lg == input$airline_filter, ]
      }
    }
    
    return(data)
  })
  
  # ========================================================================
  # SUMMARY STATISTICS
  # ========================================================================
  
  output$stat_mean_fare <- renderText({
    data <- filtered_data()
    mean_value <- mean(data$fare, na.rm = TRUE)
    paste("$", round(mean_value, 2))
  })
  
  output$stat_median_fare <- renderText({
    data <- filtered_data()
    median_value <- median(data$fare, na.rm = TRUE)
    paste("$", round(median_value, 2))
  })
  
  output$stat_min_fare <- renderText({
    data <- filtered_data()
    min_value <- min(data$fare, na.rm = TRUE)
    paste("$", round(min_value, 2))
  })
  
  output$stat_max_fare <- renderText({
    data <- filtered_data()
    max_value <- max(data$fare, na.rm = TRUE)
    paste("$", round(max_value, 2))
  })
  
  output$stat_records <- renderText({
    data <- filtered_data()
    format(nrow(data), big.mark = ",")
  })
  # ========================================================================
  # LINEAR REGRESSION MODEL
  # ========================================================================
  
  output$regression_summary <- renderPrint({
    data <- filtered_data()
    
    # Convert passengers and market share to numeric
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    # Build linear regression model
    model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    
    summary(model)
  })
  
  output$regression_plot <- renderPlotly({
    data <- filtered_data()
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    
    # Add predicted values
    data$predicted <- predict(model, data)
    
    # Sample for performance (plotly struggles with huge datasets)
    if (nrow(data) > 5000) {
      data <- data[sample(nrow(data), 5000), ]
    }
    
    p <- ggplot(data, aes(x = predicted, y = fare)) +
      geom_point(alpha = 0.2, color = "steelblue", size = 1) +
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
      theme_minimal() +
      labs(
        title = "Linear Regression: Actual vs Predicted Fare",
        x = "Predicted Fare ($)",
        y = "Actual Fare ($)"
      ) +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(p)
  })
  
  output$regression_metrics <- renderTable({
    data <- filtered_data()
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    
    predicted <- predict(model, data)
    actual <- data$fare
    
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mae  <- mean(abs(actual - predicted), na.rm = TRUE)
    r2   <- summary(model)$r.squared
    
    data.frame(
      Metric = c("R-squared", "RMSE", "MAE"),
      Value  = c(round(r2, 4), round(rmse, 2), round(mae, 2))
    )
  }, striped = TRUE, hover = TRUE)
  
  output$regression_coefficients <- renderDataTable({
    data <- filtered_data()
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    
    coef_df <- as.data.frame(summary(model)$coefficients)
    coef_df$Variable <- rownames(coef_df)
    coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    coef_df$Estimate    <- round(coef_df$Estimate, 4)
    coef_df$`Std. Error` <- round(coef_df$`Std. Error`, 4)
    coef_df$`t value`   <- round(coef_df$`t value`, 3)
    coef_df$`Pr(>|t|)`  <- round(coef_df$`Pr(>|t|)`, 4)
    
    coef_df
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  # ========================================================================
  # BOX PLOT 1: PRICE BY QUARTER (SINGLE YEAR)
  # ========================================================================
  
  output$box_plot_quarter <- renderPlotly({
    data <- filtered_data()
    
    # Create basic box plot
    p <- ggplot(data, aes(x = quarter_label, y = fare, fill = quarter_label)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Airline Ticket Price Distribution by Quarter",
        x = "Quarter",
        y = "Average Fare ($)"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  # ========================================================================
  # BOX PLOT 2: PRICE BY QUARTER - COMPARISON ACROSS YEARS
  # ========================================================================
  
  output$box_plot_years_comparison <- renderPlotly({
    data <- airfare_data()
    
    # Filter by origin if selected (but keep all years)
    if (!is.null(input$origin_airport)) {
      if (input$origin_airport != "All") {
        data <- data[data$airport_1 == input$origin_airport, ]
      }
    }
    
    # Create year factor for better ordering
    data$year <- as.factor(data$year)
    
    # Create comparison box plot
    p <- ggplot(data, aes(x = quarter_label, y = fare, fill = year)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Price Comparison: 2022 vs 2023 vs 2024",
        x = "Quarter",
        y = "Average Fare ($)",
        fill = "Year"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # ========================================================================
  # BOX PLOT 3: PRICE BY AIRLINE
  # ========================================================================
  
  output$box_plot_airline <- renderPlotly({
    data <- filtered_data()
    
    # Remove NA airlines
    data <- data[!is.na(data$carrier_lg), ]
    
    p <- ggplot(data, aes(x = carrier_lg, y = fare, fill = carrier_lg)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Price Distribution by Airline",
        x = "Airline",
        y = "Average Fare ($)"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_fill_brewer(palette = "Dark2")
    
    ggplotly(p)
  })
  
  # ========================================================================
  # BOX PLOT 4: PRICE BY QUARTER AND DISTANCE
  # ========================================================================
  
  output$box_plot_distance_quarter <- renderPlotly({
    data <- filtered_data()
    
    # Create distance categories
    data$distance_cat <- NA
    data$distance_cat[data$nsmiles <= 500] <- "Short (≤500)"
    data$distance_cat[data$nsmiles > 500 & data$nsmiles <= 1000] <- "Medium (501-1000)"
    data$distance_cat[data$nsmiles > 1000 & data$nsmiles <= 2000] <- "Long (1001-2000)"
    data$distance_cat[data$nsmiles > 2000] <- "Very Long (>2000)"
    
    # Create box plot
    p <- ggplot(data, aes(x = quarter_label, y = fare, fill = distance_cat)) +
      geom_boxplot(alpha = 0.7, position = "dodge") +
      theme_minimal() +
      labs(
        title = "Price by Quarter and Flight Distance",
        x = "Quarter",
        y = "Average Fare ($)",
        fill = "Distance"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # ========================================================================
  # QUARTERLY SUMMARY TABLE
  # ========================================================================
  
  output$quarterly_summary_table <- renderDataTable({
    data <- filtered_data()
    
    # Create summary for each quarter
    q1_data <- data[data$quarter == 1, ]
    q2_data <- data[data$quarter == 2, ]
    q3_data <- data[data$quarter == 3, ]
    q4_data <- data[data$quarter == 4, ]
    
    # Build summary table
    summary_table <- data.frame(
      Quarter = c("Q1", "Q2", "Q3", "Q4"),
      Mean = c(
        round(mean(q1_data$fare, na.rm = TRUE), 2),
        round(mean(q2_data$fare, na.rm = TRUE), 2),
        round(mean(q3_data$fare, na.rm = TRUE), 2),
        round(mean(q4_data$fare, na.rm = TRUE), 2)
      ),
      Median = c(
        round(median(q1_data$fare, na.rm = TRUE), 2),
        round(median(q2_data$fare, na.rm = TRUE), 2),
        round(median(q3_data$fare, na.rm = TRUE), 2),
        round(median(q4_data$fare, na.rm = TRUE), 2)
      ),
      Min = c(
        round(min(q1_data$fare, na.rm = TRUE), 2),
        round(min(q2_data$fare, na.rm = TRUE), 2),
        round(min(q3_data$fare, na.rm = TRUE), 2),
        round(min(q4_data$fare, na.rm = TRUE), 2)
      ),
      Max = c(
        round(max(q1_data$fare, na.rm = TRUE), 2),
        round(max(q2_data$fare, na.rm = TRUE), 2),
        round(max(q3_data$fare, na.rm = TRUE), 2),
        round(max(q4_data$fare, na.rm = TRUE), 2)
      ),
      Records = c(nrow(q1_data), nrow(q2_data), nrow(q3_data), nrow(q4_data))
    )
    
    return(summary_table)
  }, options = list(pageLength = 4, searching = FALSE, paging = FALSE))
  
  # ========================================================================
  # FEATURE CORRELATION
  # ========================================================================
  
  output$feature_correlation <- renderPlotly({
    data <- filtered_data()
    
    # Convert columns to numeric (remove any non-numeric characters first)
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    # Calculate simple correlations
    cor_distance <- cor(data$nsmiles, data$fare, use = "complete.obs")
    cor_passengers <- cor(data$passengers, data$fare, use = "complete.obs")
    cor_market_share <- cor(data$large_ms, data$fare, use = "complete.obs")
    
    # Create data frame for plotting
    corr_data <- data.frame(
      Feature = c("Distance", "Passengers", "Market Share"),
      Correlation = c(cor_distance, cor_passengers, cor_market_share)
    )
    
    # Create bar plot
    p <- ggplot(corr_data, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Feature)) +
      geom_col(alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Feature Correlation with Ticket Price",
        x = "Feature",
        y = "Correlation"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      ) +
      scale_fill_brewer(palette = "Dark2")
    
    ggplotly(p)
  })
  
  # ========================================================================
  # STATE HEATMAP: MEAN FARE BY ORIGIN STATE (NEW)
  # ========================================================================
  
  output$state_heatmap <- renderPlotly({
    data <- filtered_data()
    
    # Parse state abbreviation from city1 column (format: "Albany, NY")
    data$origin_state <- trimws(sub(".*,\\s*", "", data$city1))
    
    # Drop rows with missing state info
    data <- data[!is.na(data$origin_state) & nchar(data$origin_state) == 2, ]
    
    # Keep only valid US states
    valid_states <- c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
      "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
      "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
    )
    data <- data[data$origin_state %in% valid_states, ]
    
    # Compute mean fare by origin state
    state_fares <- aggregate(fare ~ origin_state, data = data, FUN = mean, na.rm = TRUE)
    colnames(state_fares) <- c("state", "mean_fare")
    state_fares$mean_fare <- round(state_fares$mean_fare, 2)
    
    # Build the choropleth map using plotly
    fig <- plot_geo(state_fares, locationmode = "USA-states") %>%
      add_trace(
        z         = ~mean_fare,
        locations = ~state,
        color     = ~mean_fare,
        colorscale = list(
          list(0,   "rgba(100, 149, 237, 0.3)"),
          list(0.5, "rgba(138, 43, 226, 0.5)"),
          list(1,   "rgba(75, 0, 130, 0.85)")
        ),
        text = ~paste0(state, "<br>Mean Fare: $", mean_fare),
        hoverinfo = "text",
        colorbar  = list(title = "Mean Fare ($)")
      ) %>%
      layout(
        title = list(
          text = paste0("Mean Airfare by Origin State (", input$year_select, ")"),
          x    = 0.5
        ),
        geo = list(
          scope        = "usa",
          projection   = list(type = "albers usa"),
          showlakes    = TRUE,
          lakecolor    = "rgb(255,255,255)"
        )
      )
    
    fig
  })
  
  # ========================================================================
  # STATE FARE SUMMARY TABLE (NEW)
  # ========================================================================
  
  output$state_fare_table <- renderDataTable({
    data <- filtered_data()
    
    # Parse state abbreviation from city1 column (format: "Albany, NY")
    data$origin_state <- trimws(sub(".*,\\s*", "", data$city1))
    
    # Drop rows with missing or invalid state info
    data <- data[!is.na(data$origin_state) & nchar(data$origin_state) == 2, ]
    
    valid_states <- c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
      "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
      "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
    )
    data <- data[data$origin_state %in% valid_states, ]
    
    state_summary <- aggregate(
      fare ~ origin_state,
      data = data,
      FUN  = function(x) c(
        mean   = round(mean(x, na.rm = TRUE), 2),
        median = round(median(x, na.rm = TRUE), 2),
        n      = length(x)
      )
    )
    colnames(state_summary)[1] <- "State"
    
    output_table <- data.frame(
      State       = state_summary$State,
      Mean_Fare   = round(state_summary$fare[, "mean"],   2),
      Median_Fare = round(state_summary$fare[, "median"], 2),
      Records     = as.integer(state_summary$fare[, "n"])
    )
    
    output_table <- output_table[order(output_table$Mean_Fare), ]
    
    return(output_table)
  }, options = list(pageLength = 10, searching = TRUE, paging = TRUE))
  
  # ========================================================================
  # PRICE INCREASE PREDICTION OUTPUT WITH BREAKDOWN
  # ========================================================================
  
  output$price_increase_prediction <- renderUI({
    data <- filtered_data()
    all_data <- airfare_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p style='color: red;'>No data available for this selection.</p>"))
    }
    
    # Get probability and breakdown
    result <- get_price_increase_probability(data, all_data)
    prob_increase <- result$prob
    breakdown <- result$breakdown
    history <- result$history
    
    if (is.null(prob_increase)) {
      return(HTML("<p style='color: orange;'>Unable to calculate prediction.</p>"))
    }
    
    # Color code based on probability
    if (prob_increase > 70) {
      color <- "#d32f2f"
      recommendation <- "⚠️ HIGH RISK — Book Now!"
      explanation <- "Prices are likely to increase in the next quarter."
    } else if (prob_increase > 50) {
      color <- "#f57c00"
      recommendation <- "⚡ MODERATE RISK — Book Soon"
      explanation <- "There's a moderate chance prices will increase."
    } else {
      color <- "#388e3c"
      recommendation <- "✅ LOW RISK — Price Stable"
      explanation <- "Prices are likely to remain stable or decrease."
    }
    
    # Build breakdown HTML
    breakdown_html <- paste(
      "<div style='background-color: rgba(255,255,255,0.1); padding: 16px; border-radius: 8px; margin-top: 16px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 12px; font-size: 13px;'>Risk Factors Breakdown:</h5>",
      
      # Market Share
      "<div style='margin-bottom: 10px; font-size: 12px;'>",
      "<strong>Market Concentration:</strong> ", breakdown$market_share$severity, " (+", breakdown$market_share$impact, "%)<br>",
      "<em style='color: rgba(255,255,255,0.8);'>", breakdown$market_share$reason, "</em>",
      "</div>",
      
      # Seasonality
      "<div style='margin-bottom: 10px; font-size: 12px;'>",
      "<strong>Seasonal Demand:</strong> ", breakdown$seasonality$season, " (+", breakdown$seasonality$impact, "%)<br>",
      "<em style='color: rgba(255,255,255,0.8);'>", breakdown$seasonality$reason, "</em>",
      "</div>",
      
      # Distance
      "<div style='font-size: 12px;'>",
      "<strong>Flight Distance:</strong> ", breakdown$distance$category, " (+", breakdown$distance$impact, "%)<br>",
      "<em style='color: rgba(255,255,255,0.8);'>", breakdown$distance$reason, "</em>",
      "</div>",
      
      "</div>",
      sep = ""
    )
    
    # Build historical comparison HTML
    history_html <- paste(
      "<div style='background-color: rgba(255,255,255,0.1); padding: 16px; border-radius: 8px; margin-top: 16px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 12px; font-size: 13px;'>Historical Price Comparison:</h5>",
      
      "<div style='display: flex; justify-content: space-around; text-align: center; font-size: 12px;'>",
      "<div>",
      "<strong style='font-size: 14px;'>$", history$current_avg, "</strong><br>",
      "Current Average",
      "</div>",
      "<div>",
      "<strong style='font-size: 14px;'>$", history$historical_avg, "</strong><br>",
      "Historical Average",
      "</div>",
      "<div>",
      "<strong style='font-size: 14px;'>", history$difference_pct, "%</strong><br>",
      history$status,
      "</div>",
      "</div>",
      
      "</div>",
      sep = ""
    )
    
    HTML(paste(
      "<div style='background-color:", color, "; color: white; padding: 24px; border-radius: 10px; text-align: center;'>",
      "<h3 style='margin: 0 0 12px 0;'>Price Increase Prediction</h3>",
      "<h1 style='margin: 0 0 8px 0; font-size: 48px;'>", round(prob_increase, 1), "%</h1>",
      "<p style='margin: 0 0 16px 0; font-size: 14px;'>Probability of price increase next quarter</p>",
      "<hr style='border: none; border-top: 1px solid rgba(255,255,255,0.3); margin: 16px 0;'>",
      "<h4 style='margin: 12px 0;'>", recommendation, "</h4>",
      "<p style='margin: 8px 0 0 0; font-size: 13px;'>", explanation, "</p>",
      
      breakdown_html,
      history_html,
      
      "</div>",
      sep = ""
    ))
  })
  
  # ========================================================================
  # BEST TIME TO BOOK - INTERACTIVE PREDICTION TOOL
  # ========================================================================
  
  output$best_time_to_book_ui <- renderUI({
    data <- best_time_book_data()  # USE THE NEW DATASET
    
    # Clean and get unique destinations (airport_2)
    data$airport_2 <- trimws(data$airport_2)
    destinations <- sort(unique(data$airport_2[!is.na(data$airport_2) & 
                                                 data$airport_2 != ""]))
    
    # Debug: print to console
    print(paste("Number of destinations found:", length(destinations)))
    print(paste("Sample destinations:", paste(head(destinations, 5), collapse = ", ")))
    
    div(
      div(class = "content-card",
          div(class = "content-card-title", "Find Your Best Flight Deal"),
          tags$p("Select your preferred destination and travel quarter to find the cheapest departure airport and optimal booking time.",
                 style = "font-size: 12px; color: #718096; margin-bottom: 16px;"),
          
          fluidRow(
            column(4,
                   selectInput("dest_airport", "Destination Airport",
                               choices = c("Select destination..." = "", destinations),
                               selected = "")
            ),
            column(4,
                   selectInput("travel_quarter", "Travel Quarter",
                               choices = c("Select quarter..." = "", 
                                           "Q1 (Jan-Mar)" = 1,
                                           "Q2 (Apr-Jun)" = 2,
                                           "Q3 (Jul-Sep)" = 3,
                                           "Q4 (Oct-Dec)" = 4),
                               selected = "")
            ),
            column(4,
                   actionButton("predict_btn", "Find Best Deal", 
                                style = "margin-top: 25px; width: 100%; background-color: #4a90d9; color: white; border: none; padding: 8px; cursor: pointer;")
            )
          ),
          
          hr(style = "margin: 20px 0;"),
          
          uiOutput("best_deal_results")
      )
    )
  })
  
  # Reactive expression for best deal calculation
  best_deal_data <- eventReactive(input$predict_btn, {
    
    if (input$dest_airport == "" || input$travel_quarter == "") {
      return(NULL)
    }
    
    route_data <- best_time_book_data()  # USE THE NEW DATASET
    
    # Filter for destination and quarter
    route_data <- route_data[route_data$airport_2 == input$dest_airport & 
                               route_data$quarter == as.numeric(input$travel_quarter), ]
    
    print(paste("Rows found for", input$dest_airport, "in Q", input$travel_quarter, ":", nrow(route_data)))
    
    if (nrow(route_data) == 0) {
      return(NULL)
    }
    
    # Aggregate by origin airport
    summary_clean <- data.frame()
    
    for (airport in unique(route_data$airport_1)) {
      airport_data <- route_data[route_data$airport_1 == airport, ]
      
      summary_clean <- rbind(summary_clean, data.frame(
        Origin = airport,
        Predicted_Fare = round(mean(airport_data$fare, na.rm = TRUE), 2),
        Actual_Fare = round(mean(airport_data$fare, na.rm = TRUE), 2),
        Avg_Distance = round(mean(airport_data$nsmiles, na.rm = TRUE), 0),
        Routes = nrow(airport_data),
        stringsAsFactors = FALSE
      ))
    }
    
    if (nrow(summary_clean) == 0) {
      return(NULL)
    }
    
    # Sort by predicted fare
    summary_clean <- summary_clean[order(summary_clean$Predicted_Fare), ]
    rownames(summary_clean) <- NULL
    
    return(summary_clean)
  })
  
  # Render best deal results
  output$best_deal_results <- renderUI({
    result <- best_deal_data()
    
    if (is.null(result)) {
      return(HTML("<p style='color: #f57c00;'>⚠️ No data available for this route and quarter. Try a different combination.</p>"))
    }
    
    best_airport <- result$Origin[1]
    best_fare <- result$Predicted_Fare[1]
    worst_airport <- result$Origin[nrow(result)]
    worst_fare <- result$Predicted_Fare[nrow(result)]
    savings <- worst_fare - best_fare
    savings_pct <- (savings / worst_fare) * 100
    
    # Determine booking window based on quarter
    quarter_num <- as.numeric(input$travel_quarter)
    if (quarter_num == 4) {
      booking_window <- "60-90 days in advance"
      booking_reason <- "Q4 is peak holiday season - book early!"
    } else if (quarter_num == 1) {
      booking_window <- "45-60 days in advance"
      booking_reason <- "Post-holiday period - moderate demand"
    } else if (quarter_num == 3) {
      booking_window <- "45-75 days in advance"
      booking_reason <- "Summer travel - book well ahead"
    } else {
      booking_window <- "30-45 days in advance"
      booking_reason <- "Spring season - moderate booking window"
    }
    
    # Build recommendation card
    recommendation_html <- paste(
      "<div style='background: linear-gradient(135deg, #388e3c 0%, #2e7d32 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 16px;'>",
      "<h4 style='margin: 0 0 12px 0;'>✅ Best Deal Found</h4>",
      "<div style='font-size: 24px; font-weight: bold; margin-bottom: 8px;'>$", best_fare, "</div>",
      "<p style='margin: 0 0 12px 0; font-size: 14px;'>",
      "Depart from <strong>", best_airport, "</strong> to <strong>", input$dest_airport, 
      "</strong> in <strong>Q", input$travel_quarter, "</strong>",
      "</p>",
      "<div style='background: rgba(255,255,255,0.2); padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
      "<strong>Recommended Booking Window:</strong> ", booking_window, "<br>",
      "<em>", booking_reason, "</em>",
      "</div>",
      "<p style='margin: 0; font-size: 13px;'>",
      "Save up to <strong>$", round(savings, 2), " (", round(savings_pct, 1), "%)</strong> ",
      "vs. flying from ", worst_airport,
      "</p>",
      "</div>",
      sep = ""
    )
    
    # Build comparison table
    table_html <- dataTableOutput("best_deal_table")
    
    list(
      HTML(recommendation_html),
      div(class = "content-card",
          div(class = "content-card-title", "All Departure Airports (Ranked by Price)"),
          tags$p("Sorted from cheapest to most expensive.",
                 style = "font-size: 12px; color: #718096; margin-bottom: 12px;"),
          table_html
      )
    )
  })
  
  # Render comparison table
  output$best_deal_table <- renderDataTable({
    result <- best_deal_data()
    
    if (is.null(result)) {
      return(data.frame())
    }
    
    return(result)
  }, options = list(pageLength = 10, searching = TRUE, paging = TRUE))
  
  # ========================================================================
  # MODEL COMPARISON TAB
  # ========================================================================
  
  output$model_comparison_ui <- renderUI({
    div(
      div(class = "content-card",
          div(class = "content-card-title", "Compare Model Predictions"),
          tags$p("See how the Naive Bayes and Linear Regression models predict prices differently.",
                 style = "font-size: 12px; color: #718096; margin-bottom: 16px;"),
          
          fluidRow(
            column(6,
                   h4("Linear Regression Model", style = "color: #4a90d9;"),
                   tableOutput("lr_model_summary")
            ),
            column(6,
                   h4("Naive Bayes Classifier", style = "color: #388e3c;"),
                   tableOutput("nb_model_summary")
            )
          ),
          
          hr(),
          
          h4("Key Differences:", style = "margin-top: 20px;"),
          dataTableOutput("model_comparison_table")
      )
    )
  })
  
  # Linear Regression Summary
  output$lr_model_summary <- renderTable({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Metric = "No data available"))
    }
    
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    
    predicted <- predict(model, data)
    actual <- data$fare
    
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    r2 <- summary(model)$r.squared
    
    data.frame(
      Metric = c("R-squared", "RMSE", "MAE", "Model Type", "Output"),
      Value = c(
        round(r2, 4),
        round(rmse, 2),
        round(mae, 2),
        "Continuous",
        "Predicted Fare ($)"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Naive Bayes Summary
  output$nb_model_summary <- renderTable({
    data <- filtered_data()
    all_data <- airfare_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Metric = "No data available"))
    }
    
    result <- get_price_increase_probability(data, all_data)
    
    data.frame(
      Metric = c("Probability", "Risk Level", "Model Type", "Output"),
      Value = c(
        paste0(round(result$prob, 1), "%"),
        if (result$prob > 70) "High" else if (result$prob > 50) "Moderate" else "Low",
        "Binary Classification",
        "Price Increase? (Yes/No)"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Comparison Table
  output$model_comparison_table <- renderDataTable({
    data.frame(
      Aspect = c(
        "Purpose",
        "Output Type",
        "Best For",
        "Accuracy Metric",
        "Prediction Example",
        "Factors Considered"
      ),
      Linear_Regression = c(
        "Predict exact fare prices",
        "Continuous numeric value",
        "Finding specific price estimates",
        "RMSE, MAE, R²",
        "$285.50 ± $45",
        "Distance, passengers, market share, quarter"
      ),
      Naive_Bayes = c(
        "Predict if prices will increase",
        "Binary classification (Yes/No)",
        "Booking decision guidance",
        "Probability percentage",
        "83% chance prices go up",
        "Market concentration, seasonality, distance"
      )
    )
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # ========================================================================
  # DECISION TREE MODEL
  # ========================================================================
  
  output$decision_tree_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) < 10) {
      return(NULL)
    }
    
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    # Build decision tree
    tree_model <- rpart(fare ~ nsmiles + passengers + large_ms + quarter,
                        data = data,
                        method = "anova",  # for continuous target
                        cp = 0.01)  # complexity parameter
    
    # Plot the tree
    rpart.plot(tree_model, 
               main = "Decision Tree: Airline Fare Prediction",
               type = 4, 
               extra = 101)
  })
  
  # Decision Tree Predictions & Metrics
  output$decision_tree_metrics <- renderTable({
    data <- filtered_data()
    
    if (nrow(data) < 10) {
      return(data.frame(Metric = "Insufficient data"))
    }
    
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    # Build tree
    tree_model <- rpart(fare ~ nsmiles + passengers + large_ms + quarter,
                        data = data,
                        method = "anova",
                        cp = 0.01)
    
    # Get predictions
    predicted <- predict(tree_model, data)
    actual <- data$fare
    
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    
    # Calculate R²
    ss_res <- sum((actual - predicted)^2)
    ss_tot <- sum((actual - mean(actual))^2)
    r2 <- 1 - (ss_res / ss_tot)
    
    data.frame(
      Metric = c("R-squared", "RMSE", "MAE", "Model Type"),
      Value = c(round(r2, 4), round(rmse, 2), round(mae, 2), "Non-parametric")
    )
  }, striped = TRUE, hover = TRUE)
  
  # ========================================================================
  # MODEL COMPARISON: LINEAR REGRESSION vs DECISION TREE
  # ========================================================================
  
  output$lr_vs_dt_comparison <- renderDataTable({
    data.frame(
      Aspect = c(
        "Model Type",
        "Prediction Output",
        "Handles Non-linearity",
        "Feature Interactions",
        "Interpretability",
        "Overfitting Risk",
        "Best When",
        "Weakness"
      ),
      Linear_Regression = c(
        "Parametric",
        "Continuous fare estimate",
        "❌ No (assumes linear)",
        "❌ Limited",
        "✅ Very High (see coefficients)",
        "Low",
        "Data shows clear linear trend",
        "May underfit complex patterns"
      ),
      Decision_Tree = c(
        "Non-parametric",
        "Continuous fare estimate",
        "✅ Yes (captures curves)",
        "✅ Yes (natural splits)",
        "✅ High (visualize decision rules)",
        "High (needs pruning)",
        "Data has complex, non-linear patterns",
        "May overfit if not pruned carefully"
      )
    )
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # Side-by-side Metrics Comparison
  output$model_performance_comparison <- renderTable({
    data <- filtered_data()
    
    if (nrow(data) < 10) {
      return(data.frame(Metric = "Insufficient data"))
    }
    
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    data$large_ms <- as.numeric(data$large_ms)
    
    # Linear Regression
    lr_model <- lm(fare ~ nsmiles + passengers + large_ms + quarter, data = data)
    lr_pred <- predict(lr_model, data)
    lr_rmse <- sqrt(mean((data$fare - lr_pred)^2, na.rm = TRUE))
    lr_mae <- mean(abs(data$fare - lr_pred), na.rm = TRUE)
    lr_r2 <- summary(lr_model)$r.squared
    
    # Decision Tree
    tree_model <- rpart(fare ~ nsmiles + passengers + large_ms + quarter,
                        data = data,
                        method = "anova",
                        cp = 0.01)
    dt_pred <- predict(tree_model, data)
    dt_rmse <- sqrt(mean((data$fare - dt_pred)^2, na.rm = TRUE))
    dt_mae <- mean(abs(data$fare - dt_pred), na.rm = TRUE)
    ss_res <- sum((data$fare - dt_pred)^2)
    ss_tot <- sum((data$fare - mean(data$fare))^2)
    dt_r2 <- 1 - (ss_res / ss_tot)
    
    data.frame(
      Metric = c("R-squared", "RMSE ($)", "MAE ($)", "Winner"),
      Linear_Regression = c(
        round(lr_r2, 4),
        round(lr_rmse, 2),
        round(lr_mae, 2),
        if (lr_rmse < dt_rmse) "✅ Better" else "❌"
      ),
      Decision_Tree = c(
        round(dt_r2, 4),
        round(dt_rmse, 2),
        round(dt_mae, 2),
        if (dt_rmse < lr_rmse) "✅ Better" else "❌"
      )
    )
  }, striped = TRUE, hover = TRUE)
}  

# return server object
server