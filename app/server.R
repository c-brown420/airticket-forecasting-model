library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
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

# ============================================================================
# SHINY SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Load data once at startup
  airfare_data <- reactive({
    load_and_clean()
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
  # BOX PLOT 5: PRICE BY QUARTER - FACETED BY TOP ORIGINS
  # ========================================================================
  
  output$box_plot_origin_quarter <- renderPlotly({
    data <- filtered_data()
    
    # Get top 5 origin airports
    origin_counts <- table(data$airport_1)
    top_origins <- names(sort(origin_counts, decreasing = TRUE)[1:5])
    
    # Filter for top origins only
    data <- data[data$airport_1 %in% top_origins, ]
    
    # Create faceted box plot
    p <- ggplot(data, aes(x = quarter_label, y = fare, fill = quarter_label)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~airport_1, nrow = 2) +
      theme_minimal() +
      labs(
        title = "Price Distribution by Quarter - Top 5 Origins",
        x = "Quarter",
        y = "Average Fare ($)"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
      ) +
      scale_fill_brewer(palette = "Set2")
    
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
  # TOP ROUTES TABLE
  # ========================================================================
  
  output$top_routes <- renderDataTable({
    data <- filtered_data()
    
    # Convert passengers to numeric if it's a character
    data$passengers <- as.numeric(gsub(",", "", data$passengers))
    
    # Create unique route combinations
    routes <- data.frame(
      origin = data$airport_1,
      destination = data$airport_2,
      passengers = data$passengers,
      fare = data$fare
    )
    
    # Remove rows with NA passengers
    routes <- routes[!is.na(routes$passengers), ]
    
    # Aggregate by route
    route_summary <- aggregate(
      cbind(passengers, fare) ~ origin + destination,
      data = routes,
      FUN = function(x) c(sum = sum(x), mean = mean(x))
    )
    
    # Sort by passenger volume
    route_summary <- route_summary[order(route_summary$passengers[, 1], decreasing = TRUE), ]
    route_summary <- route_summary[1:10, ]
    
    # Format output
    output_table <- data.frame(
      Origin = route_summary$origin,
      Destination = route_summary$destination,
      Total_Passengers = round(route_summary$passengers[, 1], 0),
      Avg_Fare = round(route_summary$fare[, 2], 2)
    )
    
    return(output_table)
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # ========================================================================
  # SCATTER PLOT: DISTANCE VS FARE
  # ========================================================================
  
  output$scatter_distance_fare <- renderPlotly({
    data <- filtered_data()
    data$year <- as.factor(data$year)
    
    p <- ggplot(data, aes(x = nsmiles, y = fare, color = year,
                          text = paste("Route:", airport_1, "->", airport_2,
                                       "<br>Distance:", nsmiles, "miles",
                                       "<br>Fare: $", round(fare, 2),
                                       "<br>Airline:", carrier_lg))) +
      geom_point(alpha = 0.3, size = 1) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
      theme_minimal() +
      labs(title = "Ticket Price vs Flight Distance by Year",
           x = "Distance (miles)", y = "Fare ($)", color = "Year") +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p, tooltip = "text")
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
  
}  

# return server object
server