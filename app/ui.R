library(shiny)
library(shinythemes)
library(plotly)
library(DT)   

# Load data once to get unique airports
get_airports <- function() {
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  airports <- sort(unique(airfare_data$airport_1))
  return(c("All", airports))
}

get_airlines <- function() {
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  airlines <- sort(unique(airfare_data$carrier_lg))
  return(c("All", airlines[!is.na(airlines)]))
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title
  h1("✈️ Airticket Forecasting Model", style = "text-align: center; margin: 20px 0;"),
  p("Analyzing domestic airline ticket prices (2022-2024)", style = "text-align: center; color: gray;"),
  
  hr(),
  
  fluidRow(
    # SIDEBAR - FILTERS
    column(width = 3,
           h4("Filters", style = "font-weight: bold;"),
           
           # Year selector
           selectInput("year_select", "Select Year:", 
                       choices = c(2022, 2023, 2024), 
                       selected = 2024),
           
           # Origin airport - NOW DYNAMIC
           selectInput("origin_airport", "Origin Airport:",
                       choices = get_airports(),
                       selected = "All"),
           
           # Distance range
           sliderInput("distance_range", "Flight Distance (miles):",
                       min = 0, max = 3000,
                       value = c(0, 3000),
                       step = 100),
           
           selectInput("airline_filter", "Airline:",
                       choices = get_airlines(),
                       selected = "All"),
           
           hr(),
           
           # Summary stats
           h4("Summary Statistics", style = "font-weight: bold;"),
           
           div(style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin: 5px 0;",
               p("Mean Fare:", br(), textOutput("stat_mean_fare"), style = "font-size: 14px; font-weight: bold;")
           ),
           
           div(style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin: 5px 0;",
               p("Median Fare:", br(), textOutput("stat_median_fare"), style = "font-size: 14px; font-weight: bold;")
           ),
           
           div(style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin: 5px 0;",
               p("Records:", br(), textOutput("stat_records"), style = "font-size: 14px; font-weight: bold;")
           )
    ),
    
    # MAIN CONTENT - BOX PLOTS
    column(width = 9,
           tabsetPanel(
             # TAB 1: Primary box plot
             tabPanel("Quarterly Trends",
                      br(),
                      plotlyOutput("box_plot_quarter", height = "500px"),
                      br(),
                      dataTableOutput("quarterly_summary_table")
             ),
             
             # TAB 2: Years comparison
             tabPanel("Year Comparison",
                      br(),
                      plotlyOutput("box_plot_years_comparison", height = "500px")
             ),
             
             # TAB 3: By airline
             tabPanel("By Airline",
                      br(),
                      plotlyOutput("box_plot_airline", height = "500px")
             ),
             
             # TAB 4: By distance
             tabPanel("Distance Analysis",
                      br(),
                      plotlyOutput("box_plot_distance_quarter", height = "500px")
             ),
             
             # TAB 5: By origin
             tabPanel("Top Origins",
                      br(),
                      plotlyOutput("box_plot_origin_quarter", height = "600px")
             ),
             
             tabPanel("Price Forecast",
                      br(),
                      plotlyOutput("scatter_distance_fare", height = "500px")
             ),
             
             # TAB 6: Feature importance
             tabPanel("Feature Analysis",
                      br(),
                      plotlyOutput("feature_correlation", height = "400px"),
                      br(),
                      h4("Top Routes by Passenger Volume"),
                      dataTableOutput("top_routes")
             )
           )
    )
  )
)

# Return UI object
ui