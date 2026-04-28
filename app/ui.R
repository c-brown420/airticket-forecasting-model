library(shiny)
library(plotly)
library(DT)

# ============================================================================
# DATA HELPERS
# ============================================================================

get_airports <- function() {
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  us_airports <- c("ATL", "DFW", "DEN", "ORD", "LAX", "JFK", "SFO", "LAS", "SEA",
                   "MCO", "MIA", "EWR", "BOS", "MSP", "DTW", "PHL", "LGA", "IAD",
                   "CLT", "DCA", "PHX", "SAN", "DAL", "MDW", "IAH", "HOU", "AUS",
                   "BNA", "MCI", "STL", "TPA", "FLL", "PIT", "CLE", "RDU", "MEM",
                   "BWI", "MSY", "SLC", "PDX", "SJC", "OAK", "SMF", "SNA", "ONT",
                   "LGB", "BUR", "FAT", "LIH", "KOA", "OGG", "HNL", "ABE")
  airports <- sort(unique(airfare_data$airport_1))
  airports <- airports[airports %in% us_airports]
  return(c("All", sort(airports)))
}

get_airlines <- function() {
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  airfare_data <- airfare_data[airfare_data$Year >= 2022 & airfare_data$Year <= 2024, ]
  airlines <- sort(unique(airfare_data$carrier_lg))
  airlines <- airlines[!is.na(airlines)]
  return(c("All", airlines))
}

# ============================================================================
# CUSTOM CSS
# ============================================================================

custom_css <- "
  /* ── Google Fonts ── */
  @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&family=Playfair+Display:wght@700&display=swap');
 
  /* ── Reset & Base ── */
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
 
  body {
    font-family: 'Source Sans 3', sans-serif;
    background: #f4f6f9;
    color: #1a2332;
    font-size: 15px;
    line-height: 1.6;
  }
 
  /* ── Top Navigation Bar ── */
  .top-nav {
    background: #1a2e4a;
    padding: 0 40px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    height: 64px;
    box-shadow: 0 2px 12px rgba(0,0,0,0.3);
    position: sticky;
    top: 0;
    z-index: 1000;
  }
 
  .nav-brand {
    display: flex;
    align-items: center;
    gap: 14px;
  }
 
  .nav-logo-icon {
    width: 38px;
    height: 38px;
    background: linear-gradient(135deg, #4a90d9, #1a5fa8);
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 18px;
  }
 
  .nav-title {
    color: #ffffff;
    font-size: 18px;
    font-weight: 700;
    letter-spacing: 0.3px;
  }
 
  .nav-subtitle {
    color: #8ab4d4;
    font-size: 11px;
    font-weight: 300;
    letter-spacing: 1px;
    text-transform: uppercase;
  }
 
  .nav-links {
    display: flex;
    gap: 32px;
  }
 
  .nav-link {
    color: #c8dae8;
    font-size: 13px;
    font-weight: 600;
    letter-spacing: 0.5px;
    text-transform: uppercase;
    cursor: pointer;
    padding: 4px 0;
    border-bottom: 2px solid transparent;
    transition: color 0.2s, border-color 0.2s;
  }
 
  .nav-link:hover {
    color: #ffffff;
    border-color: #4a90d9;
  }
 
  /* ── Hero Banner ── */
  .hero-banner {
    background: linear-gradient(135deg, #1a2e4a 0%, #1a4a7a 50%, #1a3a5c 100%);
    padding: 40px 40px 36px;
    border-bottom: 3px solid #4a90d9;
    position: relative;
    overflow: hidden;
  }
 
  .hero-banner::before {
    content: '';
    position: absolute;
    top: -60px; right: -60px;
    width: 220px; height: 220px;
    background: rgba(74, 144, 217, 0.08);
    border-radius: 50%;
  }
 
  .hero-banner::after {
    content: '';
    position: absolute;
    bottom: -40px; left: 30%;
    width: 140px; height: 140px;
    background: rgba(74, 144, 217, 0.05);
    border-radius: 50%;
  }
 
  .hero-text h2 {
    font-family: 'Playfair Display', serif;
    color: #ffffff;
    font-size: 28px;
    font-weight: 700;
    line-height: 1.2;
    margin-bottom: 6px;
  }
 
  .hero-text p {
    color: #8ab4d4;
    font-size: 14px;
    font-weight: 300;
    letter-spacing: 0.3px;
  }
 
  .hero-badge {
    display: inline-block;
    background: rgba(74, 144, 217, 0.2);
    border: 1px solid rgba(74, 144, 217, 0.4);
    color: #8ab4d4;
    font-size: 11px;
    font-weight: 600;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    padding: 4px 12px;
    border-radius: 20px;
    margin-bottom: 10px;
  }
 
  /* ── Layout ── */
  .app-body {
    display: flex;
    gap: 0;
    min-height: calc(100vh - 64px - 120px);
  }
 
  /* ── Sidebar ── */
  .sidebar-panel {
    width: 280px;
    min-width: 280px;
    background: #ffffff;
    border-right: 1px solid #e2e8f0;
    padding: 28px 24px;
    box-shadow: 2px 0 8px rgba(0,0,0,0.04);
  }
 
  .sidebar-section-title {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 1.8px;
    text-transform: uppercase;
    color: #8ab4d4;
    margin-bottom: 16px;
    padding-bottom: 8px;
    border-bottom: 1px solid #e2e8f0;
  }
 
  /* Override Shiny select/slider styling */
  .sidebar-panel .form-group label,
  .sidebar-panel label {
    font-size: 12px;
    font-weight: 600;
    color: #4a5568;
    letter-spacing: 0.3px;
    text-transform: uppercase;
    margin-bottom: 6px;
  }
 
  .sidebar-panel .form-control,
  .sidebar-panel select {
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    font-size: 13px;
    color: #1a2332;
    background: #f8fafc;
    padding: 8px 12px;
    width: 100%;
    transition: border-color 0.2s;
  }
 
  .sidebar-panel .form-control:focus,
  .sidebar-panel select:focus {
    border-color: #4a90d9;
    outline: none;
    background: #fff;
    box-shadow: 0 0 0 3px rgba(74,144,217,0.12);
  }
 
  /* Stat Cards */
  .stat-card {
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-left: 3px solid #4a90d9;
    border-radius: 8px;
    padding: 14px 16px;
    margin-bottom: 10px;
    transition: box-shadow 0.2s;
  }
 
  .stat-card:hover {
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
  }
 
  .stat-label {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: #718096;
    margin-bottom: 4px;
  }
 
  .stat-value {
    font-size: 20px;
    font-weight: 700;
    color: #1a2e4a;
    line-height: 1;
  }
 
  /* ── Main Content ── */
  .main-panel {
    flex: 1;
    padding: 28px 32px;
    overflow-x: hidden;
  }
 
  /* ── Tab Navigation ── */
  .nav-tabs {
    border-bottom: 2px solid #e2e8f0;
    margin-bottom: 24px;
    display: flex;
    gap: 4px;
    flex-wrap: wrap;
  }
 
  .nav-tabs > li > a,
  .nav-tabs .nav-link {
    font-size: 12px !important;
    font-weight: 600 !important;
    letter-spacing: 0.5px !important;
    text-transform: uppercase !important;
    color: #718096 !important;
    border: none !important;
    border-bottom: 2px solid transparent !important;
    padding: 10px 16px !important;
    border-radius: 0 !important;
    background: transparent !important;
    margin-bottom: -2px !important;
    transition: color 0.2s, border-color 0.2s !important;
  }
 
  .nav-tabs > li.active > a,
  .nav-tabs > li > a:hover,
  .nav-tabs .nav-link.active,
  .nav-tabs .nav-link:hover {
    color: #1a2e4a !important;
    border-bottom-color: #4a90d9 !important;
    background: transparent !important;
  }
 
  /* ── Content Cards ── */
  .content-card {
    background: #ffffff;
    border: 1px solid #e2e8f0;
    border-radius: 10px;
    padding: 24px;
    margin-bottom: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.04);
  }
 
  .content-card-title {
    font-size: 13px;
    font-weight: 700;
    letter-spacing: 0.8px;
    text-transform: uppercase;
    color: #4a5568;
    margin-bottom: 16px;
    padding-bottom: 10px;
    border-bottom: 1px solid #f0f4f8;
    display: flex;
    align-items: center;
    gap: 8px;
  }
 
  .content-card-title::before {
    content: '';
    width: 3px;
    height: 16px;
    background: #4a90d9;
    border-radius: 2px;
    display: inline-block;
  }
 
  /* ── Regression Metrics ── */
  .metric-grid {
    display: flex;
    gap: 16px;
    margin-bottom: 20px;
    flex-wrap: wrap;
  }
 
  .metric-pill {
    flex: 1;
    min-width: 120px;
    background: linear-gradient(135deg, #1a2e4a, #1a4a7a);
    color: #ffffff;
    border-radius: 10px;
    padding: 18px 20px;
    text-align: center;
  }
 
  .metric-pill .m-label {
    font-size: 10px;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: #8ab4d4;
    margin-bottom: 6px;
  }
 
  .metric-pill .m-value {
    font-size: 22px;
    font-weight: 700;
  }
 
  /* ── DataTable overrides ── */
  .dataTables_wrapper .dataTables_length select,
  .dataTables_wrapper .dataTables_filter input {
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    padding: 4px 10px;
    font-size: 13px;
  }
 
  table.dataTable thead th {
    background: #f8fafc;
    color: #4a5568;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 1px;
    text-transform: uppercase;
    border-bottom: 2px solid #e2e8f0 !important;
    padding: 12px 14px;
  }
 
  table.dataTable tbody tr:hover {
    background: #f0f6ff !important;
  }
 
  /* ── Footer ── */
  .app-footer {
    background: #1a2e4a;
    color: #8ab4d4;
    text-align: center;
    padding: 16px 40px;
    font-size: 12px;
    letter-spacing: 0.3px;
    border-top: 3px solid #4a90d9;
  }
 
  /* ── Divider ── */
  .section-divider {
    border: none;
    border-top: 1px solid #e2e8f0;
    margin: 20px 0;
  }
 
  /* Shiny slider override */
  .irs--shiny .irs-bar,
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #4a90d9 !important;
    border-color: #4a90d9 !important;
  }
 
  .irs--shiny .irs-handle {
    border-color: #1a2e4a !important;
  }
"

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(custom_css)),
    tags$title("AirFare Analytics — Forecasting Dashboard")
  ),
  
  # ── Top Navigation ──
  div(class = "top-nav",
      div(class = "nav-brand",
          div(class = "nav-logo-icon", "✈"),
          div(
            div(class = "nav-title", "AirFare Analytics"),
            div(class = "nav-subtitle", "U.S. Domestic Market · 2022–2024")
          )
      ),
      div(class = "nav-links",
          span(class = "nav-link", "Dashboard")
      )
  ),
  
<<<<<<< HEAD
  # ── Hero Banner ──
  div(class = "hero-banner",
      div(class = "hero-text",
          tags$h2("Domestic Flight Price Intelligence"),
          tags$p("Interactive analysis of airline ticket prices across U.S. airport markets.")
      )
  ),
  
  # ── App Body ──
  div(class = "app-body",
      
      # ── SIDEBAR ──
      div(class = "sidebar-panel",
          
          div(class = "sidebar-section-title", "Filters"),
          
          selectInput("year_select", "Year",
                      choices  = c(2022, 2023, 2024),
                      selected = 2024),
          
          selectInput("origin_airport", "Origin Airport",
                      choices  = get_airports(),
                      selected = "All"),
          
          sliderInput("distance_range", "Flight Distance (miles)",
                      min = 0, max = 3000,
                      value = c(0, 3000), step = 100),
          
          selectInput("airline_filter", "Airline",
                      choices  = get_airlines(),
                      selected = "All"),
          
          hr(class = "section-divider"),
          div(class = "sidebar-section-title", "Summary Statistics"),
          
          div(class = "stat-card",
              div(class = "stat-label", "Mean Fare"),
              div(class = "stat-value", textOutput("stat_mean_fare"))
          ),
          div(class = "stat-card",
              div(class = "stat-label", "Median Fare"),
              div(class = "stat-value", textOutput("stat_median_fare"))
          ),
          div(class = "stat-card",
              div(class = "stat-label", "Total Records"),
              div(class = "stat-value", textOutput("stat_records"))
          )
      ),
      
      # ── MAIN PANEL ──
      div(class = "main-panel",
          
          tabsetPanel(id = "main_tabs",
                      
                      # ── TAB 1: Quarterly Trends ──
                      tabPanel("Quarterly Trends",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Price Distribution by Quarter"),
                                   plotlyOutput("box_plot_quarter", height = "420px")
                               ),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Quarterly Summary Table"),
                                   dataTableOutput("quarterly_summary_table")
                               )
                      ),
                      
                      # ── TAB 2: Year Comparison ──
                      tabPanel("Year Comparison",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Price Comparison: 2022 vs 2023 vs 2024"),
                                   plotlyOutput("box_plot_years_comparison", height = "440px")
                               )
                      ),
                      
                      # ── TAB 3: By Airline ──
                      tabPanel("By Airline",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Price Distribution by Carrier"),
                                   plotlyOutput("box_plot_airline", height = "440px")
                               )
                      ),
                      
                      # ── TAB 4: Distance Analysis ──
                      tabPanel("Distance Analysis",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Fare by Quarter & Flight Distance"),
                                   plotlyOutput("box_plot_distance_quarter", height = "440px")
                               )
                      ),
                      
                      # ── TAB 5: Top Origins ──
                      tabPanel("Top Origins",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Fare Distribution — Top 5 Origin Airports"),
                                   plotlyOutput("box_plot_origin_quarter", height = "480px")
                               )
                      ),
                      
                      # ── TAB 6: Price Forecast ──
                      tabPanel("Price Forecast",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Ticket Price vs Flight Distance"),
                                   plotlyOutput("scatter_distance_fare", height = "440px")
                               )
                      ),
                      
                      # ── TAB 7: Feature Analysis ──
                      tabPanel("Feature Analysis",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Feature Correlation with Ticket Price"),
                                   plotlyOutput("feature_correlation", height = "340px")
                               ),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Top Routes by Passenger Volume"),
                                   dataTableOutput("top_routes")
                               )
                      ),
                      
                      # ── TAB 8: Linear Regression ──
                      tabPanel("Linear Regression",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Model Performance Metrics"),
                                   tableOutput("regression_metrics")
                               ),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Actual vs Predicted Fare"),
                                   plotlyOutput("regression_plot", height = "400px")
                               ),
                               fluidRow(
                                 column(6,
                                        div(class = "content-card",
                                            div(class = "content-card-title", "Model Coefficients"),
                                            dataTableOutput("regression_coefficients")
                                        )
                                 ),
                                 column(6,
                                        div(class = "content-card",
                                            div(class = "content-card-title", "Full Model Summary"),
                                            verbatimTextOutput("regression_summary")
                                        )
                                 )
                               )
                      ),
                      
                      # ── TAB 9: State Heatmap ──
                      tabPanel("State Heatmap",
                               br(),
                               div(class = "content-card",
                                   div(class = "content-card-title", "Mean Airfare by Origin State"),
                                   tags$p("States shaded by mean fare from airports in that state. Green = cheaper, Red = more expensive.",
                                          style = "font-size: 12px; color: #718096; margin-bottom: 12px;"),
                                   plotlyOutput("state_heatmap", height = "480px")
                               ),
                               div(class = "content-card",
                                   div(class = "content-card-title", "State Fare Summary"),
                                   dataTableOutput("state_fare_table")
                               )
                      )
                      
          ) # end tabsetPanel
      )   # end main-panel
  ),    # end app-body
  
  # ── Footer ──
  div(class = "app-footer",
      "AirFare Analytics Dashboard · Data: U.S. Department of Transportation · Consumer Airfare Report Table 1a"
=======
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
    
    # MAIN CONTENT
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
             ),
             
             # TAB 7: State Heatmap (NEW)
             tabPanel("State Heatmap",
                      br(),
                      h4("Mean Fare by Origin State", style = "font-weight: bold; text-align: center;"),
                      p("States are shaded by mean airfare from airports in that state. Green = cheaper, Red = more expensive.",
                        style = "text-align: center; color: gray; font-size: 13px;"),
                      plotlyOutput("state_heatmap", height = "550px"),
                      br(),
                      dataTableOutput("state_fare_table")
             ),
             
             # NEW TAB: Wyoming Analysis
             tabPanel("Why is WYOMING so Expensive?",
                      br(),
                      h3("Wyoming Fare Analysis", style = "font-weight: bold;"),
                      tableOutput("wyoming_analysis"),
                      uiOutput("wyoming_explanation"),
                      br(),
                      h3("All States Comparison", style = "font-weight: bold;"),
                      p("Click column headers to sort. Notice: States with longer average distances have higher fares.",
                        style = "color: gray; font-size: 13px;"),
                      DT::DTOutput("state_comparison")
             )
           )
    )
>>>>>>> 3865f74e0e879ab948e8319e5c670b34caa93903
  )
  
)

# Return UI object
ui