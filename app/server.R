library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# load and clean the data upon server launch
load_and_clean <- function() {
  # load data
  airfare_data <- read.csv("data/Consumer_Airfare_Report.csv", stringsAsFactors = FALSE)
  
  # data cleaning
  # 1. remove all missing vals in important columns
  airfare_data <- airfare_data[!is.na(airfare_data$average_fare), ]
  airfare_data <- airfare_data[!is.na(airfare_data$quarter), ]
  airfare_data <- airfare_data[!is.na(nonstop_distance), ]
  
  # 2. filter for 2022 - 2024 only
  airfare2022 <- airfare_data[airfare_data$year == 2022, ]
  airfare2023 <- airfare_data[airfare_data$year == 2023, ]
  airfare2024 <- airfare_data[airfare_data$year == 2024, ]
  
  # 3. remove bad data
  airfare2022 <- airfare2022[airfare2022$average_fare > 0, ]
  airfare2022 <- airfare2022[airfare2022$nonstop_distance > 0, ]
  
  airfare2023 <- airfare2023[airfare2023$average_fare > 0, ]
  airfare2023 <- airfare2023[airfare2023$nonstop_distance > 0, ]
  
  airfare2024 <- airfare2024[airfare2024$average_fare > 0, ]
  airfare2024 <- airfare2024[airfare2024$nonstop_distance > 0, ]
  
  # 4. remove extreme outliers
  airfare2022 <- airfare2022[airfare2022$average_fare < 2000, ]
  airfare2023 <- airfare2023[airfare2023$average_fare < 2000, ]
  airfare2024 <- airfare2024[airfare2024$average_fare < 2000, ]
  
  # 5. convert quarter to factor for proper ordering
  
  
}

titanic <- titanic_train
titanic <- na.omit(titanic)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)

server <- function(input, output) {
  # Create a new data frame with all possible combinations of Pclass and Sex
  all_combinations <- expand.grid(Pclass = c("1", "2", "3"), Sex = c("female", "male"))
  
  # Join the new data frame with the original titanic dataset
  titanic_complete <- merge(all_combinations, titanic, by = c("Pclass", "Sex"), all.x = TRUE)
  
  # Convert Pclass and Survived to factors
  titanic_complete$Pclass <- as.factor(titanic_complete$Pclass)
  titanic_complete$Survived <- as.factor(titanic_complete$Survived)
  
  
  
  titanic_filtered <- reactive({
    #code start here:
    #use subset() and take titanic_complete as data
    #inside subset()， write conditions for pclass, sex, age, and survived variables to match user selected values.
    subset(titanic_complete, 
           (Pclass == input$pclass[1] | Pclass == input$pclass[2] | Pclass == input$pclass[3]) & 
             (Sex == input$sex[1] | Sex == input$sex[2]) & 
             Age >= input$age[1] & 
             Age <= input$age[2])
    
    #code end here
  })
  
  
  output$plot <- renderPlot({
    
    #code start here:
    #creates a ggplot object using the filtered data as input: titanic_filtered(), setting the x-axis to the Age column and the fill to the Survived column.
    #use facet_wrap(), geom_density()
    ggplot(titanic_filtered(), aes(x = Age, fill = Survived)) + geom_density(alpha = 0.5) + facet_wrap(Sex ~ Pclass) + theme_bw() + labs(x = "Age", y = "Density", title = "Survival by Age, Gender, and Ticket Class", fill = "Survived")
    
    #code end here
    
    
  }, res = 96)
}

# return the server object
server