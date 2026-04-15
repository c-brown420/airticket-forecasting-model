library(shiny)
library(ggplot2)
library(dplyr)
library(titanic)

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