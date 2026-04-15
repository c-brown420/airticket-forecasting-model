ui <- fluidPage(
  #use fluidRow() to control the plot layout
  fluidRow(
    column(width = 4,
           #selection control part
           #code start here:
           #Add three selection elements, one for the survival status, one for pclass and one for the gender of the passengers. Set their initial selections using the selected argument.
           #Add one slider for selecting age range.
           checkboxGroupInput("survival", "Did the passenger survive?",
                              choices = c("survived" = 1, "died" = 0),
                              selected = c(1, 0)),
           checkboxGroupInput("pclass", "Ticket Class:",
                              choices = c("1st" = "1", "2nd" = "2", "3rd" = "3"),
                              selected = c("1", "2", "3")),
           checkboxGroupInput("sex", "Gender:",
                              choices = c("male", "female"),
                              selected = c("male", "female")),
           sliderInput("age", "Age Range:",
                       min = 0, max = 80,
                       value = c(0, 80))
           
           #code end here
    ),
    column(width = 8,
           #display plot part
           #code start here:
           plotOutput("plot")
           #code end here:
    )
  )
)
# return the UI object
ui