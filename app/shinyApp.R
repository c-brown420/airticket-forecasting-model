# source the UI and server files
source("ui.R")
source("server.R")

# create the Shiny app
shinyApp(ui = ui, server = server)