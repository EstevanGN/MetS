# Local files ------------------------------------------------------------------
source("ui.R", local = TRUE)
source("server.R")



# Execute app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)