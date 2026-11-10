library(shiny)
library(bslib)

ui <- page_navbar(
  title = "My App",
  id = "nav_bar", # Required to track and change tabs programmatically
  
  nav_panel(
    title = "Home",
    value = "home_tab",
    h3("First Tab"),
    actionButton("go_to_data", "Submit & Go to Data")
  ),
  
  nav_panel(
    title = "Data",
    value = "data_tab",
    h3("Second Tab - You made it here!")
  )
)

server <- function(input, output, session) {
  observeEvent(input$go_to_data, {
    # Optional: perform calculations or save data here before switching
    
    # Switch to the 'data_tab' panel
    nav_select(id = "nav_bar", selected = "data_tab")
  })
}

shinyApp(ui, server)
