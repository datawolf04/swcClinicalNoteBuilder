library(shiny)
library(bslib)

ui <- page_fluid(
  navset_tab(
    # Panel 1: Select items
    nav_panel(
      title = "Step 1: Choose Items",
      checkboxGroupInput(
        inputId = "parent_selection",
        label = "Select your main items:",
        choices = c("Project A", "Project B", "Project C"),
        selected = c("Project A", "Project B")
      )
    ),
    
    # Panel 2: Generate a checkboxGroupInput for each selected item
    nav_panel(
      title = "Step 2: Sub-options",
      uiOutput("sub_checkboxes_ui")
    ),
    
    # Panel 3: View combined results
    nav_panel(
      title = "Step 3: Summary",
      verbatimTextOutput("summary_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic UI: create a checkboxGroupInput for each item selected in Panel 1
  output$sub_checkboxes_ui <- renderUI({
    selected_items <- input$parent_selection
    
    if (is.null(selected_items) || length(selected_items) == 0) {
      return(p("Please go back to Step 1 and select at least one item."))
    }
    
    # Generate a list of checkboxGroupInput elements dynamically
    tagList(
      lapply(selected_items, function(item) {
        checkboxGroupInput(
          inputId = paste0("sub_", make.names(item)),
          label = paste("Sub-options for", item),
          choices = paste0(item, " - Option ", 1:3),
          selected = paste0(item, " - Option 1")
        )
      })
    )
  })
  
  # Server logic to collect/display all sub-selections
  output$summary_output <- renderPrint({
    selected_items <- input$parent_selection
    req(selected_items)
    
    # Collect inputs dynamically generated for each selected item
    sub_results <- lapply(selected_items, function(item) {
      input_id <- paste0("sub_", make.names(item))
      list(
        parent = item,
        sub_selected = input[[input_id]]
      )
    })
    
    print(sub_results)
  })
}

shinyApp(ui, server)
