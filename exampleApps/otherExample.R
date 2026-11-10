library(shiny)

ui <- fluidPage(
  titlePanel("Checkbox Group with 'Other' Option"),
  
  sidebarLayout(
    sidebarPanel(
      # Standard Checkbox Group
      checkboxGroupInput(
        inputId = "hobbies",
        label = "Select your hobbies:",
        choices = c("Reading", "Gaming", "Cooking", "Other")
      ),
      
      # Conditional text entry field: only visible when "Other" is selected
      conditionalPanel(
        condition = "input.hobbies.includes('Other')",
        textInput(
          inputId = "hobbies_other",
          label = "Please specify 'Other':",
          value = ""
        )
      ),
      
      actionButton("submit", "Submit Selection")
    ),
    
    mainPanel(
      h4("Final Selected Choices:"),
      verbatimTextOutput("final_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to combine standard choices and the "Other" fill-in text
  combined_choices <- eventReactive(input$submit, {
    req(input$hobbies)
    
    current_selection <- input$hobbies
    
    # Check if "Other" is checked and the text input isn't blank
    if ("Other" %in% current_selection) {
      # Remove the generic label "Other"
      current_selection <- current_selection[current_selection != "Other"]
      
      # Append the custom text if the user wrote something
      if (nzchar(trimws(input$hobbies_other))) {
        current_selection <- c(current_selection, trimws(input$hobbies_other))
      } else {
        current_selection <- c(current_selection, "Other (unspecified)")
      }
    }
    
    return(current_selection)
  })
  
  # Display the combined list on screen
  output$final_output <- renderPrint({
    combined_choices()
  })
}

shinyApp(ui = ui, server = server)
