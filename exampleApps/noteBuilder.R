library(shiny)
library(bslib)
library(glue)
library(dplyr)

otherPresFeatures <- read.csv('presentationTypes.csv')

ui <- page_navbar(
  title = "Clinical Note Builder",
  id = 'clinNote',

  nav_panel(
    title = "Presentation",
    value = 'presentation1',

    layout_columns(
      col_widths = c(3,3,3,3),
      card(
        checkboxGroupInput(
          inputId = "people",
          label = "Session participants",
          choices = c(
            "Client" = "client", "Parent" = "parent", 
            "Caregiver" = "caregiver", "Sibling" = "sibling", "Other"),
          selected = NULL 
        ),

        conditionalPanel(
          condition = "input.people.includes('Other')",
          textInput(
            inputId = "people_other",
            label = "Please specify 'Other':",
            value = "person"
          )
        ),

        radioButtons(
          inputId = "location",
          label = "Session Location",        
          choices = c("Office" = "in her office", 
                      "Online" = "via telehealth")
        )
      ),

      card(
        checkboxGroupInput(
          inputId = "mood",
          label = "Client mood",        
          choices = c("enthymic", "dysphoric", "grandiose", "labile", "depressed",
            "anxious","elated","irritable","angry","not assessed", "other")
        ),

        conditionalPanel(
          condition = "input.mood.includes('other')",
          textInput(
            inputId = "mood_other",
            label = "Please specify 'Other':",          
            value = ""
          )
        )
      ),

      card(
        checkboxGroupInput(
          inputId = "affect",
          label = "Client affect",        
          choices = c("congruent","incongruent","inappropriate","flat","blunted",
            "restricted","not assessed","irritable","angry","not assessed", "other")
        ),

        conditionalPanel(
          condition = "input.affect.includes('other')",
          textInput(
            inputId = "affect_other",
            label = "Please specify 'Other':",
            value = ""
          )
        )
      ),

      card(
        checkboxGroupInput(
          inputId = 'addlPres',
          label = 'Additional Presentation',
          choices = c("Appearance","Behavior","Speech","Thought Process","Thought Content",
          "Attention/Concentration","Insight","Judgment")
        )
      )
    ),
    actionButton("goToAddlPres", "Save and Next")
  ),

  nav_panel(
    title = "Additional Presentation",
    value = 'presentation2',
    uiOutput("sub_checkboxes_ui")
  ),
    
  nav_panel(
    title = "Final Note Text",
    id = 'finalNote',
    textOutput("noteText")
  )
)


server <- function(input, output, session) {
  observeEvent(input$goToAddlPres,{
    nav_select(id = 'clinNote', selected = 'presentation2')
  })

  listCollapse <- function(lst){
    if(length(lst) == 2){
      lst <- paste(lst, collapse=" and ")
    } else {
      lst <- glue_collapse(lst, sep = ", ", last = ", and ")
    }
    return(lst)
  }

  # Reactive expression to combine standard choices and the "Other" fill-in text
  combined_participants <- eventReactive(input$goToAddlPres, {
    req(input$people)
    
    currPpl <- input$people
    
    # Check if "Other" is checked and the text input isn't blank
    if ("Other" %in% currPpl) {
      otherPpl <- trimws(input$people_other)
      if (nzchar(otherPpl)){
        currPpl <- replace(currPpl, currPpl == "Other", otherPpl)
      } else {
        currPpl <- currPpl[currPpl != "Other"]
      }
    }
    
    return(currPpl)
  })

  combined_mood <- eventReactive(input$goToAddlPres, {
    req(input$mood)
    
    currMood <- input$mood
    
    # Check if "Other" is checked and the text input isn't blank
    if ("other" %in% currMood) {
      otherMood <- trimws(input$mood_other)
      if (nzchar(otherPpl)){
        currMood <- replace(currMood, currMood == "other", otherMood)
      } else {
        currMood <- currMood[currMood != "other"]
      }
    }
    return(currMood)
  })

  combined_affect <- eventReactive(input$goToAddlPres, {
    req(input$affect)
    
    currAffect <- input$affect
    
    # Check if "Other" is checked and the text input isn't blank
    if ("other" %in% currAffect) {
      otherAffect <- trimws(input$affect_other)
      if (nzchar(otherPpl)){
        currAffect <- replace(currAffect, currAffect == "other", otherAffect)
      } else {
        currAffect <- currAffect[currAffect != "other"]
      }
    }
    return(currAffect)
  })
  
  s1 <- function(people, loc){
    stem <- "This writer met with "
    s1 <- paste0(stem, listCollapse(people), " ", loc, ".")
    return(s1)
  }

  s2 <- function(mood, affect){
    stem <- "Client presented with"
    post <- 'as evidenced by verbalizations and nonverbals.'
    mood <- listCollapse(mood)
    affect <- listCollapse(affect)
    s2 <- paste(stem, mood, 'mood and', affect, 'affect', post)
    return(s2)
  }

  s3 <- "This writer engaged in Socratic questioning and supportive reflections to facilitate exploration of ct functioning and exploration of clinical material."

  output$noteText <- renderText({
    # input$selected_fruits is accessed as a standard R vector
    paste(
      s1(combined_participants(), input$location), 
      s2(combined_mood(),combined_affect()),
      s3
    )
  })
}

shinyApp(ui = ui, server = server)
