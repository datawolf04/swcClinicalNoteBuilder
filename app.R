library(shiny)
library(shinyjs)
library(bslib)
library(glue)
library(dplyr)
library(stringr)

otherPresFeatures <- read.csv('presentationTypes.csv')
otherPresFeatures$option <- tolower(otherPresFeatures$option)

list2sentence <- function(ll) {
  if (length(ll) == 1) {
    out <- ll
  } else if (length(ll) == 2) {
    out <- paste(ll, collapse = ' and ')
  } else {
    out <- glue_collapse(ll, sep = ', ', last = ', and ')
  }
  return(out)
}

apInputId <- function(item) {
  paste0("ap", gsub("[^A-Za-z0-9]", "", item))
}

ui <- page_fluid(
  useShinyjs(),
  title = "Clinical Note Builder",
  id = 'clinNote',

  div(
    id = 'wrapper',
    navset_tab(
      id = 'navTabs',
      # Panel 1: Initial Presentation
      nav_panel(
        title = "Presentation",
        value = 'presentation1',

        layout_columns(
          col_widths = c(3, 3, 3, 3),
          card(
            checkboxGroupInput(
              inputId = "people",
              label = "Session participants",
              choices = c(
                "Client" = "client",
                "Parent" = "parent",
                "Caregiver" = "caregiver",
                "Sibling" = "sibling",
                "Other" = "other"
              ),
              selected = NULL
            ),

            conditionalPanel(
              condition = "input.people.includes('other')",
              textInput(
                inputId = "people_other",
                label = "Please specify 'Other':",
                value = ""
              )
            ),

            radioButtons(
              inputId = "location",
              label = "Session Location",
              choices = c(
                "Office" = "in her office",
                "Online" = "via telehealth"
              )
            )
          ),

          card(
            checkboxGroupInput(
              inputId = "mood",
              label = "Client mood",
              choices = c(
                "enthymic",
                "dysphoric",
                "grandiose",
                "labile",
                "depressed",
                "anxious",
                "elated",
                "irritable",
                "angry",
                "not assessed",
                "other"
              )
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
              choices = c(
                "congruent",
                "incongruent",
                "inappropriate",
                "flat",
                "blunted",
                "restricted",
                "not assessed",
                "irritable",
                "angry",
                "not assessed",
                "other"
              )
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
              choices = c(
                "Appearance",
                "Behavior",
                "Speech",
                "Thought Process",
                "Thought Content",
                "Attention/Concentration",
                "Insight",
                "Judgment"
              )
            )
          )
        )
      ),

      # Panel 2: Generate a checkboxGroupInput for each selected item
      nav_panel(
        title = "Additonal Presentation",
        value = "presentation2",
        layout_columns(
          col_widths = c(6, 6),
          card(uiOutput("initialPresentation")),
          card(uiOutput("addlPresentation"))
        )
      ),

      # Panel 3: Note Text
      nav_panel(
        title = "Narrative Text",
        id = "note",
        h3("Narrative text"),
        p(
          "Edit this as you see fit. Copy/paste into EHR system when complete."
        ),
        uiOutput("noteNarrative"),
        p("Reset form when ready to move to next note"),
        actionButton("reset_all", "Reset note")
      )
    )
  )
)

server <- function(input, output, session) {
  # Ids of dynamically rendered inputs whose stored values are left over from
  # a previous note; they get cleared at the next render of that input
  staleInputs <- reactiveValues(ids = character(0))
  seenParticipants <- reactiveValues(choices = character(0))

  observeEvent(input$reset_all, {
    # reset() restores dynamically rendered inputs to their creation values,
    # and removed inputs keep their last server-side value, so mark those ids
    # stale instead of restoring them into the next note
    ids <- names(reactiveValuesToList(input))
    staleInputs$ids <- c(
      ids[grepl("^ap", ids)],
      ids[grepl("^engagedParticipants", ids)]
    )
    # Resets all inputs
    reset("wrapper")
    seenParticipants$choices <- character(0)
    # Switch back to the first tab
    updateTabsetPanel(session, "navTabs", selected = "presentation1")
  })

  # Dynamic UI: create a checkboxGroupInput for each item selected in Panel 1
  output$initialPresentation <- renderUI({
    sessionParticipants <- input$people
    if (is.null(sessionParticipants) || length(sessionParticipants) == 0) {
      return(p(
        "Please go back to Presentation and select at least one session participant."
      ))
    }

    # Preserve which participants are checked across re-renders (triggered by
    # changes to input$people) while auto-checking newly added participants;
    # otherwise a re-render would re-check everything, including 'other'
    staleIds <- isolate(staleInputs$ids)
    staleEngaged <- "engagedParticipants" %in% staleIds
    staleOther <- "engagedParticipants_other" %in% staleIds
    isolate(
      staleInputs$ids <- setdiff(
        staleIds,
        c("engagedParticipants", "engagedParticipants_other")
      )
    )

    prevSelected <- if (staleEngaged) {
      NULL
    } else {
      isolate(input$engagedParticipants)
    }
    prevChoices <- isolate(seenParticipants$choices)
    selected <- if (is.null(prevSelected)) {
      sessionParticipants
    } else {
      intersect(
        union(prevSelected, setdiff(sessionParticipants, prevChoices)),
        sessionParticipants
      )
    }
    seenParticipants$choices <- union(prevChoices, sessionParticipants)

    # Seed the 'Other' text box with what was typed on the Presentation tab
    otherTxt <- if (staleOther) {
      NULL
    } else {
      isolate(input$engagedParticipants_other)
    }
    if (is.null(otherTxt) || !nzchar(trimws(otherTxt))) {
      otherTxt <- isolate(input$people_other)
    }
    if (is.null(otherTxt)) {
      otherTxt <- ""
    }

    tagList(
      checkboxGroupInput(
        inputId = "engagedParticipants",
        label = "Participants engaged clinically",
        choices = sessionParticipants,
        selected = selected
      ),
      conditionalPanel(
        condition = "input.engagedParticipants.includes('other')",
        textInput(
          inputId = "engagedParticipants_other",
          label = "Please specify 'Other':",
          value = otherTxt
        )
      )
    )
  })

  output$addlPresentation <- renderUI({
    selectedPresentation <- input$addlPres

    if (is.null(selectedPresentation) || length(selectedPresentation) == 0) {
      return(p(
        "Please go back to Presentation and select at least one Additional Presentation"
      ))
    }

    # Generate a list of checkboxGroupInput elements dynamically
    staleIds <- isolate(staleInputs$ids)
    tagList(
      lapply(selectedPresentation, function(item) {
        inputId <- apInputId(item)
        otherId <- paste0(inputId, "_other")
        if (inputId %in% staleIds) {
          # Stored values for this input are from a previous note
          selected <- NULL
          prevOther <- ""
          isolate(
            staleInputs$ids <- setdiff(staleInputs$ids, c(inputId, otherId))
          )
        } else {
          selected <- isolate(input[[inputId]])
          prevOther <- isolate(input[[otherId]])
          if (is.null(prevOther)) {
            prevOther <- ""
          }
        }
        tagList(
          checkboxGroupInput(
            inputId = inputId,
            label = item,
            choices = c(
              otherPresFeatures$option[otherPresFeatures$type == item],
              "Other"
            ),
            selected = selected
          ),
          conditionalPanel(
            condition = sprintf("input.%s.includes('Other')", inputId),
            textInput(
              inputId = otherId,
              label = "Please specify 'Other':",
              value = prevOther
            )
          )
        )
      })
    )
  })

  replaceOther <- function(obj, otherLabel = "other") {
    objLst <- input[[obj]]
    if (is.null(objLst)) {
      return(NULL)
    }
    idx <- which(tolower(objLst) == tolower(otherLabel))
    if (length(idx)) {
      otherTxt <- trimws(input[[paste0(obj, "_other")]])
      if (is.null(otherTxt)) {
        otherTxt <- ""
      }

      if (nzchar(otherTxt)) {
        objLst[idx] <- tolower(otherTxt)
      } else {
        objLst <- objLst[-idx]
      }
    }
    return(objLst)
  }

  output$noteNarrative <- renderUI({
    ppl <- list2sentence(replaceOther("people"))
    place <- input$location
    sentence1 <- glue('This writer met with {ppl} {place}.')

    mood <- replaceOther('mood')
    affect <- replaceOther('affect')
    presBits <- c(
      if (length(mood)) glue("{list2sentence(mood)} mood"),
      if (length(affect)) glue("{list2sentence(affect)} affect")
    )
    addlCategories <- input$addlPres
    for (addCat in addlCategories) {
      newCat <- replaceOther(apInputId(addCat))
      if (length(newCat)) {
        catText <- glue('{list2sentence(newCat)} {tolower(addCat)}')
        presBits <- c(presBits, catText)
      }
    }

    sentence2 <- if (length(presBits)) {
      glue(
        "Client presented with {glue_collapse(presBits, sep = ', ', last = ', and ')}."
      )
    }

    engPpl <- list2sentence(replaceOther("engagedParticipants"))
    sentence3 <- glue(
      'This writer engaged {engPpl} in Socratic questioning and supportive reflections to facilitate exploration of client functioning and exploration of clinical material.'
    )

    noteText <- paste(sentence1, sentence2, sentence3)

    textAreaInput(
      inputId = 'finalNote',
      label = 'Narrative',
      width = '75%',
      value = noteText
    )
  })
}

# This writer met with (people) (location). Client presented with (moodType) mood and
# (affectType) affect, and (otherThing) as evidenced by verbalizations and nonverbals.
# This writer engaged (engagedPeople) in Socratic questioning and supportive reflections
# to facilitate exploration of ct functioning and exploration of clinical material.

shinyApp(ui, server)
