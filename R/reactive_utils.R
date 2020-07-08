library(shiny)
library(shinyjs)
library(purrr)

# Disable a list of inputs is a specific tab is selected
disable_inputs <- function(selectedPanel = NULL, disableInputsPanel = NULL, input_ids) {
  observe({
    if(selectedPanel() == disableInputsPanel) {
      walk(input_ids, shinyjs::disable)
    } else {
      walk(input_ids, shinyjs::enable)
    }
  })
}