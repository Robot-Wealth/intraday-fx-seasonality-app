library(shiny)
library(shinyjs)
library(purrr)

# Disable a list of inputs in a specific tab is selected
disable_inputs <- function(selected_panel = NULL, disable_inputs_panel = NULL, input_ids) {
  observe({
    if(selected_panel() == disable_inputs_panel) {
      walk(input_ids, shinyjs::disable)
    } else {
      walk(input_ids, shinyjs::enable)
    }
  })
}