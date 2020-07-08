# Module for setting two date range sliders and guaranteeing end date is not before start date
# Optionally disable date sliders on a given tabPanel

library(shiny)
library(here)

source(here::here("R", "reactive_utils.R"))

mod_dateslider_ui <- function(id, min, max, value_start, value_end, step) {
  ns <- NS(id)
  
  tagList(
    column(6, sliderInput(
      ns("startYear"), 
      "Select Start Year",
      min = min, 
      max = max, 
      value = value_start, 
      step = step, 
      round = TRUE, 
      sep = "", 
      ticks = FALSE
    )),
    column(6, sliderInput(
      ns("endYear"), 
      "Select End Year", 
      min = min, 
      max = max, 
      value = value_end, 
      step = step, 
      round = TRUE, 
      sep = "", 
      ticks = FALSE
    ))
    
  )
}

mod_dateslider_server <- function(id, selectedPanel = NULL, disableInputsPanel = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    if(!is.null(selectedPanel)) {
      disable_inputs(selectedPanel, disableInputsPanel, list("startYear", "endYear"))
    }
      
    slider_vals <- reactiveValues(start_year = NULL, end_year = NULL)
    
    observeEvent(input$startYear, {
      if(input$startYear > input$endYear)
        updateSliderInput(session, "endYear", value = input$startYear)
      
      slider_vals$startYear <- input$startYear
      slider_vals$endYear <- input$endYear
      
    })
    
    observeEvent(input$endYear, {
      if(input$startYear > input$endYear)
        updateSliderInput(session, "startYear", value = input$endYear)
      
      slider_vals$startYear <- input$startYear
      slider_vals$endYear <- input$endYear
    })
    
    return(slider_vals)
  })
}

# For testing
dateslider_app <- function() {
  ui <- fluidPage(
    mod_dateslider_ui(id = "testslider1", 1, 10, 5, 10, 1),
    mod_dateslider_ui(id = "testslider2", 20, 30, 25, 26, 1)
  )
  
  server <- function(input, output, session) {
    mod_dateslider_server(id = "testslider1")
    mod_dateslider_server(id = "testslider2")
  }
  
  shinyApp(ui, server)
}