# module for setting two date range sliders and guaranteeing end date is not before start date

mod_dateslider_ui <- function(id, min, max, value, step) {
  ns <- NS(id)
  
  tagList(
    column(6, sliderInput(
      ns("startYear"), 
      "Select Start Year",
      min = min, 
      max = max, 
      value = value, 
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
      value = value, 
      step = step, 
      round = TRUE, 
      sep = "", 
      ticks = FALSE
    ))
    
  )
}

mod_dateslider_server <- function(id) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      slider_vals <- reactiveValues(start_year = NULL, end_year = NULL)
      
      observeEvent(input$startYear, {
        if(input$startYear > input$endYear)
          updateSliderInput(session, "endYear", value = input$startYear)
        
        slider_vals$start_year <- input$startYear
        slider_vals$end_year <- input$endYear
        
      })
      
      observeEvent(input$endYear, {
        if(input$startYear > input$endYear)
          updateSliderInput(session, "startYear", value = input$endYear)
        
        slider_vals$start_year <- input$startYear
        slider_vals$end_year <- input$endYear
      })
      
      
      
      return(slider_vals)
      
    }
  )
}