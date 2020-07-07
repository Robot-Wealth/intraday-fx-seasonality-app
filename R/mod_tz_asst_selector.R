library(shiny)
# tz_list: named list whose values correspond to the names (keys) in asst_list. Keys are shown to user in select input dropdown.
# asset_list: named list of lists whose names correspond to the values in tz_list. Sublists populate select input dropdown.

# could pass panel id iot disable assets selector....

timezone_list <- c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY")
timezone_map <- c("USD" = "ET", "EUR" = "CET", "JPY" = "JST")
assets_list <- list(
  "USD" = c("AUDUSD", "CADUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD"),
  "EUR" = c("AUDEUR", "CADEUR", "CHFEUR", "GBPEUR", "JPYEUR", "USDEUR"),
  "JPY" = c("AUDJPY", "CADJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY")
)

mod_tz_asset_selector_ui <- function(id, tz_list, asst_list, selected_tz) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(
      ns("tzSelector"),
      "Select Local Currency and Time Zone",
      choices = tz_list,
      selected = tz_list[[selected_tz]],
      multiple = FALSE
    ),
    
    selectizeInput(
      ns("assets"), 
      "Select Assets", 
      choices = asst_list[[tz_list[[selected_tz]]]], 
      selected = asst_list[[tz_list[[selected_tz]]]][1:3], 
      multiple = TRUE
    )
  )
  
}

mod_tz_asset_selector_server <- function(id, tz_list, asst_list) {
  moduleServer(id, function(input, output, session) {
    
    selected <- reactiveValues(assets = NULL, timezone = NULL)
    
    observeEvent(input$tzSelector, {
      # message(cat(input$tzSelector))
      
      updateSelectizeInput(
        session = session, 
        inputId = "assets", 
        choices = asst_list[[input$tzSelector]], 
        selected = asst_list[[input$tzSelector]][1:3]
      )
      
      selected$assets <- input$assets
      selected$timezone <- input$tzSelector
      
    })
    
    selected
    
  })
}

tz_asset_selector_app <- function() {
  ui <- fluidPage(
    mod_tz_asset_selector_ui(id = "seasAsstSelector", timezone_list, assets_list, "USD in ET")
  )
  
  server <- function(input, output, session) {
    mod_tz_asset_selector_server(id = "seasAsstSelector", timezone_list, assets_list)
  }
  
  shinyApp(ui, server)
}

mod_seas_tabset_ui <- function(id) {
  tabsetPanel(
    type = "tabs", 
    id = "seasonalityPanels",
    tabPanel(
      value = "granular",
      "Granular in Time and Asset",
      # seasonality barline plots will go here
    ),
    tabPanel(
      value = "byTimezone",
      "By Timezone",
      # facet plot outputs will go here
    )
  )
}




tz_asset_selector_app()
