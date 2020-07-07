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
      "timezoneSelector",
      "Select Local Currency and Time Zone",
      choices = tz_list,
      selected = tz_list[[selected_tz]],
      multiple = FALSE
    ),
    
    selectizeInput(
      "assets", 
      "Select Assets", 
      choices = asst_list[[tz_list[[selected_tz]]]], 
      selected = asst_list[[tz_list[[selected_tz]]]][1:3], 
      multiple = TRUE
    )
  )
  
}

mod_tz_asset_selector_server <- function(id, tz_list, asst_list) {
  moduleServer(id, function(input, output, session) {
    observeEvent((input$timezoneSelector), {
      updateSelectizeInput(
        session, 
        "assets", 
        choices = asst_list[[tz_list[[input$timezoneSelector]]]], 
        selected = asst_list[[tz_list[[input$timezoneSelector]]]][1:3]
      )
      
    })
    
    # observe({
    #   if(input$seasonalityPanels == "byTimezone") {
    #     shinyjs::disable(id = "assets")
    #   } else {
    #     shinyjs::enable(id = "assets")
    #   }
    # })
  }
    
  )
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

tz_asset_selector_app()
