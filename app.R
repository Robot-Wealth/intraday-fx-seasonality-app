library(shiny)
library(tidyverse)
library(shinyjs)

# TODO:
# consider modules for plot outputs

timezone_list <- c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY")
timezone_map <- c("USD" = "ET", "EUR" = "CET", "JPY" = "JST")
assets_list <- list(
    "USD" = c("AUDUSD", "CADUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD"),
    "EUR" = c("AUDEUR", "CADEUR", "CHFEUR", "GBPEUR", "JPYEUR", "USDEUR"),
    "JPY" = c("AUDJPY", "CADJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY")
)

source(here::here("R", "server_shared.R"), local = TRUE)  # visible to server, all sessions
source(here::here("R", "mod_dateslider.R"), local = FALSE)  # visible to server, all sessions

ui <- navbarPage(
    shinyjs::useShinyjs(),
    
    title = "FX Intraday Seasonality",
    id = "tab",
    selected = "seasonality",
    
    tabPanel(
        "Seasonality",
        value = "seasonality",
        
        # pick assets and date range - explore changing seasonality relationships
        # press button to plot mean cum returns by asset and by year (2 different views)
        sidebarLayout(
            sidebarPanel(
                mod_tz_asset_selector_ui("seas", timezone_list, assets_list, "USD in ET"),
                fluidRow(
                    mod_dateslider_ui("barline_datesliders", 2009, 2020, 2009, 2013, 1L),
                ),
                fluidRow(
                    checkboxInput("detrendCheckbox", "Detrend Returns", value = TRUE)
                )
            ),
            mainPanel(
                tabsetPanel(type = "tabs", id = "seasonalityPanels",
                    tabPanel(
                        value = "granular",
                        "Granular in Time and Asset",
                        plotOutput("seasonalityPlot", height = "600px"),
                    ),
                    tabPanel(
                        value = "byTimezone",
                        "By Timezone",
                        fluidRow(
                            column(12, plotOutput("facetYearPlot", height = "700px"))
                        ),
                        fluidRow(
                            column(12, plotOutput("facetAssetPlot", height = "600px"))
                        )
                    )
                )
            )
        )
    ),
    
    tabPanel(
        "Heatmaps",
        value = "heatmaps",
        sidebarLayout(
            sidebarPanel(
                # selectizeInput(
                #     "timezoneSelectorHM",
                #     "Select Local Currency and Time Zone",
                #     choices = c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY"),
                #     selected = "USD in ET",
                #     multiple = FALSE
                # ),
                
            ),
            mainPanel()
            
        ),
    ),
    
    tabPanel(
        "Evoloving Heatmaps",
        value = "evoHeatmaps"
    )
)

server <- function(input, output, session) {
    
    # Seasonality plot reactives ========
    
    tz_assets <- mod_tz_asset_selector_server(id = "seas", timezone_list, assets_list, reactive(input$seasonalityPanels), "byTimezone") # reactiveValues
    date_range <- mod_dateslider_server("barline_datesliders", reactive(input$seasonalityPanels), "byTimezone")  # reactiveValues
    
    output$seasonalityPlot <- renderPlot({
        req(date_range, tz_assets)
        seasonality_barlineplot(
            returns_df, 
            tz_assets$assets, 
            timezone = timezone_map[[tz_assets$timezone]], 
            years = date_range$startYear:date_range$endYear, 
            detrend = input$detrendCheckbox
        )
    })
    
    output$facetYearPlot <- renderPlot({
        req(tz_assets)
        
        returns_df %>% 
            seasonality_facet_year_plot(
                tickers = assets_list[[tz_assets$timezone]], 
                timezone = timezone_map[[tz_assets$timezone]],
                detrend = input$detrendCheckbox
            )
        
    })
    
    output$facetAssetPlot <- renderPlot({
        req(tz_assets)
        
        returns_df %>% 
            seasonality_facet_asset_plot(
                tickers = assets_list[[tz_assets$timezone]], 
                timezone = timezone_map[[tz_assets$timezone]],
                detrend = input$detrendCheckbox
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
