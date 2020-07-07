library(shiny)
library(tidyverse)
library(shinyjs)

# TODO:
# remove rendered image code
# modularise...but what? read up on how to modularize for sidebar/main panel - modules in modules...


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
                selectizeInput(
                    "timezoneSelector",
                    "Select Local Currency and Time Zone",
                    choices = timezone_list,
                    selected = timezone_list[["USD in ET"]],
                    multiple = FALSE
                ),
                selectizeInput("assets", "Select Assets", choices = assets_list[["USD"]], selected = assets_list[["USD"]][1:3], multiple = TRUE),
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
    
    # Animation showing seasonality evolving over time
    # Pick assets? Or just have the image pre-made?
    # Consider clicking a button to show on previous tab
    
    # tabPanel(
    #     "Evolving Seasonality",
    #     value = "evoSeasonality",
    #     sidebarLayout(
    #         sidebarPanel(
    #             fluidRow(
    #                 column(6, checkboxInput("detrendEvoCheckbox", "Detrend Returns", value = TRUE))
    #             ),
    #             fluidRow(
    #                 selectizeInput(
    #                     "timezoneSelector",
    #                     "Select Local Currency and Time Zone",
    #                     choices = timezone_list,
    #                     selected = timezone_list[["USD in ET"]],
    #                     multiple = FALSE
    #                 )
    #             ),
    #             width = 3
    #         ),
    #         mainPanel(
    #             fluidRow(
    #                 column(12, plotOutput("facetYearPlot", height = "700px"))
    #             ),
    #             fluidRow(
    #                 column(12, plotOutput("facetAssetPlot", height = "600px"))
    #             )
    #         )
    #     )
    # ),
    
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
    
    observeEvent((input$timezoneSelector), {
        updateSelectizeInput(
            session, 
            "assets", 
            choices = assets_list[[input$timezoneSelector]], 
            selected = assets_list[[input$timezoneSelector]][1:3]
        )
        
    })
    
    observe({
        if(input$seasonalityPanels == "byTimezone") {
            shinyjs::disable(id = "assets")
        } else {
            shinyjs::enable(id = "assets")
        }
    })
            
    
    date_range <- mod_dateslider_server("barline_datesliders")
    # todo: return start and end dates as reacitves
    # or incorporate seasonality plot in module - but is placed elsewhere in ui, prob won't work
    
    output$seasonalityPlot <- renderPlot({
        req(date_range, input$assets)
        seasonality_barlineplot(
            returns_df, 
            input$assets, 
            timezone = timezone_map[[input$timezoneSelector]], 
            years = date_range$startYear:date_range$endYear, 
            detrend = input$detrendCheckbox
        )
    })
    
    output$facetYearPlot <- renderPlot({
        # req(input$showFacetPlots)
        
        returns_df %>% 
            seasonality_facet_year_plot(
                tickers = assets_list[[input$timezoneSelector]], 
                timezone = timezone_map[[input$timezoneSelector]],
                detrend = input$detrendCheckbox
            )
        
    })
    
    output$facetAssetPlot <- renderPlot({
        # req(input$showFacetPlots)
        
        returns_df %>% 
            seasonality_facet_asset_plot(
                tickers = assets_list[[input$timezoneSelector]], 
                timezone = timezone_map[[input$timezoneSelector]],
                detrend = input$detrendCheckbox
            )
        
    })
    
    output$facetAssetAnim <- renderImage({
        
        list(
            src = "assetByYear.gif",
            contentType = "image/gif"
            # width = 600
        )
        
    }, deleteFile = FALSE)
    
    output$facetYearAnim <- renderImage({
        
        list(
            src = "yearByAsset.gif",
            contentType = "image/gif"
            # width = 600
        )
        
    }, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)
