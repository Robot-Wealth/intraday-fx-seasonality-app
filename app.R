library(shiny)
library(tidyverse)

# TODO:
# on seasonality barline plots set y axis limits to constant
# make returns data available on load

assets_list <- c("AUDUSD", "CADUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD")

source(here::here("R", "server_shared.R"), local = TRUE)  # visible to server, all sessions
source(here::here("R", "mod_dateslider.R"), local = FALSE)  # visible to server, all sessions

ui <- navbarPage(
    
    title = "FX Intraday Seasonality",
    id = "tab",
    selected = NULL,
    
    tabPanel(
        # pick assets and date range - explore changing seasonality relationships
        # press button to plot mean cum returns by asset and by year (2 different views)
        "Seasonality",
        value = "seasonality",
        
        sidebarLayout(
            sidebarPanel(
                selectizeInput("assets", "Select Assets", choices = assets_list, selected = assets_list[1:3], multiple = TRUE),
                fluidRow(
                    # mod_dateslider_ui("barline_datesliders", 2009, 2020, 2009, 1L),
                    column(6, sliderInput(
                        "startYear",
                        "Select Start Year",
                        min = 2009,
                        max = 2020,
                        value = 2009,
                        step = 1L,
                        round = TRUE,
                        sep = "",
                        ticks = FALSE
                    )),
                    column(6, sliderInput(
                        "endYear",
                        "Select End Year",
                        min = 2009,
                        max = 2020,
                        value = 2010,
                        step = 1L,
                        round = TRUE,
                        sep = "",
                        ticks = FALSE
                    ))
                ),
                fluidRow(
                    column(12, align = "center", actionButton("showFacetPlots", "SHOW ALL ASSETS/YEARS", style="color: #000000; background-color: #F0B176"))
                )
            ),
            mainPanel(
                plotOutput("seasonalityPlot", height = "500px"),
                fluidRow(
                    column(6, plotOutput("facetYearPlot")),
                    column(6, plotOutput("facetAssetPlot"))
                )
            )
        )
    ),
    
    # Animation showing seasonality evolving over time
    # Pick assets? Or just have the image pre-made?
    # Consider clicking a button to show on previous tab
    
    tabPanel(
        "Evolving Seasonality",
        value = "evoSeasonality",
        tabsetPanel(
            id = "animtabs",
            tabPanel(
                "Evolving Seasonality by Asset",
                imageOutput("facetAssetAnim")
            ),
            tabPanel(
                "Evolving Seasonality by Year",
                imageOutput("facetYearAnim")
            )
        )
    ),
    
    tabPanel(
        "Heatmaps",
        value = "heatmaps",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(
                    "timezoneSelector",
                    "Select Local Currency and Time Zone",
                    choices = c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY"),
                    selected = "USD in ET",
                    multiple = FALSE
                ),
                
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
    
    observeEvent(input$startYear, {
        if(input$startYear > input$endYear)
            updateSliderInput(session, "endYear", value = input$startYear)
    })

    observeEvent(input$endYear, {
        if(input$startYear > input$endYear)
            updateSliderInput(session, "startYear", value = input$endYear)
    })
    
    # date_range <- mod_dateslider_server("barline_datesliders")
    # todo: return start and end dates as reacitves
    # or incorporate seasonality plot in module - but is placed elsewhere in ui, prob won't work
    
    # output$seasonalityPlot <- renderPlot({
    #     req(date_range)
    #     seasonality_barlineplot(returns_df, input$assets, years = date_range$startYear:date_range$endYear)
    # })
    
    output$seasonalityPlot <- renderPlot({
        seasonality_barlineplot(returns_df, input$assets, years = input$startYear:input$endYear)
    })
    
    output$facetYearPlot <- renderPlot({
        req(input$showFacetPlots)
        
        returns_df %>% 
            seasonality_facet_year_plot()
        
    })
    
    output$facetAssetPlot <- renderPlot({
        req(input$showFacetPlots)
        
        returns_df %>% 
            seasonality_facet_asset_plot()
        
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
