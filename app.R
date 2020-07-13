library(shiny)
library(tidyverse)
library(shinyjs)
library(shinycssloaders)

# TODO:
# tests
# consider modules for plot outputs
# replace indexing lists by values of other lists with switch statements
# make data: offset/detrended subsets for each tz
# dynamic ui in heatmap panels needs some work - currently removing radio buttons on middle tab, would be better to disable
# pot chachign would be really useful
# display IR value of heatmap on mouse hover (partially implemented)

ui <- navbarPage(
    shinyjs::useShinyjs(),
    
    title = "FX Intraday Seasonality",
    id = "tab",
    selected = "seasonality",
    
    # Seasonality bar and line plots ====
    
    tabPanel(
        "Seasonality",
        value = "seasonality",
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
    
    # Heatmaps ==========================
    
    tabPanel(
        "Heatmaps",
        value = "heatmaps",
        sidebarLayout(
            sidebarPanel(
                mod_tz_asset_selector_ui("hmAssetsTZ", timezone_list, hm_assets_list, "USD in ET"),
                fluidRow(
                    column(6, uiOutput("hmTimeSubsets")), # generate this element dynamically based on selected tab
                    column(6, radioButtons("hmOffset", "Hour Offset (Minutes)", choices = c("0", "17", "33"), selected = "0"))
                ),
                fluidRow(
                    column(6, checkboxInput("hmDetrendCheckbox", "Detrended Performance Data", value = TRUE)),
                    column(6, textOutput("hmIR"))
                ),
                fluidRow(
                    column(6, checkboxInput("ShowValues", "Show Values on Heatmap", value = TRUE)),
                    column(6, numericInput("TextSize", "Heatmap Text Size", 4, min = 0, max = 100, step = 0.5))
                )
            ),
            mainPanel(
                tabsetPanel(type = "tabs", id = "heatmapPanels",
                    tabPanel(
                        value = "hmGranular",
                        "Granular in Time and Asset",
                        plotOutput(
                            "heatmapPlot", 
                            height = "600px",
                            # hover = hoverOpts(id = "heatmapHover", delayType = "debounce")
                        ) %>% 
                            withSpinner(),
                    ),
                    tabPanel(
                        value = "hmByTime",
                        "Tickers by Time Subset",
                        fluidRow(column(12, plotOutput(
                            "hmFacetYearPlot", 
                            height = "900px",
                            # hover = hoverOpts(id = "facetYearHover", delayType = "debounce")
                        ) %>% withSpinner()))
                    ),
                    tabPanel(
                        value = "hmByAsset",
                        "Time Subset by Tickers",
                        fluidRow(column(12, plotOutput(
                            "hmFacetAssetPlot", 
                            height = "900px",
                            # hover = hoverOpts(id = "facetAssetHover", delayType = "debounce")
                        ) %>% withSpinner()))
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Seasonality plot reactives ========
    
    tz_assets <- mod_tz_asset_selector_server(id = "seas", timezone_list, assets_list, reactive(input$seasonalityPanels), "byTimezone") 
    date_range <- mod_dateslider_server("barline_datesliders", reactive(input$seasonalityPanels), "byTimezone")  
    
    output$seasonalityPlot <- renderCachedPlot(
        {
            req(date_range, tz_assets)
            seasonality_barlineplot(
                returns_df, 
                tz_assets$assets, 
                timezone = timezone_map[[tz_assets$timezone]], 
                years = date_range$startYear:date_range$endYear, 
                detrend = input$detrendCheckbox
            )
        },
        cacheKeyExpr = {
            list(
                tz_assets$assets,
                tz_assets$timezone,
                date_range$startYear,
                date_range$endYear,
                input$detrendCheckbox
            )
        }
    )
    
    output$facetYearPlot <- renderCachedPlot(
        {
            req(tz_assets)
            
            returns_df %>% 
                seasonality_facet_year_plot(
                    tickers = assets_list[[tz_assets$timezone]], 
                    timezone = timezone_map[[tz_assets$timezone]],
                    detrend = input$detrendCheckbox
                )
        },
        cacheKeyExpr = {
            list(
                tz_assets$assets,
                tz_assets$timezone,
                input$detrendCheckbox
            )
        }
    )
    
    output$facetAssetPlot <- renderPlot({
        req(tz_assets)
        
        returns_df %>% 
            seasonality_facet_asset_plot(
                tickers = assets_list[[tz_assets$timezone]], 
                timezone = timezone_map[[tz_assets$timezone]],
                detrend = input$detrendCheckbox
            )
    })
    
    # Heatmap plot reactives ============
    
    output$hmTimeSubsets <- renderUI({
        if(input$heatmapPanels == "hmByAsset") {
            checkboxGroupInput("hmDateRangesBoxes", "Date Ranges", choices = hm_date_ranges[1:4], selected = hm_date_ranges[1:4])
        } else if(input$heatmapPanels == "hmByTime") {
           NULL
        } else {
            radioButtons("hmDateRangesRadio", "Date Range", choices = hm_date_ranges, selected = "2009-2020")
        }
    })
    
    hm_tz_assets <- mod_tz_asset_selector_server(id = "hmAssetsTZ", timezone_list, hm_assets_list, NULL, NULL)  
    
    output$heatmapPlot <- renderCachedPlot(
        {
            req(input$hmDateRangesRadio, hm_tz_assets)
            heatmap_plot(
                performance_df, 
                tickers = hm_tz_assets$assets, 
                timezone = timezone_map[[hm_tz_assets$timezone]], 
                years = input$hmDateRangesRadio, 
                detrend = input$hmDetrendCheckbox,
                hour_offset = as.numeric(input$hmOffset),
                show_values = input$ShowValues,
                label_size = input$TextSize
            )
        },
        cacheKeyExpr = { 
            list(
                input$hmDateRangesRadio, 
                hm_tz_assets$assets,
                hm_tz_assets$timezone,
                input$hmDateRangesRadio, 
                input$hmDetrendCheckbox, 
                input$hmOffset,
                input$ShowValues,
                input$TextSize
            )
        }
    )
    
    output$hmFacetYearPlot <- renderCachedPlot(
        {
            req(hm_tz_assets)
            
            performance_df %>% 
                compose_facet_year_heatmaps(
                    tickers = hm_tz_assets$assets, 
                    timezone = timezone_map[[hm_tz_assets$timezone]], 
                    detrend = input$hmDetrendCheckbox, 
                    hour_offset = input$hmOffset,
                    show_values = input$ShowValues,
                    label_size = input$TextSize
                )
        },
        cacheKeyExpr = {
            list(
                hm_tz_assets$assets,
                hm_tz_assets$timezone,
                input$hmDetrendCheckbox,
                input$hmOffset,
                input$ShowValues,
                input$TextSize
            )
        }
    )
    
    output$hmFacetAssetPlot <- renderCachedPlot(
        {
            req(hm_tz_assets, input$hmDateRangesBoxes)
    
            performance_df %>%
                compose_facet_asset_heatmaps(
                    tickers = hm_tz_assets$assets, 
                    year_subsets = input$hmDateRangesBoxes, 
                    timezone = timezone_map[[hm_tz_assets$timezone]], 
                    detrend = input$hmDetrendCheckbox,
                    hour_offset = input$hmOffset,
                    show_values = input$ShowValues,
                    label_size = input$TextSize
                )
        },
        cacheKeyExpr = {
            list(
                hm_tz_assets$assets,
                hm_tz_assets$timezone,
                input$hmDateRangesBoxes,
                input$hmDetrendCheckbox,
                input$hmOffset,
                input$ShowValues,
                input$TextSize
            )
        }
    )
    
    # hover_out <- reactive(
    #     switch(
    #         input$heatmapPanels,
    #         "hmGranular" = input$heatmapHover,
    #         "hmByTime" = input$facetYearHover,
    #         "hmByAsset" = input$facetAssetHover
    #     )
    # )
    # 
    # output$hmIR <- renderPrint(hover_out())
}

# Run the application 
shinyApp(ui = ui, server = server)
