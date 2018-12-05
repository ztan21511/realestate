library(shiny)
library(leaflet)

ui <- navbarPage('Exploring real estate price variations in WA', 
                  header=tags$style(type="text/css",
                            ".recalculating {opacity: 1.0;}"), 
                  id = "conditionedPanels",
                  tabPanel("Real Estate Statistics",
                            sidebarLayout(
                            sidebarPanel(
                                  helpText('See sales only, rent only or both?'),
                                  radioButtons('datasetFilter', 'Dataset filter',
                                               c('Show all neighborhoods'='none',
                                                 'Have sales price data'='sales',
                                                 'Have rental price data'='rent',
                                                 'Have both datasets'='both'),
                                               selected='both'),
                                  uiOutput('neighborhoodOut'),
                                  uiOutput('timeRangeOut')
                            ),
                  
                            mainPanel(
                                tabsetPanel(
                                    type='tabs',
                                    tabPanel(
                                        'Rental price fluctuation',
                                        plotOutput('rentPlot')
                                    ),
                                    tabPanel(
                                        'Sales price fluctutation',
                                        plotOutput('salesPlot')
                                    )
                  
                                )
                            )
                        )
                  ),
                    
                  tabPanel("Price History and Prediction Map",
                           
                           leafletOutput("seattlePriceMap"),
                           uiOutput("valueYrSlider")
                           
                           
                  )
                    
)

