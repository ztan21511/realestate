library(shiny)
library(leaflet)

ui <- navbarPage('Exploring real estate price variations in WA', 
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
                    
                  tabPanel("Leaflet Map",
                           textOutput("Map goes here")
                  )
                    
)

