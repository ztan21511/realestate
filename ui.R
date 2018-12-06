# Set-Up

library(shiny)
#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#

# UI-Side

ui <- navbarPage('Exploring real estate price variations in WA',
                  id = "conditionedPanels",
                 header=tags$style(type="text/css",
                                   ".recalculating {opacity: 0.75;}"), 
                 
                  tabPanel("Real Estate Statistics",
                            sidebarLayout(
                            sidebarPanel(
                                  tags$p('The purpose of this project is to
                                            observe the ever-changing real
                                         estate market in the state of
                                         Washington. The state of Washington
                                         consists of a variety of affluent
                                         neighborhoods that have real-estate
                                         potential, and this app is targeted
                                         towards users who want to choose
                                         particular areas in Washington that
                                         they may invest in. Our app allows
                                         users to select specific neighborhoods
                                         in Washington, observe the fluctuating
                                         rental and sale prices in one
                                         neighborhood, and compare it to the
                                         other. This ultimately allows users to
                                         discern which particular neighborhood
                                         would have a higher market potential.
                                         The source of our data comes from the
                                         Zillow Home Value index (ZHVI) in the
                                         form of CSV files                   '),
                                  helpText('See sales only, rent only or both?'),
                                  radioButtons('datasetFilter', 'Dataset filter',
                                               c('Show all neighborhoods'='none',
                                                 'Have sales price data'='sales',
                                                 'Have rental price data'='rent',
                                                 'Have both datasets'='both'),
                                               selected='both'),
                                  uiOutput('neighborhoodOut'),
                                  uiOutput('timeRangeOut'),
                                  helpText("Upon selecting six or more neighborhoods
                                            for either rental or sale prices, the
                                           visualization will evolve into a summary
                                           trend line graph to indicate the overall
                                           trend combining all selected neighborhoods"),
                                  tags$a("Zillow Data", href="https://www.zillow.com/research/data"),
                                  br(),
                                  tags$a("Source Code", href = "https://github.com/ztan21511/final-project-info201")
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
                           
                           sidebarPanel(uiOutput("valueYrSlider")#,
                                        #uiOutput("PropertyClassSelect"),
                                        #uiOutput("PropertyTypeSelect")
                                        ),
                           #leafletOutput("seattlePriceMap"),
                           mainPanel(tags$p('This map plots historical King County Assessor
                                              sales price data summaries for Micro Community Policing Plan Neighborhoods.
                                              This data is created from spatial joins between MCPP data from the SPD,
                                            King County GIS Shapefiles for parcels, and Assessor Sales Price data by parcel.'),
                             plotOutput("ggplotMap", height="600px", width="400px"))
                           
                           
                           
                  )
                    
)