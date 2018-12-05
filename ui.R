library(shiny)

ui <- fluidPage(
    titlePanel('Exploring affordable housing options in Seattle'),
    sidebarLayout(
        sidebarPanel(
            helpText('See sales only, rent only or both?'),
            radioButtons('datasetFilter', 'Dataset filter',
                         c('Show all neighborhoods'='none',
                           'Have sales price data'='sales',
                           'Have rental price data'='rent',
                           'Have both datasets'='both'),
                         selected='none'),
            uiOutput('neighborhoodOut'),
            textOutput('debug')
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
                )))))
