library(shiny)

ui <- fluidPage(
    titlePanel('Exploring affordable housing options in Seattle'),
    sidebarLayout(
        sidebarPanel(
            helpText('See sales only, rent only or both?'),
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
