library(shiny)

ui <- fluidPage(
    titlePanel('Exploring affordable housing options in Seattle'),
    sidebarLayout(
        sidebarPanel(
            helpText('Data'),
            selectInput('neighborhoodOut',
                        'Select neighborhoods you are interested in',
                        multiple=TRUE,
                        choices=c('Soda', 'Cascade', 'Wallingford'))
        ),
        mainPanel(
            tabsetPanel(
                type='tabs',
                tabPanel(
                    'Price fluctuation',
                    helpText('Plot goes here!'),
                    helpText('Table goes here!')
                ),
                tabPanel(
                    'Crime rate fluctutation',
                    helpText('Plot goes here!'),
                    helpText('Table goes here!')
                )))))
