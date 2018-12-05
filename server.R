


# Set up
library(dplyr)
library(shiny)
library(data.table)
library(tidyr)
library(ggplot2)
library(leaflet)
wa_rent_data <-
    fread('data/Rent Price/Neighborhood_MedianRentalPrice_AllHomes.csv') %>%
    filter(State == 'WA')
neighborhoods_rent <- wa_rent_data$RegionName %>% unique()



wa_sales_data <-
    fread('data/Home Values/Sale_Prices_Neighborhood.csv') %>%
    filter(StateName == 'Washington')
neighborhoods_sales <- wa_sales_data$RegionName %>% unique()

all_neighborhoods <- unique(c(neighborhoods_sales, neighborhoods_rent))
both_neighborhoods <- intersect(neighborhoods_sales, neighborhoods_rent)

#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#

# Functions:

## full_date_range <- names(wa_sales_data) %>%
##     tail(-4) %>%
##     paste0('-01') %>%
##     as.Date(format='%Y-%m-%d')

get_prices_for_neighboorhoods <- function (data, list_of_regions, date) {
    result <- data.frame(stringsAsFactors=FALSE)
    if ('(Select All)' %in% list_of_regions) {
        list_of_regions <- unique(data$RegionName)
    }

    for (name in list_of_regions) {
        row <- data %>%
            filter(RegionName == name) %>%
            head(1)
        point <- row %>%
            select(date[1]:date[2]) %>%
            gather(year_month, price) %>%
            mutate(region=name)
        result <- rbind(result, point)
    }

    result
}

get_aggregate <- function (price_data) {
    ## year_month  price  region
    price_data %>%
        group_by(year_month) %>%
        summarize(min_price=min(price),
                  max_price=max(price),
                  median_price=median(price))
}

render_plot <- function (data, kilo) {
    if (nrow(data) > 0) {
        ## transform price unit, if necessary
        ylabel <- 'Median Price (USD)'
        if (kilo) {
            data <- mutate(data, price=price / 1000)
            ylabel <- 'Median Price (Kilo USD)'
        }
        ## plot either individual lines or an aggregate
        use_aggregate <- length(unique(data$region)) > 6
        f <- ifelse(use_aggregate, render_ribbon, render_individual_lines)
        f(data, ylabel)
    }
}

render_individual_lines <- function (data, ylabel) {
    ggplot(data) +
        geom_line(aes(year_month, price, group=region, col=region)) +
        labs(x='Time', y=ylabel) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

render_ribbon <- function (data, ylabel) {
    data <- get_aggregate(data)
    ggplot(data) +
        geom_ribbon(aes(year_month, ymin=min_price, ymax=max_price, group=1),
                    fill='grey70', alpha=0.5) +
        geom_line(aes(year_month, median_price, group=2)) +
        labs(x='Time', y=ylabel) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

#Shiny App Server Side:

server <- function (input, output) {
    get_all_names <- reactive({
        switch(input$datasetFilter,
               'none'=all_neighborhoods,
               'both'=both_neighborhoods,
               'sales'=neighborhoods_sales,
               'rent'=neighborhoods_rent)
    })

    output$neighborhoodOut <- renderUI({
        selectInput('neighborhoodIn',
                    'Select neighborhoods',
                    multiple=TRUE,
                    choices=c('(Select All)', get_all_names()))
    })

    ## output$timeRangeOut <- renderUI({
    ##     min_date <- head(full_date_range, 1)
    ##     max_date <- tail(full_date_range, 1)
    ##     sliderInput('timeRangeIn',
    ##                 'Select a time range',
    ##                 min=min_date,
    ##                 max=max_date,
    ##                 value=c(min_date, max_date),
    ##                 timeFormat='%Y-%m')
    ## })

    # Another way to get date range, might give it a try
    output$timeRangeOut <- renderUI(dateRangeInput('dateRange',
                                    label = 'Date range input: yyyy-mm-dd',
                                    start = "2016-10-1", end = "2018-10-1",
                                    min = "2010-2-1", max = "2018-10-31",
                                    format = "yyyy-mm"
                           )
    )

    get_points <- reactive({
        get_prices_for_neighboorhoods(wa_sales_data,
                                      input$neighborhoodIn, format(as.Date(input$dateRange), "%Y-%m")
                                     )
    })

    get_points_rent <- reactive({
        get_prices_for_neighboorhoods(wa_rent_data,
                                      input$neighborhoodIn, format(as.Date(input$dateRange), "%Y-%m"))
    })

    output$salesPlot <- renderPlot({
        point <- get_points()
        render_plot(point, kilo=TRUE)
    })

    output$rentPlot <- renderPlot({
        point <- get_points_rent()
        render_plot(point, kilo=FALSE)
    })

    output$`Map goes here` <- renderLeaflet(
      leaflet() %>%
        setView(lng = -122.335167, lat = 47.608013, zoom = 11) %>%
        addTiles()
    )
}

## Histogram (we don't have enough data to plot this)
## all_plotable_data <- wa_sales_data %>%
##     get_prices_for_neighboorhoods(c('(Select All)')) %>%
##     filter(year_month == '2018-01')
## ggplot(all_plotable_data) + geom_histogram(aes(x=price))
