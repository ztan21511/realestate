library(dplyr)
library(shiny)
library(data.table)
library(tidyr)
library(ggplot2)

wa_rent_data <-
    fread('data/Rent Price/Neighborhood_MedianRentalPrice_AllHomes.csv') %>%
    filter(State == 'WA' & CountyName == 'King County')
neighborhoods_rent <- wa_rent_data$RegionName %>% unique()

wa_sales_data <-
    fread('data/Home Values/Sale_Prices_Neighborhood.csv') %>%
    filter(StateName == 'Washington')
neighborhoods_sales <- wa_sales_data$RegionName %>% unique()

get_prices_for_neighboorhoods <- function (data, list_of_regions) {
    result <- data.frame(stringsAsFactors=FALSE)

    for (name in list_of_regions) {
        row <- data %>%
            filter(RegionName == name) %>%
            head(1)
        point <- row %>%
            select(`2016-10`:`2018-10`) %>%
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
        xlabel <- 'Time'
        ylabel <- 'Price (USD)'
        if (kilo) {
            data <- mutate(data, price=price / 1000)
            ylabel <- 'Price (Kilo USD)'
        }
        ggplot(data) +
            geom_line(aes(year_month, price, group=region, col=region)) +
            labs(x=xlabel, y=ylabel) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
}


server <- function (input, output) {
    get_all_names <- reactive({
        unique(c(neighborhoods_sales, neighborhoods_rent))
    })

    output$neighborhoodOut <- renderUI({
        selectInput('neighborhoodIn',
                    'Select neighborhoods',
                    multiple=TRUE,
                    choices=c('(Select All)', get_all_names()))
    })

    get_points <- reactive({
        get_prices_for_neighboorhoods(wa_sales_data,
                                      input$neighborhoodIn)
    })

    output$salesPlot <- renderPlot({
        point <- get_points()
        render_plot(point, TRUE)
    })

    output$rentPlot <- renderPlot({
        point <- get_prices_for_neighboorhoods(wa_rent_data,
                                               input$neighborhoodIn)
        render_plot(point, FALSE)
    })

    output$test <- renderText({
        "Hello world!"
    })
}
