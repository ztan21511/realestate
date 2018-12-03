library(dplyr)
library(shiny)
library(data.table)
library(tidyr)

crime_rate_data <- fread('data/Crime_Data.csv')
seasonal_price_data <- fread('data/Rent Price/Neighborhood_MedianRentalPrice_AllHomes.csv')
wa_seasonal_price_data <- seasonal_price_data %>%
    filter(State == 'WA' & CountyName == 'King County')

get_prices_for_neighboorhoods <- function (list_of_regions) {
    result <- data.frame(row.names=c('year_month', 'price', 'region'),
                         stringsAsFactors=FALSE)

    for (name in list_of_regions) {
        row <- wa_seasonal_price_data %>%
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


point <- get_prices_for_neighboorhoods(c('Downtown',
                                         'Capitol Hill',
                                         'Greenwood'))
ggplot(point) +
    geom_line(aes(year_month, price, group=region, col=region)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

server <- function (input, output) {
    output$test <- renderText({
        "Hello world!"
    })
}
