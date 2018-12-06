# Set up
library(dplyr)
library(shiny)
library(data.table)
library(tidyr)
library(ggplot2)
library(sf)

wa_rent_data <-
    fread('data/Rent Price/Neighborhood_MedianRentalPrice_AllHomes.csv') %>%
    filter(State == 'WA')
neighborhoods_rent <- wa_rent_data$RegionName %>% unique()

wa_sales_data <-
    fread('data/Home Values/Sale_Prices_Neighborhood.csv') %>%
    filter(StateName == 'Washington')
neighborhoods_sales <- wa_sales_data$RegionName %>% unique()

spd_mcpp_neighborhoods <- st_read(dsn='data/Seattle Police Micro-Community Policing Plans Neighborhoods/geo_export_29a19543-7dda-45bf-99f2-2be3980bb1e9.shp', stringsAsFactors = FALSE)

neighborhoodAvgSale <- read.csv("data/neighborhoodAvgSale.csv", stringsAsFactors=FALSE)

#was using these to provide select options to the user, but the file and filter
#times were rather slow
#types <- c(rbind(data.frame(val="All"), data.frame(val=levels(factor(neighborhoodAvgSale$HumanReadablePropertyType)))))

#classes <- c(rbind(data.frame(val="All"), data.frame(val=levels(factor(neighborhoodAvgSale$HumanReadablePropertyClass)))))


all_neighborhoods <- unique(c(neighborhoods_sales, neighborhoods_rent))
both_neighborhoods <- intersect(neighborhoods_sales, neighborhoods_rent)

#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#

# Functions:

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
                                    format = "yyyy-mm", startview = "year"
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


    #allow the user to select a year of avg sales prices for neighborhoods
    changeAnimation <- reactive({
      #print(input$frameWindow)
      sliderInput("animation",
                  "Year",
                  min = min(neighborhoodAvgSale$Yr),
                  max = max(neighborhoodAvgSale$Yr),
                  value = 2018,
                  step = 1,
                  sep ="",
                  animate = animationOptions(interval=2000, loop=TRUE))
      
    })
    
    #provide year slider to ui
    output$valueYrSlider <- renderUI({
      changeAnimation()
    })
    
    #these selects allowed the user to select the property class and type,
    #refer to those csvs and script to generate them for desc source
    #output$PropertyTypeSelect <- renderUI({
    #  selectInput("TypeSelect", label=h3("Property Type"),
    #              choices=types,
    #              selected=types[1],
    #              selectize=FALSE)
    #})
    
    #output$PropertyClassSelect <- renderUI({
    #  selectInput("ClassSelect", label=h3("Property Class"),
    #              choices=classes,
    #              selected=classes[1],
    #              selectize=FALSE)
    #})
    
    #plot the mcpp based map of neighborhoods, colored by avg sales price 
    #during a given year
    output$ggplotMap <- renderPlot({
      
      #require that the input slider exist and its data be available here
      req(input$animation)
      
      #require selects
      #req(input$TypeSelect)
      #req(input$ClassSelect)
      
      #Provide a 3 year window
      YrAvgSale <- neighborhoodAvgSale %>% 
        filter(Yr > input$animation-1 & Yr < input$animation+1)
      
      
        #work on the filtering mechanism to allow the user to see
        # sales by property type and class (e.g. residential, e.g. improved )
        #if(input$TypeSelect=="All"){
        #  print("all")
        #  YrAvgSale <- YrAvgSale %>%
        #    group_by(name) %>% 
        #    summarize(AvgSlPr = mean(AvgSlPr))
          
        #}else{
        #  YrAvgSale <- YrAvgSale %>% 
        #    filter(HumanReadablePropertyType == input$TypeSelect)
        #}
      
        #if(input$ClassSelect=="All"){
        #  YrAvgSale <- YrAvgSale %>% 
        #    group_by(name, HumanReadablePropertyType) %>% 
        #    summarize(AvgSlPr = mean(AvgSlPr))
        #}else{
        #  YrAvgSale <- YrAvgSale %>% 
        #    filter(HumanReadablePropertyClass == input$ClassSelect)
        #}
          
        #join the sales data to the neighboorhood geometry
        YrAvgSale <- spd_mcpp_neighborhoods %>% 
          left_join(YrAvgSale, by=c("name"="name"))
        
        #plot neighborhood geometry, fill by avg sale price
        ggplot() +
          geom_sf(data=YrAvgSale, aes(fill=AvgSlPr))
          #geom_sf(data=spd_mcpp_neighborhoods)
    })
    
    
    output$test <- renderText({
        "Hello world!"
    })
}

## Histogram (we don't have enough data to plot this)
## all_plotable_data <- wa_sales_data %>%
##     get_prices_for_neighboorhoods(c('(Select All)')) %>%
##     filter(year_month == '2018-01')
## ggplot(all_plotable_data) + geom_histogram(aes(x=price))
