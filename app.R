library(tidyverse)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)


# read data ---------------------------------------------------------------

# wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
# saveRDS(wine_ratings, "wine_ratings.RDS")
wine_ratings <- readRDS("wine_ratings.RDS")

# tmp file during development, loading all rows too slow
# saveRDS(wine_ratings[1:1000,], "wine_ratings1000.RDS")
# wine_ratings <- readRDS("wine_ratings1000.RDS")


# get values for categorical variables for "option" argument in pickerInput
options <- function(var){
  var <- enquo(var)
  wine_ratings %>% pull(!!var) %>% unique()
}
countries <- options(country)
vineyards <- options(designation)
provinces <- options(province)
regions <- options(region_1)
titles <- options(title)
varieties <- options(variety)
wineries <- options(winery)
prices <- options(price)




# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Wine rating explorer"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("1 - All wines", tabName = "all"),
      
      pickerInput(
        inputId = "country", 
        label = "Choose country", 
        choices = countries, 
        selected = countries,
        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3", liveSearch = T), 
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "region", 
        label = "Choose region", 
        choices = regions, 
        selected = regions,
        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3", liveSearch = T), 
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "variety", 
        label = "Choose variety", 
        choices = varieties, 
        selected = varieties,
        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3", liveSearch = T), 
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "winery", 
        label = "Choose winery", 
        choices = wineries, 
        selected = wineries,
        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3", liveSearch = T), 
        multiple = TRUE
      ),
      
      actionButton("plot", "Update plot"),

      
      hr(),
      
      menuItem("2 - Individual wine", tabName = "each"),
      
      pickerInput(
        inputId = "title", 
        label = "Choose title", 
        choices = titles, 
        selected = titles[1],
        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3", liveSearch = T), 
        multiple = FALSE
      )
      
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "all",
              fluidRow(
                box(width = 12,
                    column(width = 2,
                           numericInput(
                             inputId = "maxprice",
                             label = "Maximum price",
                             value = max(prices, na.rm = T),
                             min = min(prices, na.rm = T),
                             max = max(prices, na.rm = T),
                             step = 100
                           )),
                    column(width = 10,
                           sliderInput(
                             inputId = "pricerange",
                             label = "Price range",
                             min = min(prices, na.rm = T),
                             max = max(prices, na.rm = T),
                             value = c(min(prices, na.rm = T), max(prices, na.rm = T))
                           ))
                    ),
                box(width = 6, title = "Wine price and rating plot",
                    plotOutput("pricevrating1",
                               click = clickOpts("plot_hover1")),
                    uiOutput("hover_info1")),
                
                box(width = 6, title = "Wine price and rating plot (selected range)",
                    plotOutput("pricevrating2",
                               click = clickOpts("plot_hover2")),
                    uiOutput("hover_info2")
              )
                )
              ),
      
      tabItem(tabName = "each",
              uiOutput("titleUI"),
              br(),
              fluidRow(
                 valueBoxOutput("countryBox"),
                 valueBoxOutput("provinceBox"),
                 valueBoxOutput("regionBox")
              ),
              fluidRow(
                 valueBoxOutput("varietyBox"),
                 valueBoxOutput("wineryBox"),
                 valueBoxOutput("reviewerBox") 
              ),
              
              br(),
              
              fluidRow(
                box(width = 6,
                    plotOutput("hist1")),
                box(width = 6,
                    plotOutput("hist2"))
              )
      )

    )## end tabItems

  )
)





# server ------------------------------------------------------------------

server <- function(session, input, output) {
  
  ## update price range slider with maxprice
  ## hard to use slider with such long-tail prices
  observeEvent(input$maxprice,{updateSliderInput(session, inputId = "pricerange", label = "Price range", max = input$maxprice)})
  
  ## filtered data
  dat <- eventReactive(input$plot, {
           wine_ratings %>%
                filter(price <= input$maxprice,
                       country %in% input$country &
                       price >= input$pricerange[1] &
                       price <= input$pricerange[2] &
                       variety %in% input$variety &
                       region_1 %in% input$region &
                       winery %in% input$winery)
  })
  
  ## adj r squared for all data < maxprice in left plot
  rsq_all <- summary(lm(points ~ price, data = wine_ratings))$adj.r.squared
  
  ## left plot
  p1 <- eventReactive(input$plot, {
           ggplot(data = filter(wine_ratings, price <= input$maxprice), aes(x = price, y = points)) +
               geom_point() +
               ylim(75, 100) +
               geom_smooth(method = "lm", se = F) +
               labs(subtitle = paste0("How much variation in rating is explained by price: ", round(rsq_all, 2)), y = "rating") +
               theme_light() + 
               if(input$pricerange[1] != min(prices, na.rm = T) | input$pricerange[2] != input$maxprice) annotate("rect", xmin = input$pricerange[1], xmax = input$pricerange[2], ymin = 75, ymax = 100, alpha = 0.2)
  })
  
  ## right plot
  p2 <- eventReactive(input$plot, {
    validate(
      need(try(nrow(dat())!=0), paste0("No data available in selected categories."))
    )
    
    rsq_sub <- summary(lm(points ~ price, data = dat()))$adj.r.squared
    
           ggplot(data = dat(), aes(x = price, y = points)) +
              geom_point() +
              ylim(75, 100) +
              geom_smooth(method = "lm", se = F) +
              theme_light() +
              labs(subtitle = paste0("How much variation in rating is explained by price: ", round(rsq_sub, 2)), y = "rating")
  })

  output$pricevrating1 <- renderPlot(p1())
  output$pricevrating2 <- renderPlot(p2())
  
  
  ## hover info
  output$hover_info1 <- renderUI({
    hover <- input$plot_hover1
    point <- nearPoints(wine_ratings, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    
    left_px <- if(left_pct <= 0.5){
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }else if(left_pct > 0.5){
      hover$range$right - left_pct * (hover$range$right - hover$range$left) 
    }
    
    top_px <- if(top_pct <= 0.5){
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    }else if(top_pct > 0.5){
      hover$range$bottom - top_pct * (hover$range$bottom - hover$range$top) 
    }
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Title: </b>", point$title, "<br/>",
                    "<b> Price: </b>", point$price, "<br/>",
                    "<b> Rating: </b>", point$points,  "<br/>",
                    "<b> Variety: </b>", point$variety,"<br/>",
                    "<b> Country: </b>", point$country, "<br/>", 
                    "<b> Region: </b>", point$region_1, "<br/>", 
                    "<b> Winery: </b>", point$winery)))
    )
  })

  ## need to write into a function...
  output$hover_info2 <- renderUI({
    hover <- input$plot_hover2
    point <- nearPoints(dat(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    left_px <- if(left_pct <= 0.5){
                  hover$range$left + left_pct * (hover$range$right - hover$range$left)
               }else if(left_pct > 0.5){
                  hover$range$right - left_pct * (hover$range$right - hover$range$left) 
               }
    
    top_px <- if(top_pct <= 0.5){
                  hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
              }else if(top_pct > 0.5){
                  hover$range$bottom - top_pct * (hover$range$bottom - hover$range$top) 
              }
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Title: </b>", point$title, "<br/>",
                    "<b> Price: </b>", point$price, "<br/>",
                    "<b> Rating: </b>", point$points,  "<br/>",
                    "<b> Variety: </b>", point$variety,"<br/>",
                    "<b> Country: </b>", point$country, "<br/>", 
                    "<b> Region: </b>", point$region_1, "<br/>", 
                    "<b> Winery: </b>", point$winery)))
    )
  })
  
  

# individual wine tab -----------------------------------------------------

  output$titleUI <- renderUI(titlePanel(input$title))
  
  ## selected single wine
  selected <- reactive(wine_ratings %>% filter(title == input$title))
  
  ## all wine in the same variety as the selected single wine
  variety_selected <- reactive(wine_ratings %>% filter(variety == selected()$variety))
  
  
  ## value boxes
  output$countryBox <- renderValueBox(valueBox(selected()$country, "Country", icon = icon("list"), color = "purple"))
  output$provinceBox <- renderValueBox(valueBox(selected()$province, "Province", icon = icon("list"), color = "aqua"))
  output$regionBox <- renderValueBox(valueBox(selected()$region_1, "Region", icon = icon("list"), color = "green"))
  
  output$varietyBox <- renderValueBox(valueBox(selected()$variety, "Variety", icon = icon("list"), color = "blue"))
  output$wineryBox <- renderValueBox(valueBox(selected()$winery, "Winery", icon = icon("list"), color = "maroon"))
  output$reviewerBox <- renderValueBox(valueBox(selected()$taster_name, "Reviewer", icon = icon("list"), color = "orange"))
  
  
  ## histogram of prices and points of wines in the same variety as the selected wine
  output$hist1 <- renderPlot({
    
    quantile_price <- ecdf(variety_selected()$price)(selected()$price)
    
    ggplot(data = variety_selected(), aes(x = price)) +
      geom_histogram(fill = "white", color = "black") +
      theme_light() +
      geom_vline(xintercept = selected()$price, color = "red", size = 2) +
      labs(x = "price", title = "Price of selected wine compared to same variety", 
           subtitle = paste0("Quantile = ", round(quantile_price, 2))) +
      theme(plot.title=element_text(hjust=0.5))
  })
  
  output$hist2 <- renderPlot({
    
    quantile_points <- ecdf(variety_selected()$points)(selected()$points)
    
    ggplot(data = variety_selected(), aes(x = points)) +
      geom_histogram(fill = "white", color = "black") +
      theme_light() +
      geom_vline(xintercept = selected()$points, color = "green", size = 2) +
      labs(x = "rating", title = "Rating of selected wine compared to same variety", 
           subtitle = paste0("Quantile = ", round(quantile_points, 2))) +
      theme(plot.title=element_text(hjust=0.5))
  })
  
}

shinyApp(ui, server)



