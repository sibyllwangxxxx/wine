library(tidyverse)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)

# wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
# saveRDS(wine_ratings, "wine_ratings.RDS")
wine_ratings <- readRDS("wine_ratings.RDS")

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

ui <- dashboardPage(
  dashboardHeader(title = "Wine rating explorer"),
  dashboardSidebar(
    sidebarMenu(
      id="menu", ## https://stackoverflow.com/questions/29925585/conditional-panel-in-shiny-dashboard
      
      menuItem("All wines", tabName = "all"),
      
      uiOutput("all_widgets"),
      
      hr(),
      
      menuItem("Individual wine", tabName = "each")
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "all",
              fluidRow(
                box(width = 6, title = "Wine price and rating plot",
                    plotOutput("pricevrating1")),
                box(width = 6, title = "Wine price and rating plot (selected range)",
                    plotOutput("pricevrating2"))
                )
              ),
      
      tabItem(tabName = "each")
      
    )## end tabItems

  )
)

server <- function(input, output) {
  
  output$all_widgets <- renderUI({
    
    if(input$menu == "all")
    
    tagList(
      pickerInput(
        inputId = "country", 
        label = "Choose country", 
       choices = countries, 
        selected = countries,
        options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
        multiple = TRUE
      ),
    
      sliderInput(
        inputId = "pricerange",
        label = "Price range",
        min = min(prices, na.rm = T),
        max = max(prices, na.rm = T),
        value = c(min(prices, na.rm = T), max(prices, na.rm = T))
      ),
    
      actionButton("plot", "Update plot")
    )
  })
  
  
  dat <- eventReactive(input$plot, {
           wine_ratings %>%
                filter(country %in% input$country &
                       price >= input$pricerange[1] &
                       price <= input$pricerange[2])
  })
  
  p1 <- eventReactive(input$plot, {
           ggplot(data = wine_ratings, aes(x = price, y = points)) +
               geom_point() +
               ylim(75, 100) +
               geom_smooth(method = "lm") +
               theme_light() + 
               if(input$pricerange[1] != min(prices, na.rm = T) | input$pricerange[2] != max(prices, na.rm = T)) annotate("rect", xmin = input$pricerange[1], xmax = input$pricerange[2], ymin = 75, ymax = 100, alpha = 0.2)
  })
  
  p2 <- reactive({
           ggplot(data = dat(), aes(x = price, y = points)) +
              geom_point() +
              ylim(75, 100) +
              geom_smooth(method = "lm") +
              theme_light()
  })

  output$pricevrating1 <- renderPlot(p1())
  output$pricevrating2 <- renderPlot(p2())
  
}

shinyApp(ui, server)



