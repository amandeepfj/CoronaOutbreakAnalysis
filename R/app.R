#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(prophet)
library(shiny)
library(dygraphs)
library(shinycssloaders)

lst_countries <- lst_countries <- readRDS("lst_countries.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Select Country to forecast"),

    # Sidebar with a slider input for number of bins 
    inputPanel(
        selectInput("country", label = "Number of bins:",
                    choices = lst_countries, selected = "India")
    ),
    dygraphOutput("forecasts") %>% withSpinner(),
    plotOutput("forecast_components") %>% withSpinner(),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    get_reactive_model <- reactive({
        readRDS(str_glue({"models/{input$country}.rds"}))
    })
    
    get_reactive_forecast <- reactive({
        readRDS(str_glue({"models/{input$country}_forecast.rds"}))
    })
    
    output$forecast_components <- renderPlot({
        prophet_plot_components(get_reactive_model(), get_reactive_forecast()) 
    })
    
    output$forecasts <- renderDygraph({
        dyplot.prophet(get_reactive_model(), get_reactive_forecast())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
