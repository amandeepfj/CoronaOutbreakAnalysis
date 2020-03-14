## ---------------------------
##
## Script name: Corona Outbreak Shiny App
##
## Purpose of script:
##
## Author: Amandeep Jiddewar
##
## Date Created: 2020-03-14
##
## "We are drowning in information, while starving for wisdom - E. O. Wilson"
##
## ---------------------------


library(tidyverse)
library(prophet)
library(shiny)
library(dygraphs)
library(shinycssloaders)
library(shinydashboard)
library(DT)

source("model_helper.R")

lst_countries <- readRDS("lst_countries.rds") #%>% head(2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "COVID-19 Forecast",
    titlePanel("Select Country"),
    selectInput("country", label = NULL,
                choices = lst_countries, selected = "Iran"),
    hr(),
    fluidRow(
        column(
            width = 4,
            h2("Confirmed Cases"),
            br(),
            dataTableOutput("dt_predictions")
        ),
        column(
            width = 8,
            br(),
            br(),
            br(),
            uiOutput("commentary_confirm"),
            br(),
            br(),
            dygraphOutput("forecasts") %>% withSpinner(),
            #plotOutput("forecast_components") %>% withSpinner(),
        ),
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    get_reactive_model <- reactive({
        readRDS(str_glue({"models/{input$country}.rds"}))
    })
    
    get_reactive_forecast <- reactive({
        readRDS(str_glue({"models/{input$country}_forecast.rds"}))
    })
    
    output$commentary_confirm <- renderUI({
        h4(str_glue({"
                The below graph shows the trend and next 7 days forecast of confirmed cases of the corona outbreak 
                in {input$country}. The rate of increase is {get_rate_of_change(get_reactive_model())}.
                The cap set when the model was trained is {get_cap(get_reactive_model())}
                "}))
    })
    
    output$dt_predictions <- renderDataTable({
        DT::datatable(get_df_actual_vs_predicted(get_reactive_model(), get_reactive_forecast()),
                      rownames = F, options = list(searching = FALSE, pageLength = 7, 
                                                   lengthMenu = c(7, 10, 15, 20), scrollX = T))
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
