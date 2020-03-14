## ---------------------------
##
## Script name: 
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

server <- function(input, output) {
  
  ##----------------------------------------------------------------------------##
  ## Reactives ####
  ##----------------------------------------------------------------------------##
  get_confirmed_cases_models <- reactive({
    readRDS(str_glue({"models/Confirmed_{input$country}.rds"}))
  })
  
  get_deaths_cases_models <- reactive({
    readRDS(str_glue({"models/Deaths_{input$country}.rds"}))
  })
  
  get_recovered_cases_models <- reactive({
    readRDS(str_glue({"models/Recovered_{input$country}.rds"}))
  })
  
  get_confirmed_cases_forecast <- reactive({
    readRDS(str_glue({"models/Confirmed_{input$country}_forecast.rds"}))
  })
  
  get_deaths_cases_forecast <- reactive({
    readRDS(str_glue({"models/Deaths_{input$country}_forecast.rds"}))
  })
  
  get_recovered_cases_forecast <- reactive({
    readRDS(str_glue({"models/Recovered_{input$country}_forecast.rds"}))
  })
  
  ##----------------------------------------------------------------------------##
  ## Confirmed cases section ####
  ##----------------------------------------------------------------------------##
  
  output$commentary_confirmed <- renderText({
    get_commentary_html(get_confirmed_cases_models(), input$country)
  })
  
  output$dt_predictions_confirmed <- renderDataTable({
    
    df <- get_df_actual_vs_predicted(get_confirmed_cases_models(),
                                     get_confirmed_cases_forecast())
    get_forecasts_table_html(df)
  
  })
  
  # output$forecast_components <- renderPlot({
  #     prophet_plot_components(get_reactive_model(), get_reactive_forecast()) 
  # })
  
  output$forecasts_confirmed <- renderDygraph({
    dyplot.prophet(get_confirmed_cases_models(),
                   get_confirmed_cases_forecast())
  })
  
  ##----------------------------------------------------------------------------##
  ## Deaths cases section ####
  ##----------------------------------------------------------------------------##
  
  output$commentary_deaths <- renderText({
    get_commentary_html(get_deaths_cases_models(), input$country)
  })
  
  output$dt_predictions_deaths <- renderDataTable({
    
    df <- get_df_actual_vs_predicted(get_deaths_cases_models(),
                                     get_deaths_cases_forecast())
    get_forecasts_table_html(df)
    
  })
  
  output$forecasts_deaths <- renderDygraph({
    dyplot.prophet(get_deaths_cases_models(),
                   get_deaths_cases_forecast())
  })
  
  ##----------------------------------------------------------------------------##
  ## Recovered cases section ####
  ##----------------------------------------------------------------------------##
  
  output$commentary_recovered <- renderText({
    get_commentary_html(get_recovered_cases_models(), input$country)
  })
  
  output$dt_predictions_recovered <- renderDataTable({
    
    df <- get_df_actual_vs_predicted(get_recovered_cases_models(),
                                     get_recovered_cases_forecast())
    get_forecasts_table_html(df)
    
  })
  
  output$forecasts_recovered <- renderDygraph({
    dyplot.prophet(get_recovered_cases_models(),
                   get_recovered_cases_forecast())
  })
}

