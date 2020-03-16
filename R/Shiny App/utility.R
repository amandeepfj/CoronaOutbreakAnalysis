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

get_list_of_countries <- function(){
  readRDS("lst_countries.rds")
}


get_forecasts_table_html <- function(df){
  DT::datatable(df, rownames = F, options = list(searching = FALSE, pageLength = 7, 
                                             lengthMenu = c(7, 10, 15, 20), scrollX = T))  
}

get_commentary_html <- function(model, country){
  str_glue({"
                The below graph shows the trend and 
                <strong style = 'color: #107896;'> next 7 days </strong>
                forecast of cases of the corona outbreak 
                in <strong style = 'color: #107896;'>{country}</strong>. The rate of increase, every 
                seven days, when the model was created is 
                <strong style = 'color: #107896;'>{get_rate_of_change(model)}</strong> times.
                The cap set when the model was trained is 
                <strong style = 'color: #107896;'>{get_cap(model)}</strong>. 
                The <strong>black dots</strong> are actual numbers, 
                <strong style = 'color: blue;'>blue dots</strong> are predicted numbers. 
                The uncertainity interval is set to {get_uncertainity_interval(model)}.
                "})
}


get_dygraph_to_plot <- function(model, fcst){
  
  model$history$y <- round(model$history$y, 2)
  fcst$yhat <- round(fcst$yhat, 2)
  
  dyplot.prophet(model, fcst, ylab = "Number of people") %>% 
      dyOptions(drawPoints = TRUE, pointSize = 2) %>%  
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 400)

}
