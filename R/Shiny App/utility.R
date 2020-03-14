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
                in <strong style = 'color: #107896;'>{country}</strong>. The rate of increase is 
                <strong style = 'color: #107896;'>{get_rate_of_change(model)}</strong>.
                The cap set when the model was trained is 
                <strong style = 'color: #107896;'>{get_cap(model)}</strong>.
                "})
}