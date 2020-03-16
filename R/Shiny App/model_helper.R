## ---------------------------
##
## Script name: Script to parse model object and return required items 
##
## Purpose of script:
##
## Author: Amandeep Jiddewar
##
## Date Created: 2020-03-13
##
## "We are drowning in information, while starving for wisdom - E. O. Wilson"
##
## ---------------------------

get_rate_of_change <- function(model){
  round(model$history$cap_scaled %>% pluck(1), digits = 2)
}


get_cap <- function(model){
  round(first(model$history$cap), digits = 2)
}


get_df_actual_vs_predicted <- function(model, forecast){
  as_tibble(
    forecast %>% left_join(model$history, by = "ds") %>% 
    select(Date = ds, Actual = y, Predicted = yhat, 
           Predicted_lower = yhat_lower, Predicted_upper = yhat_upper) %>% 
      mutate(Date = as.Date(Date)) %>% mutate_if(is.numeric, round) %>% 
      arrange(desc(Date))
    )
}


get_uncertainity_interval <- function(model){
  str_glue({ "
  {model$interval.width*100}%
  "})
}

