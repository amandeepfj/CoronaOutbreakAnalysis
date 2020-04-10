## ---------------------------
##
## Script name: Script to create model files
##
## Purpose of script:
##
## Author: Amandeep Jiddewar
##
## Date Created: 2020-03-15
##
## "We are drowning in information, while starving for wisdom - E. O. Wilson"
##
## ---------------------------

##----------------------------------------------------------------------------##
## Import the libraries ####
##----------------------------------------------------------------------------##

library(tidyverse)
library(lubridate)
library(prophet)
library(dygraphs)
library(curl)

##----------------------------------------------------------------------------##
## Pull WHO Data ####
##----------------------------------------------------------------------------##

WHO_DATA_URL <- "https://covid.ourworldindata.org/data/full_data.csv"

x <- read_csv(curl(WHO_DATA_URL))

who_df <- x %>% 
  select(date, location, total_cases, total_deaths) %>% 
  group_by(date, location) %>% 
  summarise_if(is.numeric, first) %>% ungroup() %>% 
  mutate(
    total_cases = ifelse(is.na(total_cases), 0, total_cases),
    total_deaths = ifelse(is.na(total_deaths), 0, total_deaths)) %>% 
  select(ds = date, 
         CountryOrRegion = location,
         Confirmed = total_cases,
         Deaths = total_deaths)

max(who_df$ds)

##----------------------------------------------------------------------------##
## Data.world dataset ####
##----------------------------------------------------------------------------##


api_token <- read_file("api_token_data_world")

dwapi::configure(auth_token = api_token)

covid_dataset <- dwapi::get_dataset("covid-19-data-resource-hub", "covid-19-case-counts")

sql_query <- "SELECT MAX(prep_flow_runtime)
              FROM covid_19_cases"

load_date <- dwapi::sql(covid_dataset$owner, covid_dataset$id, sql_query)$max

load_date <- as.Date(load_date)

sql_query <- str_glue("SELECT 
                      date as ds,
                      country_region as CountryOrRegion,
                      case_type,
                      sum(cases) as cases
                      FROM covid_19_cases 
                      WHERE prep_flow_runtime > '{load_date}'
                      GROUP BY date, country_region, case_type")

df_jhu <- dwapi::sql(covid_dataset$owner, covid_dataset$id, sql_query)

df_covid <- df_jhu %>% pivot_wider(
              names_from = case_type,
              values_from = cases
              )


##----------------------------------------------------------------------------##
## Modeling Functions ####
##----------------------------------------------------------------------------##

get_df_with_cap <- function(df_covid, country, forecast_variable){
  df <- df_covid %>% filter(CountryOrRegion == country) %>% arrange(ds) %>% 
    mutate(y = get(forecast_variable)) %>% 
    select(ds, y, Confirmed, Deaths)
  
  
  x <- ifelse(nrow(df) > 7, 7, nrow(df) - 1)
  today_count <- (df[df$ds == max(df$ds), ]$Confirmed)
  x_days_back_count <- df[df$ds >= max(df$ds) - x, ]$Confirmed %>% pluck(1)
  
  growth_rate <- (7/x) * (today_count / x_days_back_count)
  
  growth_rate <- ifelse(is.infinite(growth_rate), 1.5, growth_rate)
  
  cap <- as.integer(today_count * growth_rate)
  
  death_rate <- df[df$ds == max(df$ds), ]$y/cap
  
  cap <- ifelse(forecast_variable == "Deaths", cap * death_rate, cap)
  
  df$cap <- cap
  
  if(cap <= df[df$ds == max(df$ds), ]$y){
    if(country == "China"){
      df$cap <- cap * growth_rate
    }
    else{
      df$cap <- cap * 2.5
    }
  }
  
  if(country == "World"){
    df$cap <- as.integer(today_count * 1.1)
  }
  
  # Stan optimizer gives issues when too many 0's in y variable
  df <- df %>% mutate(y = ifelse(y == 0, 0.0001, y))
  
  # CAP should not be zero/floor value
  df <- df %>% mutate(cap = ifelse(cap == 0, 0.0001, cap))
  
  df
}

get_global_events <- function(country, forecast_variable){
  
  china_reporting_upper_window = 0
  
  if(country %in% c("China", "World") & forecast_variable == "Confirmed"){
    china_reporting_upper_window <- 500
  }
  
  china_reporting_changed <- data_frame(
    holiday = 'China reporting method change',
    ds = as.Date(c('2020-02-17')),
    lower_window = 0,
    upper_window = china_reporting_upper_window
  )
  
  
  pandemic_declared <- data_frame(
    holiday = 'WHO declares pandemic',
    ds = as.Date(c('2020-03-11')),
    lower_window = 0,
    upper_window = 0
  )
  
  italy_locked_down <- data_frame(
    holiday = 'Italy locked down',
    ds = as.Date(c('2020-03-09')),
    lower_window = 0,
    upper_window = 0
  )
  
  italy_locked_down <- data_frame(
    holiday = 'US declared emergency',
    ds = as.Date(c('2020-03-14')),
    lower_window = 0,
    upper_window = 0
  )
  
  events <- bind_rows(china_reporting_changed, pandemic_declared, italy_locked_down)
  events
}

get_forecast_model_who_data <- function(df_covid, country, forecast_variable){
  df <- get_df_with_cap(df_covid, country, forecast_variable)
  
  events <- get_global_events(country, forecast_variable)
  
  model <- prophet(df, growth = "logistic", 
                   holidays = events,
                   daily.seasonality = F, 
                   yearly.seasonality = F,
                   weekly.seasonality = F,
                   mcmc.samples = 0)
  model
}

get_forecast <- function(model, nDays = 7){
  
  future <- make_future_dataframe(model, periods = nDays, freq = "day")
  future$cap <- max(model$history$cap)
  fcst <- predict(model, future)
  fcst
  
}


##----------------------------------------------------------------------------##
## Pilot tests  ####
##----------------------------------------------------------------------------##

interested_in <- df_covid %>% group_by(CountryOrRegion) %>% 
  summarise(count = n()) %>% ungroup() %>% 
  filter(count > 4 & CountryOrRegion != "World") %>% pluck("CountryOrRegion")

lst_countries <- df_covid %>% filter(CountryOrRegion %in% interested_in) %>% 
  arrange(desc(Confirmed)) %>% distinct(CountryOrRegion)

for(i in 1:nrow(lst_countries)){
  
  country <- lst_countries[i, ] %>% pluck(1)
  
  print(paste0("Building model for ", country))
  
  confirmed_cases_model <- get_forecast_model_who_data(df_covid, country, "Confirmed")
  confirmed_cases_fcst <- get_forecast(confirmed_cases_model)
  
  deaths_cases_model <- get_forecast_model_who_data(df_covid, country, "Deaths")
  deaths_cases_fcst <- get_forecast(deaths_cases_model)
  
  country <- str_replace_all(country, "\\*", "")
  
  saveRDS(confirmed_cases_model, str_glue({"{here::here()}/R/Shiny App/models/Confirmed_{country}.rds"}))
  saveRDS(deaths_cases_model, str_glue({"{here::here()}/R/Shiny App/models/Deaths_{country}.rds"}))
  
  saveRDS(confirmed_cases_fcst, str_glue({"{here::here()}/R/Shiny App/models/Confirmed_{country}_forecast.rds"}))
  saveRDS(deaths_cases_fcst, str_glue({"{here::here()}/R/Shiny App/models/Deaths_{country}_forecast.rds"}))
  
}

saveRDS(lst_countries, str_glue({"{here::here()}/R/Shiny App/lst_countries.rds"}))

df_total <- df_covid %>% filter(ds == max(df_covid$ds)) %>% group_by(ds) %>% 
                summarise(Confirmed = sum(Confirmed),
                          Deaths = sum(Deaths))

saveRDS(df_total, str_glue({"{here::here()}/R/Shiny App/df_total.rds"}))

##----------------------------------------------------------------------------##
## Code sandbox section  ####
##----------------------------------------------------------------------------##

country <- "Italy"

forecast_variable <- "Confirmed"

model <- get_forecast_model_who_data(df_covid, country, forecast_variable)

fcst <- get_forecast(model)

model$history$y <- round(model$history$y, 2)
fcst$yhat <- round(fcst$yhat, 2)


plot(model, fcst, plot_cap = T)

dyplot.prophet(model, fcst, ylab = "Number of people") %>%
  dyOptions(drawPoints = TRUE, pointSize = 2) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 500)


##----------------------------------------------------------------------------##
## Deploy App to Shinyio ####
##----------------------------------------------------------------------------##

library(rsconnect)
deployApp(str_glue({"{here::here()}/R/Shiny App"}), 
          appName = "CoronaVirusForecast", forceUpdate = TRUE)



