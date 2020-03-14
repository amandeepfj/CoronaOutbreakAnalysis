## ---------------------------
##
## Script name: Inspect data files 
##
## Purpose of script: Check the data files we have
##
## Author: Amandeep Jiddewar
##
## Date Created: 2020-03-12
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
library(wbstats)
library(shiny)
library(dygraphs)
library(shinycssloaders)


##----------------------------------------------------------------------------##
## Read Data ####
##----------------------------------------------------------------------------##

df_corona_raw <- read_csv("input/novel-corona-virus-2019-dataset/covid_19_data.csv")

##----------------------------------------------------------------------------##
## Data preparation ####
##----------------------------------------------------------------------------##

df_corona_raw <- df_corona_raw %>% rename(ProvinceOrState = `Province/State`,
                            CountryOrRegion = `Country/Region`,
                            LatestUpdate = `Last Update`)

df_corona <- df_corona_raw %>% select(-c(LatestUpdate, SNo))

df_corona <- df_corona %>% mutate(Confirmed = as.integer(Confirmed),
                            Deaths = as.integer(Deaths),
                            Recovered = as.integer(Recovered),
                            ObservationDate = parse_date_time(ObservationDate, orders = c("m/d/Y", "m/d/y")),
                            #LatestUpdate = parse_date_time(LatestUpdate, orders = c("m/d/Y", "m/d/y"), locale = "eng")
                            )

df_corona <- df_corona %>% mutate(ObservationDate = as.Date(ObservationDate))

##----------------------------------------------------------------------------##
## Append new data from GitHub ####
##----------------------------------------------------------------------------##

library(curl)

URL_PRE <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

fetch_dates <- seq(max(df_corona$ObservationDate) + 1, today(),by = 1)
fetch_dates <- format(fetch_dates,"%m-%d-%Y")

df_corona_raw_to_append <- NULL

for(temp_date in fetch_dates){
  print(str_glue("Fetching data for {temp_date}...."))
  x <- read_csv(curl(str_glue({"{URL_PRE}{temp_date}.csv"})))
  
  print(str_glue({"Fetch complete for - Date : {temp_date}, Rows : {nrow(x)}"}))
  
  x <- x %>% mutate(ObservationDate = as.Date(`Last Update`),
                    CountryOrRegion = `Country/Region`,
                    ProvinceOrState = `Province/State`) %>% 
    
    select(ObservationDate, ProvinceOrState, CountryOrRegion,
           Confirmed, Deaths, Recovered)
  
  if(is.null(df_corona_raw_to_append)){
    df_corona_raw_to_append <- x
  }
  else{
    df_corona_raw_to_append <- rbind(df_corona_raw_to_append, x)
  }
}

df_corona <- rbind(df_corona, df_corona_raw_to_append)

##----------------------------------------------------------------------------##
## Data Cleaning after updating the dataframe with new data ####
##----------------------------------------------------------------------------##

df_corona <- df_corona %>% mutate(YesterDate = ObservationDate - 1)

df_corona <- df_corona %>% left_join(df_corona, by = c("YesterDate" = "ObservationDate", "ProvinceOrState", "CountryOrRegion"),
                        suffix = c("", "TillYesterDate")) %>% select(-YesterDateTillYesterDate)

df_corona <- df_corona %>% mutate(ConfirmedTillYesterDate = ifelse(is.na(ConfirmedTillYesterDate), Confirmed, ConfirmedTillYesterDate),
                     RecoveredTillYesterDate = ifelse(is.na(RecoveredTillYesterDate), Recovered, RecoveredTillYesterDate),
                     DeathsTillYesterDate = ifelse(is.na(DeathsTillYesterDate), Deaths, DeathsTillYesterDate))

df_corona <- df_corona %>% mutate(NewTodayConfirmed = Confirmed - ConfirmedTillYesterDate,
                                  NewTodayRecovered = Recovered - RecoveredTillYesterDate,
                                  NewTodayDeaths    = Deaths    - DeathsTillYesterDate)


##----------------------------------------------------------------------------##
## Countries that did not match from Github fetched data ####
##----------------------------------------------------------------------------##

df_corona_raw[!(df_corona_raw$CountryOrRegion %in% df_corona_raw_to_append$CountryOrRegion), ] %>% 
  distinct(CountryOrRegion) %>% pluck(1)

df_corona_raw_to_append[!(df_corona_raw_to_append$CountryOrRegion %in% df_corona_raw$CountryOrRegion), ] %>% 
  distinct(CountryOrRegion) %>% pluck(1)

##----------------------------------------------------------------------------##
## Merge Data for different name but same country ####
##----------------------------------------------------------------------------##

same_names <- c("South Korea", "Korea, South")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("Mainland China", "China")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("Taiwan", "Taiwan*")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("UK", "United Kingdom")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("Czech Republic", "Czechia")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("Ivory Coast", "Cote d'Ivoire")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("French Guiana", "Guinea")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

same_names <- c("St. Martin", "('St. Martin',)")
df_corona[df_corona$CountryOrRegion %in% same_names, "CountryOrRegion"] <- same_names[1]

detect <- "Diamond Princess"
df_corona <- df_corona %>% 
              mutate(CountryOrRegion = ifelse(!is.na(ProvinceOrState) & str_detect(ProvinceOrState, detect), detect, CountryOrRegion))

##----------------------------------------------------------------------------##
## Prophet Model ####
##----------------------------------------------------------------------------##

get_forecast_model <- function(country = "India", df_corona, forecast_variable = "Confirmed"){
  
  #country <- "Norway"
  #forecast_variable <- "Deaths"
  print(paste0("Creating ", forecast_variable," cases model for ", country, "..."))
  last_y_var <- paste0(forecast_variable, "TillYesterDate")
  
  MIN_RECORDS_NEEDED <- 7
  LAST_N_TO_CONSIDER <- MIN_RECORDS_NEEDED
 # DEFAULT_RATE <- 1.5
  
  df <- df_corona %>% 
    filter(str_detect(CountryOrRegion, country)) %>% 
    group_by(ObservationDate, ProvinceOrState) %>% summarise_if(is.numeric, first) %>% ungroup() %>% 
    group_by(ObservationDate) %>% summarise_if(is.numeric, sum) %>% 
    select(ds = ObservationDate,
           y = forecast_variable,
           last_y = last_y_var)
  
  if(nrow(df) > MIN_RECORDS_NEEDED){
    
    today_count <- max(df$y)
    
    # Avoid divide by zero issue
    df <- df %>% mutate(rate_of_change = y/(last_y + 0.0000001)) %>% arrange(ds)
    
    nth_highest <- 1
    
    rate_of_change <- df %>% 
                        tail(LAST_N_TO_CONSIDER) %>% 
                        pluck("rate_of_change") %>% 
                        sort(partial = length(.) - nth_highest) %>% 
                        .[length(.) - nth_highest]
    
    
    #rate_of_change <- ifelse(rate_of_change <= 1, DEFAULT_RATE, rate_of_change) 
    
    cap <- as.integer(today_count * rate_of_change)
    df$cap <- ifelse(cap > 0, cap, 1)
    
    m <- prophet(df, growth = "logistic", 
                daily.seasonality = F, 
                yearly.seasonality = F,
                weekly.seasonality = T)
    m
  }
  else{
    NULL
  }
}


get_forecast <- function(model, nDays = 7){
  
  future <- make_future_dataframe(model, periods = nDays, freq = "day")
  future$cap <- max(model$history$cap)
  fcst <- predict(model, future)
  fcst
  
}


lst_countries <- df_corona %>% arrange(desc(Confirmed)) %>% distinct(CountryOrRegion) %>% head(-20)

countries_with_less_records <- c()
  
for(i in 1:nrow(lst_countries)){
  country_name <- lst_countries[i, ] %>% pluck(1)
  confirmed_cases_model <- get_forecast_model(country_name, df_corona)
  if(!is.null(confirmed_cases_model)){
    saveRDS(confirmed_cases_model, str_glue({"Shiny App/models/Confirmed_{country_name}.rds"}))
    
    fcst <- get_forecast(confirmed_cases_model)
    saveRDS(fcst, str_glue({"Shiny App/models/Confirmed_{country_name}_forecast.rds"}))
    
    
    deaths_cases_model <- get_forecast_model(country_name, df_corona, "Deaths")
    saveRDS(deaths_cases_model, str_glue({"Shiny App/models/Deaths_{country_name}.rds"}))
    fcst <- get_forecast(deaths_cases_model)
    saveRDS(fcst, str_glue({"Shiny App/models/Deaths_{country_name}_forecast.rds"}))
    
    recovered_cases_model <- get_forecast_model(country_name, df_corona, "Recovered")
    saveRDS(recovered_cases_model, str_glue({"Shiny App/models/Recovered_{country_name}.rds"}))
    fcst <- get_forecast(recovered_cases_model)
    saveRDS(fcst, str_glue({"Shiny App/models/Recovered_{country_name}_forecast.rds"}))
    
  }
  else{
    countries_with_less_records <- c(countries_with_less_records, country_name)
  }
}


lst_countries <- lst_countries %>% filter(!CountryOrRegion %in% countries_with_less_records)

saveRDS(lst_countries, "Shiny App/lst_countries.rds")

source("Shiny App/model_helper.R")

test_country_name <- lst_countries %>% pluck(1, 1)

type <- "Deaths"
# 
 m2 <- readRDS(str_glue({"Shiny App/models/{type}_{test_country_name}.rds"}))
# 
 fcst <- readRDS(str_glue({"Shiny App/models/{type}_{test_country_name}_forecast.rds"}))
 
# get_df_actual_vs_predicted(m2, fcst)
# 
# #get_population_of_country("CHN", df_corona)
# 
 plot(m2, fcst, plot_cap = T)
 dyplot.prophet(m2, fcst, uncertainty = T) 
# 
# #prophet_plot_components(m, fcst)
# 



##----------------------------------------------------------------------------##
## Country-Wise Population Data ####
##----------------------------------------------------------------------------##

get_updated_with_population <- function(df_corona){
  
  countries <- df_corona %>% distinct(CountryOrRegion)
  
  countries <- countries %>% mutate(corona_df_name = CountryOrRegion, 
                                    wb_df_name = CountryOrRegion)
  
  world_population <- wb(indicator = "SP.POP.TOTL", startdate = 2017, enddate = 2017) %>% 
    select(iso3c, country, population = value)
  
  df_population <- countries %>% left_join(world_population, 
                                           by = c("wb_df_name" = "country"))
  
  df_population[df_population$CountryOrRegion == "Mainland China", "iso3c"] = "CHN"
  df_population[df_population$CountryOrRegion == "Hong Kong", "iso3c"] = "HKG"
  df_population[df_population$CountryOrRegion == "Macau", "iso3c"] = "MAC"
  df_population[df_population$CountryOrRegion == "US", "iso3c"] = "USA"
  df_population[df_population$CountryOrRegion == "South Korea", "iso3c"] = "KOR"
  df_population[df_population$CountryOrRegion == "Ivory Coast", "iso3c"] = "CIV"
  df_population[df_population$CountryOrRegion == "UK", "iso3c"] = "GBR"
  df_population[df_population$CountryOrRegion == "Egypt", "iso3c"] = "EGY"
  df_population[df_population$CountryOrRegion == "Iran", "iso3c"] = "IRN"
  df_population[df_population$CountryOrRegion == "Saint Barthelemy", "iso3c"] = "BLM"
  df_population[df_population$CountryOrRegion == "Palestine", "iso3c"] = "PSE"
  df_population[df_population$CountryOrRegion == "Russia", "iso3c"] = "RUS"
  df_population[df_population$CountryOrRegion == "Slovakia", "iso3c"] = "SVK"
  df_population[df_population$CountryOrRegion == "French Guiana", "iso3c"] = "GUF"
  df_population[df_population$CountryOrRegion == "Martinique", "iso3c"] = "MTQ"
  df_population[df_population$CountryOrRegion == "Republic of Ireland", "iso3c"] = "IRL"
  df_population[df_population$CountryOrRegion == "St. Martin", "iso3c"] = "MAF"
  df_population[df_population$CountryOrRegion == "Brunei", "iso3c"] = "BRN"
  df_population[df_population$CountryOrRegion == "Holy See", "iso3c"] = "VAT"
  df_population[df_population$CountryOrRegion == "Reunion", "iso3c"] = "REU"
  df_population[df_population$CountryOrRegion == "Slovakia", "iso3c"] = "SVK"
  df_population[df_population$CountryOrRegion == "Congo (Kinshasa)", "iso3c"] = "COD"
  df_population[df_population$CountryOrRegion == "Reunion", "iso3c"] = "REU"
  
  # Countries which were not classified correctly
  #df_population %>% filter(is.na(iso3c))
  
  df_population <- df_population %>% left_join(world_population, 
                                               by = c("iso3c")) %>% 
    select(CountryOrRegion, iso3c, population = population.y)
  
  df_corona <- df_corona %>% left_join(df_population, by = c("CountryOrRegion"))
  df_corona
}


get_population_of_country <- function(country, df_corona){
  population <- df_corona %>% group_by(iso3c) %>% 
    summarise(population = first(population)) %>% 
    filter(str_detect(iso3c, country)) %>% 
    .$population %>% sum(na.rm = T)
  population
}

#get_population_of_country("CHN", df_corona)
