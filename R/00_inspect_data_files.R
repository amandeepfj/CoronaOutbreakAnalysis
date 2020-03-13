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
## ---------------------------

##----------------------------------------------------------------------------##
## Import the libraries ####
##----------------------------------------------------------------------------##

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  prophet)


##----------------------------------------------------------------------------##
## Read Data ####
##----------------------------------------------------------------------------##

df_corona_raw <- read_csv("input/novel-corona-virus-2019-dataset/covid_19_data.csv")

df_corona_raw <- df_corona_raw %>% rename(ProvinceOrState = `Province/State`,
                            CountryOrRegion = `Country/Region`,
                            LatestUpdate = `Last Update`)

df_corona <- df_corona_raw %>% mutate(Confirmed = as.integer(Confirmed),
                            Deaths = as.integer(Deaths),
                            Recovered = as.integer(Recovered),
                            ObservationDate = parse_date_time(ObservationDate, orders = c("m/d/Y", "m/d/y"), locale = "eng"),
                            #LatestUpdate = parse_date_time(LatestUpdate, orders = c("m/d/Y", "m/d/y"), locale = "eng")
                            )

df_corona <- df_corona %>% select(-c(LatestUpdate, SNo))

corona_latest <- df_corona %>% filter(ObservationDate %in% max(corona$ObservationDate))

sum(corona_latest$Confirmed)
sum(corona_latest$Deaths)
sum(corona_latest$Recovered)

View(df_corona %>% arrange(ObservationDate) %>% 
       group_by(ObservationDate, CountryOrRegion) %>% summarise(Confirmed = sum(Confirmed)) %>% ungroup() %>% 
       mutate(new_confirmed_today = c(first(Confirmed), diff(Confirmed))))

vec <- cumsum(1:10)
c(vec[1],diff(vec))

