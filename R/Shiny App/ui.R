## ---------------------------
##
## Script name: UI File
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
source("utility.R")

lst_countries <- get_list_of_countries()
total_world_cases <- get_total_world_cases()
total_world_deaths <- get_total_world_deaths()
date_of_cases <- get_date_of_cases()


# Define UI for application that draws a histogram
fluidPage(
  
  tags$head(includeHTML(("google-analytics.html"))),
  
  title = "COVID-19 Cases",
  
  titlePanel("COVID 19 Cases"),
  h3("Trend and short term prediction model"),
  
  HTML(
  str_glue({"The document shows country-wise confirmed, deaths, and recovered cases. 
  The model <a href='https://facebook.github.io/prophet/docs/quick_start.html#r-api' target='_blank'>
  forecasts</a> 
  the next seven days of expected cases. 
  Facebook's prophet is used to create the models below. The rate of growth/change is the 
  second-highest rate of change in the window of the last five days.  There are many variables, 
  such as travel bans, lockdowns,  quarantines, etc. This is a univariate forecasting model. 
  I have only considered the previously reported cases. The model is created for my learning 
  and should not be taken as an authentic source. It does a pretty good job of predicting the 
  next 3-4 days but fails when it comes to looking far ahead in the future. 
  Here is a GitHub code for the reference. 
  <a href='https://github.com/amandeepfj/CoronaOutbreakAnalysis/tree/master/R' target='_blank'>
    Code</a>. <span style='font-size:150%;font-family:cursive;color: green;'>
    Take the necessary precautions and stay safe everyone.</span>"})),
  
  br(),br(),
  
 
  HTML(
    str_glue({"<b>As of {date_of_cases}. The total number of confirmed cases in the world are 
      {prettyNum(total_world_cases , big.mark=',', scientific=FALSE)}, and total number of deaths 
      in the world are {prettyNum(total_world_deaths , big.mark=',', scientific=FALSE)}. </b>"})),
  
  br(),br(),
  
  selectInput("country", label = "Select country",
              choices = lst_countries, selected = "US"),
  
  
  hr(),
  fluidRow(
    column(
      width = 4,
      h2("Total confirmed cases"),
      br(),
      dataTableOutput("dt_predictions_confirmed")
    ),
    column(
      width = 8,
      
      br(),br(),br(),
      
      htmlOutput("commentary_confirmed"),
      
      br(),br(),
      
      dygraphOutput("forecasts_confirmed") %>% withSpinner(),
    ),
  ),
  
  
  
  hr(),
  fluidRow(
    column(
      width = 4,
      h2("Total deaths"),
      br(),
      dataTableOutput("dt_predictions_deaths")
    ),
    column(
      width = 8,
      
      br(),br(),br(),
      
      htmlOutput("commentary_deaths"),
      
      br(),br(),
      dygraphOutput("forecasts_deaths") %>% withSpinner(),
      #plotOutput("forecast_components") %>% withSpinner(),
    ),
  ),
  
  
  # hr(),
  # fluidRow(
  #   column(
  #     width = 4,
  #     h2("Recovered Cases"),
  #     br(),
  #     dataTableOutput("dt_predictions_recovered")
  #   ),
  #   column(
  #     width = 8,
  #     
  #     br(),br(),br(),
  #     
  #     htmlOutput("commentary_recovered"),
  #     
  #     br(),br(),
  #     dygraphOutput("forecasts_recovered") %>% withSpinner(),
  #   ),
  # ),
  
  hr(),
  HTML("<ul><b>Data sources</b>
          <li> <a>https://covid.ourworldindata.org/data/full_data.csv </a>
        </ul>"),
  
  hr(),
  includeHTML("footer.html"),
  br(),
)