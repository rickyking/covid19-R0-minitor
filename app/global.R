library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(incidence)
#library(ggiraph)
library(echarts4r)
library(earlyR)
library(epitrix)
library(shinyjs)
library(shinyWidgets)
library(projections)
library(EpiEstim)

# get data
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_confirmed_global.csv", sep = "")

confirmed_long_jhu <- read_csv(jhu_url) %>% 
  rename(province = "Province/State", 
         country_region = "Country/Region") %>% 
  pivot_longer(-c(province, country_region, Lat, Long), 
               names_to = "Date", 
               values_to = "cumulative_cases") %>% 
  # adjust JHU dates back one day to reflect US time, more or less
  mutate(Date = mdy(Date)) %>%
  arrange(country_region, province, Date) %>% 
  group_by(country_region, province) %>% 
  mutate(
    incident_cases = c(0, diff(cumulative_cases))
  ) %>% 
  ungroup() %>% 
  replace_na(list(province='N/A')) %>%
  filter(!str_detect(province, "Recovered"))


# custom results plotting function to avoid the ugly
# TableGrob messages returned by the plotting function in the
# EpiEstim package
plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases = TRUE)  # plots the incidence
  p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R")
  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
}
  

