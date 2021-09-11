library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(markdown)

library(tidyverse)
library(zoo)

## Visualization
library(rgdal)
library(sf)
library(leaflet)
library(plotly)

## Web
library(RCurl)

## function
`%nin%` = Negate(`%in%`)

# Directory -------------------------
# dir_dt <- "./covid19-public"
# dir_dt_vac <- "./citf-public"
dir_map <- "./map"


# Calling data -------------------------
## Epidemic
# dt_case <- read.csv(paste(dir_dt, "epidemic", "cases_malaysia.csv", sep = "/"))
# dt_case_state <-  read.csv(paste(dir_dt, "epidemic", "cases_state.csv", sep = "/"))
# dt_cluster <-  read.csv(paste(dir_dt, "epidemic", "clusters.csv", sep = "/"))
# # dt_death <-  read.csv(paste(dir_dt, "epidemic", "deaths_malaysia.csv", sep = "/"))
# dt_death_state <-  read.csv(paste(dir_dt, "epidemic", "deaths_state.csv", sep = "/"))
# dt_hospital <-  read.csv(paste(dir_dt, "epidemic", "hospital.csv", sep = "/"))
# dt_icu <-  read.csv(paste(dir_dt, "epidemic", "icu.csv", sep = "/"))
# dt_pkrc <-  read.csv(paste(dir_dt, "epidemic", "pkrc.csv", sep = "/"))
# dt_test <-  read.csv(paste(dir_dt, "epidemic", "tests_malaysia.csv", sep = "/"))
# dt_test_state <-  read.csv(paste(dir_dt, "epidemic", "tests_state.csv", sep = "/"))

dt_case_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv")
dt_cluster <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/clusters.csv")
dt_death_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_state.csv")
dt_hospital <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/hospital.csv")
dt_icu <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/icu.csv")
dt_pkrc <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/pkrc.csv")

# ## Mysejahtera
# dt_checkin <- read.csv(paste(dir_dt, "mysejahtera", "checkin_malaysia.csv", sep = "/"))
# dt_checkin_time <- read.csv(paste(dir_dt, "mysejahtera", "checkin_malaysia_time.csv", sep = "/"))
# dt_checkin_state <- read.csv(paste(dir_dt, "mysejahtera", "checkin_state.csv", sep = "/"))
# dt_trace <- read.csv(paste(dir_dt, "mysejahtera", "trace_malaysia.csv", sep = "/"))


# ## static
# dt_population <- read.csv(paste(dir_dt, "static", "population.csv", sep = "/"))
dt_population <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/static/population.csv")

# ## vaccine
# dt_register <- read.csv(paste(dir_dt_vac, "registration", "vaxreg_malaysia.csv", sep = "/"))
# dt_register_state <- read.csv(paste(dir_dt_vac, "registration", "vaxreg_state.csv", sep = "/"))
# dt_vac <- read.csv(paste(dir_dt_vac, "vaccination", "vax_malaysia.csv", sep = "/"))
# dt_vac_state <- read.csv(paste(dir_dt_vac, "vaccination", "vax_state.csv", sep = "/"))
dt_vac_state <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/vaccination/vax_state.csv")

## map
nc <- st_read(paste(dir_map, "malaysia_singapore_brunei_administrative_malaysia_state_province_boundary.shp", sep = "/"), quiet = TRUE)

nc <- nc %>%
  mutate(name = if_else(name == "Kuala Lumpur", "W.P. Kuala Lumpur",
                        if_else(name == "Labuan", "W.P. Labuan",
                                if_else(name == "Putrajaya", "W.P. Putrajaya",
                                        if_else(name == "Penang", "Pulau Pinang",
                                                if_else(name == "Malacca", "Melaka", name))))))



# Manipulation -------------------------
source("./data manipulation.R")


# Deploy  -------------------------
# rsconnect::setAccountInfo(name='kokyew93',
#                           token='5FAE26FC52575B42F31C41F06D2A825B',
#                           secret='GcXjnW0/WmB5NA3B7ISZiJSX5JuQZaj/oKIwvsVl')

# rsconnect::deployApp("D:/Projects/Covid/COVMAL")