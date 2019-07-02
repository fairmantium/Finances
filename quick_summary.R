library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(googlesheets)
library(DT)

# Authenticate With Google Using Pre-Stored Token
gs_auth(token="../googlesheets_token.rds")

# Getting Financial Data Sheet
sheet <- gs_title("Financial Dataframe")

# Read Data From Sheets
barclaydata <- sheet %>% gs_read(ws = "Barclaycard")
Sys.sleep(2)
chasedata <- sheet %>% gs_read(ws = "Chase_Sapphire")
Sys.sleep(2)
usaadata <- sheet %>% gs_read(ws = "USAA_Checking")
Sys.sleep(2)
ashleydata <- sheet %>% gs_read(ws = "AshleyPaychecks")
Sys.sleep(2)
jimdata <- sheet %>% gs_read(ws = "JimPaychecks")

# Convert Date Columns
barclaydata$TransactionDate <- as.Date(barclaydata$TransactionDate, format="%m/%d/%Y")
chasedata$TransactionDate <- as.Date(chasedata$TransactionDate, format="%m/%d/%Y")
usaadata$TransactionDate <- as.Date(usaadata$TransactionDate, format='%m/%d/%Y')
ashleydata$TransactionDate <- as.Date(ashleydata$TransactionDate, format='%m/%d/%Y')
jimdata$TransactionDate <- as.Date(jimdata$TransactionDate, format='%m/%d/%Y')

# Clean USAA Account Data To Remove Payments to Chase and Barclaycard
usaadata <- usaadata %>% filter(!PaidTo %in% c("Chase","Barclaycard")) %>%
  filter(!SubCategory %in% c("Payroll Deposit"))

# Bind Together Dataframes
finances <- bind_rows(barclaydata)
finances <- bind_rows(finances, chasedata)
finances <- bind_rows(finances, usaadata)
finances <- bind_rows(finances, ashleydata)
finances <- bind_rows(finances, jimdata)

# Make a Separate Year, Month, & Day Column
finances$year <- lubridate::year(finances$TransactionDate)
finances$month <- lubridate::month(finances$TransactionDate)
finances$day <- lubridate::day(finances$TransactionDate)

# Summarize Data for 2019
x <- finances %>% filter(year == 2019) %>% 
  filter(month < 5) %>%
  group_by(Category2, SubCategory2) %>% 
  summarize(sum(Amount))

y <- finances %>% filter(year == 2019) %>% 
  filter(month < 5) %>%
  filter(is.na(Category2)) %>%
  group_by(Category, SubCategory, PaidTo) %>% 
  summarize(sum(Amount))