library(dplyr)

getCountryCodes <- function(){
  country_codes <- read.csv("data/Country.csv")
  
}

getData <- function(){
  data <- read.csv("data/wdi_happiness_data.csv")
  country_codes <- getCountryCodes()
  merged <- merge(data, country_codes, by.x="Country.Name", by.y="TableName")
  
  return(merged)
}

getYears <- function(){
  # Returning unique years for filter
  data <- getData()
  years <- arrange(data, year)$year
  return(unique(years))
}

getCountries <- function(){
  # Returning unique countries for filter
  data <- getData()
  countries <- arrange(data, Country.Name)$Country.Name
  return(unique(countries))
}

getRegions <- function() {
  data <- getData()
  regions <- arrange(data, Region)$Region
  return(unique(regions))
}











