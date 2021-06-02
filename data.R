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

getIndicators <- function() {
  indicators <- read.csv("data/indicators.csv", sep = ";")
  indicator_names <- arrange(indicators, indicator_name)$indicator_name
  return(indicator_names)
}

getIndicatorName <- function(indicator_code){
  
  indicators <- read.csv("data/indicators.csv", sep = ";")
  indicator_name <- indicators[which(indicators$indicator_code == indicator_code),]$indicator_name
  
  return(indicator_name)
  
}

getIndicatorCode <- function(indicator_name){
  
  indicators <- read.csv("data/indicators.csv", sep = ";")
  indicator_code <- indicators[which(indicators$indicator_name == indicator_name),]$indicator_code
  
  return(indicator_code)
  
}

get6Indicators <- function(){
  indicators <- getData()
  
  indicators <- indicators %>% select("Country.Name", "year", 
                                      "EN.ATM.CO2E.KT", "NY.GDP.PCAP.CD",
                                      "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN",
                                      "EN.POP.DNST", "Happiness.Score")
  
  return(indicators)
}

get6IndicatorsList <- function(){
  indicator_names <- read.csv("data/indicators.csv", sep = ";")
  ind6 <- c("EN.ATM.CO2E.KT", "NY.GDP.PCAP.CD",
            "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN",
            "EN.POP.DNST", "Happiness.Score")
  indicator_names <- indicator_names[which(indicator_names$indicator_code %in% ind6),]
  return(indicator_names$indicator_name)
}










