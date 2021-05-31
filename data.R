library(dplyr)

getData <- function(){
  data <- read.csv("data/wdi_happiness_data.csv")
}

getYears <- function(){
  # Returning unique years for filter
  data <- getData()
  return(unique(data$year))
}

getCountries <- function(data){
  # Returning unique countries for filter
  data <- getData()
  return(unique(data$Country.Name))
}



