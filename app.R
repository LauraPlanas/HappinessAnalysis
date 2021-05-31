library(plotly)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

source("data.R")


# Define UI 
ui <- fluidPage(
    
    selectInput("year", "Year", choices=c("All", getYears())),
    selectInput("country", "Country", choices=c("All", getCountries())),
    textOutput('testText')
    
)


# Define server logic 
server <- function(input, output) {
    
    filterData <- reactive({
        data <- get_data()
        
        # Filtering by year
        if(input$year != "All"){
            data <- data %>% filter(data$year == input$year)
        }
        
        # Filtering by country
        if(input$country != "All"){
            data <- data %>% filter(data$Country.Name == input$country)
        }
        
        data
    })
    
    output$testText <- renderText({
        input$country
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
