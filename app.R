library(plotly)
library(DT)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

source("data.R")


# Define UI 


sidebar <- sidebarMenu(
    menuItem("World indicators", tabName="page1", icon=icon("globe-americas")),
    menuItem("Country analysis", tabName="page2", icon=icon("flag")),
    menuItem("Correlated indicators", tabName="page3", icon=icon("calendar")),
    menuItem("About", tabName="about", icon=icon("info-circle"))
    
)


page1_body <- tabItem(tabName = "page1",
                      
                      column(width = 8,
                             
                             box(title="Filters",
                                 width = NULL,
                                 
                                 fluidRow(
                                     column(width = 4,
                                         selectInput("indicator", "Indicator", choices=getYears())
                                     ),
                                     
                                     column(width = 4,
                                        selectInput("year", "Year", choices=getYears())
                                     ),
                                     
                                     column(width = 4,
                                        selectInput("region", "Region", choices=c("All", getRegions()))
                                     )
                                 )
                                
                                 
                                 
                                 
                             ),
                             
                             box(title="Worldwide indicators",
                                 width = NULL,
                                 plotlyOutput("map")
                             )
                             
                     ),
                      
                      column(width = 4,

                          box(title="Table",
                              width = NULL,
                              dataTableOutput("indicators_table")
                          )
                          
                      )
                      
                      
)

page2_body <- tabItem(tabName = "page2",
                      h2("Page 2"),
                      selectInput("year", "Year", choices=getYears()),
                      selectInput("country", "Country", choices=c("All", getCountries())),
                      textOutput('testText')
                      
)





body <- tabItems(
        page1_body,
        
        page2_body,
        
        tabItem(tabName="page3",
                h2("Page 3")
        ),
        
        tabItem(tabName="about",
                h2("About")
            
        )
)
    







ui <- dashboardPage(
    
    header=dashboardHeader(
        title = tagList(
            span(class = "logo-lg", "Happiness Analysis"), 
            icon("smile"))
    ),
    
    sidebar=dashboardSidebar(
        sidebar
    ),
    body=dashboardBody(body),
    footer=dashboardFooter()
)


# Define server logic 
server <- function(input, output) {
    
    filterData <- reactive({
        data <- getData()
        
        # Filtering by year
        data <- data %>% filter(data$year == input$year)
        
        
        # Filtering by country
        if(input$country != "All"){
            data <- data %>% filter(data$Country.Name == input$country)
        }
        
        # Filtering by region
        if(input$region != "All"){
            data <- data %>% filter(data$Region == input$region)
        }
        
        data
    })
    
    output$testText <- renderText({
        input$country
    })
    
    # Page 1 graphs ------------------------------------------------------------
    # World Happiness Map
    output$map <- renderPlotly({
        data <- filterData()
        
        fig <- plot_ly(data, type='choropleth', locations=data$CountryCode, 
                       z=data$Happiness.Score, text=data$Country.Name, 
                       colors="Blues")
        #fig <- fig %>% layout(autosize=F, height=300)
        fig
    })
    
    output$indicators_table <- renderDataTable({
        data <- filterData()
        table_data <- data %>% select("Country.Name", "Happiness.Score") %>% 
            arrange(desc(Happiness.Score))
        
        datatable(table_data)
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
