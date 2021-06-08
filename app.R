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
    menuItem("Indicators correlation", tabName="page3", icon=icon("chart-line")),
    menuItem("About", tabName="about", icon=icon("info-circle"))
    
)

# Page 1 -------------------------------------------------
page1_body <- tabItem(tabName = "page1",
                      h2("World indicators"),
                      
                      column(width = 8,
                             
                             box(title="Filters",
                                 status = 'danger', solidHeader = TRUE,
                                 width = NULL,
                                 
                                 fluidRow(
                                     column(width = 6,
                                         selectInput("indicator", "Indicator", choices=getIndicators(),
                                                     selected="Happiness Score (from 0 to 10)")
                                     ),
                                     
                                     column(width = 3,
                                        selectInput("year", "Year", choices=getYears())
                                     ),
                                     
                                     column(width = 3,
                                        selectInput("region", "Region", choices=c("All", getRegions()))
                                     )
                                 )
                                
                                 
                                 
                                 
                             ),
                             
                             box(title="Worldwide indicators",
                                 width = NULL,
                                 status = 'primary', solidHeader = TRUE,
                                 plotlyOutput("map")
                             )
                             
                     ),
                      
                      column(width = 4,

                          box(title="Countries ordered by indicator",
                              width = NULL,
                              status = 'primary', solidHeader = TRUE,
                              dataTableOutput("indicators_table")
                          )
                          
                      )
                      
                      
)


# Page 2 --------------------------------------------------
page2_body <- tabItem(tabName = "page2",
                      h2("Country Analysis"),
                      
                      fluidRow(
                          column(width = 3,
                                 box(title="Filters",
                                     width = NULL,
                                     status = 'danger', solidHeader = TRUE,
                                     selectInput("country2", "Country", choices=getCountries()),
                                     selectInput("year2", "Year", choices=getYears())
                                 )
                                 
                          ),
                          
                          column(width = 9,
                                 fluidRow(
                                     valueBoxOutput("happiness_box"),
                                     valueBoxOutput("gdp_box"),
                                     valueBoxOutput("infant_box")
                                 ),
                                 
                                 fluidRow(
                                     valueBoxOutput("density_box"),
                                     valueBoxOutput("co2_box"),
                                     valueBoxOutput("life_box")
                                 )
                                 
                                 
                          )
                      ),
                      
                      fluidRow(
                          
                          box(title="Temporal evolution of selected indicator",
                              width = 12,
                              status = 'primary', solidHeader = TRUE,
                              selectInput("indicator2", "Indicator for graph", choices=get6IndicatorsList(),
                                          selected="Happiness Score (from 0 to 10)"),
                              plotlyOutput("temporal_evolution")
                              )
                      )
                      
                     
)


page3_body <- tabItem(tabName="page3",
                      h2("Indicators correlation"),
                      
                      
                      fluidRow(
                          box(title = "Select axes",
                              width = 10, 
                              status = 'danger', solidHeader = TRUE,
                              column(width=6, 
                                     selectInput("indicator3_x", "Indicator X Axis", choices=getIndicators(),
                                          selected="Happiness Score (from 0 to 10)")
                                     ),
                              
                              column(width=6,
                                     selectInput("indicator3_y", "Indicator Y Axis", choices=getIndicators(),
                                          selected="Life expectancy at birth, total (years)")
                              )
                              
                              
                              
                              
                          ),
                    
                        box(title="Correlation coefficient",
                            width = 2,
                            status = 'info', solidHeader = TRUE,
                            h1(textOutput("correlation"), style = "font-size:45px;")
                            )
                          
                          
                      ),
                      
                      box(title = "Correlation between indicators",
                          width = 12,
                          status = 'primary', solidHeader = TRUE,
                          plotlyOutput("correlation_graph")
                          )
)


about_body <- tabItem(tabName="about",
                      
                      box(title="About the project",
                         "This R Shiny Application is using data from the World Development Indicators dataset from the 
                         World Bank and a Happiness survey data from a Kaggle dataset. The merge of these 2 datasets
                         can be found in the following GitHub project:",
                         br(),
                         tags$a(href="https://github.com/LauraPlanas/WDI_happiness_merge", "GitHub: WDI and Happiness dataset merge"),
                         
                         br(),
                         br(),
                         "This project is part of the subject Data Visualitzation from the Data Science Master's Degree from the Open
                         University of Catalonia (UOC)."
                         
                          
                      ),
                      
                      box(title="The code",
                          "You can check the code to run this application here: ",
                          br(),
                          tags$a(href="https://github.com/LauraPlanas/HappinessAnalysis", "GitHub: Happiness Analysis")
                      ),
                      
                      box(title="Author",
                          "Thanks for checking my application!", icon("heart"),
                          br(),
                          "I'm Laura Planas Simón, you can find me in ",
                          br(),
                          br(),
                          tags$a(href="https://www.linkedin.com/in/laura-planas/", icon("fab fa-linkedin", "fa-2x")),
                          tags$a(href="https://github.com/LauraPlanas", icon("fab fa-github", "fa-2x"))
                          
                          )
)



# Body ---------------------------------
body <- tabItems(
        page1_body,
        
        page2_body,
        
        page3_body,
        
        about_body
            
        
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
    body=dashboardBody(
        tags$style(HTML("


.box.box-solid.box-danger>.box-header {
  color:#fff;
  background:#A9A9A9
                    }

.box.box-solid.box-danger{
border-bottom-color:#A9A9A9;
border-left-color:#A9A9A9;
border-right-color:#A9A9A9;
border-top-color:#A9A9A9;
}

.box.box-danger>.box-header {
  color:#000000;
  background:#fff
                    }

.box.box-danger{
border-bottom-color:#A9A9A9;
border-left-color:#A9A9A9;
border-right-color:#A9A9A9;
border-top-color:#A9A9A9;
}

                                    ")),
        body
        )
)


# Define server logic 
server <- function(input, output) {
    
    # Page 1 graphs ------------------------------------------------------------
    filterData <- reactive({
        data <- getData()
        
        # Filtering by year
        data <- data %>% filter(data$year == input$year)

        
        # Filtering by region
        if(input$region != "All"){
            data <- data %>% filter(data$Region == input$region)
        }
        
        data
    })
    
    
    # World Happiness Map
    output$map <- renderPlotly({
        data <- filterData()
        indicator_code <- getIndicatorCode(input$indicator)
        
        fig <- plot_ly(data, type='choropleth', locations=data$CountryCode, 
                       z=data[[indicator_code]], text=data$Country.Name, 
                       colors="Blues")
        #fig <- fig %>% layout(autosize=F, height=300)
        fig
    })
    
    # Top countries table
    output$indicators_table <- renderDataTable({
        data <- filterData()
        indicator_code <- getIndicatorCode(input$indicator)
        
        table_data <- data %>% select("Country.Name", indicator_code)
        table_data <- arrange(table_data, across(starts_with(indicator_code), desc)) 
        colnames(table_data) <- c("Country", input$indicator)
        
        
        datatable(table_data)
    })
    
    
    # Page 2 graphs ------------------------------------------------------------
    filterData2 <- reactive({
        data <- getData()
        
        # Filtering by year
        data <- data %>% filter(data$year == input$year2)
        
        # Filtering by country
        data <- data %>% filter(data$Country.Name == input$country2)

        data
    })
    
    
    # Happiness Score box
    output$happiness_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_happiness <- round(mean(all_data$Happiness.Score, na.rm=TRUE), digits=2)

        filtered_data <- filterData2()
        happiness <- round(filtered_data$Happiness.Score, digits=2)


        # Values for Value box
        title <- "Happiness Score (0 to 10)"
        world_avg <- paste("World Avg. = ", avg_happiness)

        box_text <- HTML(paste(paste(title, br()), world_avg))

        color <- "black"
        if(!is.na(happiness) && !is.na(avg_happiness)){
            if(happiness >= avg_happiness){
                color <- "green"
            } else {
                color <- "red"
            }
        }

        valueBox(value=happiness, subtitle=box_text, icon=icon("smile"),
                 color=color)

    })
    
    # GDP per capita box
    output$gdp_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_gdp <- round(mean(all_data$NY.GDP.PCAP.CD, na.rm=TRUE), digits=2)
        
        filtered_data <- filterData2()
        gdp <- round(filtered_data$NY.GDP.PCAP.CD, digits=2)
        
        
        # Values for Value box
        color <- "black"
        if(!is.na(gdp) && !is.na(avg_gdp)){
            if(gdp >= avg_gdp){
                color <- "green"
            } else {
                color <- "red"
            }
        }
        
        avg_gdp <- format(avg_gdp, big.mark=",")
        gdp <- format(gdp, big.mark=",")
        
        title <- "GDP per capita"
        world_avg <- paste("World Avg. = ", avg_gdp)
        
        box_text <- HTML(paste(paste(title, br()), world_avg))
        
        valueBox(value=gdp, subtitle=box_text, icon=icon("dollar-sign"),
                 color=color)
        
    })
    
    # Infant Mortality box
    output$infant_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_inf <- round(mean(all_data$SP.DYN.IMRT.IN, na.rm=TRUE), digits=2)
        
        filtered_data <- filterData2()
        inf <- round(filtered_data$SP.DYN.IMRT.IN, digits=2)
        
        
        # Values for Value box
        color <- "black"
        if(!is.na(inf) && !is.na(avg_inf)){
            if(inf >= avg_inf){
                color <- "red"
            } else {
                color <- "green"
            }
        }
        
        avg_inf <- format(avg_inf, big.mark=",")
        inf <- format(inf, big.mark=",")
        
        title <- "Infant Mortality %"
        world_avg <- paste("World Avg. = ", avg_inf)
        
        box_text <- HTML(paste(paste(title, br()), world_avg))
        
        valueBox(value=inf, subtitle=box_text, icon=icon("dizzy"),
                 color=color)
        
    })
    
    # Population density box
    output$density_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_dens <- round(mean(all_data$EN.POP.DNST, na.rm=TRUE), digits=2)
        
        filtered_data <- filterData2()
        dens <- round(filtered_data$EN.POP.DNST, digits=2)
        
        
        # Values for Value box
        color <- "black"
        if(!is.na(dens) && !is.na(avg_dens)){
            if(dens >= avg_dens){
                color <- "red"
            } else {
                color <- "green"
            }
        }
        
        avg_dens <- format(avg_dens, big.mark=",")
        dens <- format(dens, big.mark=",")
        
        title <- "Population Density"
        world_avg <- paste("World Avg. = ", avg_dens)
        
        box_text <- HTML(paste(paste(title, br()), world_avg))
        
        valueBox(value=dens, subtitle=box_text, icon=icon("city"),
                 color=color)
        
    })
    
    # CO2 box
    output$co2_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_co2 <- round(mean(all_data$EN.ATM.CO2E.KT, na.rm=TRUE), digits=2)
        
        filtered_data <- filterData2()
        co2 <- round(filtered_data$EN.ATM.CO2E.KT, digits=2)
        
        
        # Values for Value box
        color <- "black"
        if(!is.na(co2) && !is.na(avg_co2)){
            if(co2 >= avg_co2){
                color <- "red"
            } else {
                color <- "green"
            }
        }
        
        avg_co2 <- format(avg_co2, big.mark=",")
        co2 <- format(co2, big.mark=",")
        
        title <- "CO2 Emissions (kt)"
        world_avg <- paste("World Avg. = ", avg_co2)
        
        box_text <- HTML(paste(paste(title, br()), world_avg))
        
        valueBox(value=co2, subtitle=box_text, icon=icon("industry"),
                 color=color)
        
    })
    
    # Life Expentacy Box
    output$life_box <- renderValueBox({
        all_data <- getData()
        all_data %>% filter(all_data$year == input$year2)
        avg_life <- round(mean(all_data$SP.DYN.LE00.IN, na.rm=TRUE), digits=2)
        
        filtered_data <- filterData2()
        life <- round(filtered_data$SP.DYN.LE00.IN, digits=2)
        
        
        # Values for Value box
        color <- "black"
        if(!is.na(life) && !is.na(avg_life)){
            if(life >= avg_life){
                color <- "green"
            } else {
                color <- "red"
            }
        }
        
        avg_life <- format(avg_life, big.mark=",")
        life <- format(life, big.mark=",")
        
        title <- "Life Expentancy"
        world_avg <- paste("World Avg. = ", avg_life)
        
        box_text <- HTML(paste(paste(title, br()), world_avg))
        
        valueBox(value=life, subtitle=box_text, icon=icon("baby"),
                 color=color)
        
    })
    
    # Temporal Evolution
    output$temporal_evolution <- renderPlotly({
        data <- get6Indicators()
        
        indicator_code <- getIndicatorCode(input$indicator2)
        
        avg_indicator <- aggregate(data[2:8], list(data$year), mean, na.rm=TRUE, na.action=NULL)
        
        # Filtering by country
        data <- data %>% filter(data$Country.Name == input$country2)
        data <- arrange(data, year)
        
        
        
        x <- list(
            title = "Year"
        )
        y <- list(
            title = "Indicator"
        )
       
        fig <- plot_ly(data, x = ~as.factor(data$year), y = ~data[[indicator_code]], type = 'scatter', mode = 'lines', name="Selected Country")
        fig <- fig %>% add_trace(x= ~as.factor(avg_indicator$year), y = ~ avg_indicator[[indicator_code]], type = 'scatter', mode = 'lines', name="World Average")
        fig <- fig %>% layout(xaxis = x, yaxis = y)
        fig
    })
    
    
    
    # Page 3 graphs ------------------------------------------------------------
    
    output$correlation_graph <- renderPlotly({
        data <- getData()
        
        indicator_code_x <- getIndicatorCode(input$indicator3_x)
        indicator_code_y <- getIndicatorCode(input$indicator3_y)
        
        data_x <- data[[indicator_code_x]]
        data_y <- data[[indicator_code_y]]
        data <- data.frame(cbind(data_x, data_y))
        colnames(data) <- c("X", "Y")
        
        correlation <- cor(data$X, data$Y, use="complete.obs")
        
        linear <- data %>% filter(!is.na(Y)) %>% filter(!is.na(X)) %>% lm(Y ~ X,.) %>% fitted.values()

        data <- data %>% filter(!is.na(Y)) %>% filter(!is.na(X))
        
        x <- list(
            title = input$indicator3_x
        )
        y <- list(
            title = input$indicator3_y
        )
        
        
        fig <- plot_ly(data, x=~X, y=~Y, type="scatter", mode="markers") %>%
               add_markers(y = ~Y) %>% 
               add_trace(x=~X, y=linear, mode="lines", line=list(color='rgb(60,141,188)')) %>%
               layout(xaxis = x, yaxis = y, showlegend = FALSE)
    })
    
    output$correlation <- renderText({
        data <- getData()
        
        indicator_code_x <- getIndicatorCode(input$indicator3_x)
        indicator_code_y <- getIndicatorCode(input$indicator3_y)
        
        data_x <- data[[indicator_code_x]]
        data_y <- data[[indicator_code_y]]
        data <- data.frame(cbind(data_x, data_y))
        colnames(data) <- c("X", "Y")
        
        correlation <- round(cor(data$X, data$Y, use="complete.obs"), digits=2)
        correlation
        

    }) 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
