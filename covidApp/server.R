
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(shinycssloaders)

# covid2 <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
#                    colClasses = c(rep("character", 4), "numeric", rep("NULL",2), "numeric",rep("NULL", 51)))



shinyServer(function(input, output) {
    covid2 <- read.csv("./data/owid-covid-data.csv", 
                       colClasses = c(rep("character", 3), "Date", rep("numeric",2), "NULL", rep("numeric",2) ,rep("NULL", 50)))
    
    covid2 <- covid2[covid2$continent!="", ]
    covid2[,1:2] <- lapply(covid2[,1:2], as.factor)
    
    covid_continent <- covid2  %>% 
        group_by(continent, date)%>% 
        dplyr::summarise(totalCases=sum(total_cases, na.rm=TRUE), totalDeaths = sum(total_deaths, na.rm=TRUE)) 
    
    totals <- covid_continent %>% arrange(date) %>% group_by(date) %>% tail()
    
    
    output$totalCasesBox <- renderValueBox(
        sum(totals$totalCases) %>% 
            valueBox(subtitle = "Total Cases", icon = icon("virus")),
    )   
    
    output$totalDeathsBox <- renderValueBox(
        sum(totals$totalDeaths) %>% 
            valueBox(subtitle = "Total Deaths", icon = icon("crossbones")),
    )

    output$plot1 <- renderPlotly({
        
        covid_continent <- covid_continent %>% filter(continent %in% input$Continent)
        
        plot_ly(covid_continent, x = ~date, y = ~totalCases, color = ~continent, type = 'scatter', mode = 'lines')
    })

    output$plot2 <- renderPlotly({
        covid2 <- covid2 %>% filter(location %in% input$Country)
        
        p <- plot_ly(covid2, x = ~date, y = ~new_cases, color = ~location, type = 'bar')
    })
    
})
