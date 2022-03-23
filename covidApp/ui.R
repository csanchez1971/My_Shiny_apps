
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(shinycssloaders)

shinyUI(
    
    dashboardPage(
        dashboardHeader(title = "My covid App"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("User Manual", tabName = "User_Manual", icon = icon("book")),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"))
                
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "User_Manual",
                        h2("User Manual"),
                        h4("The following app displays the evolution of the Covid cases by Continent and Country. To display the current status of the values, click on the 'Dashboard' tap"),
                        br(),
                        img(src='dashboard2.png'),                        
                        br(),
                        br(),
                        br(),
                        h4("On the mail screen 'Dashboard', you will find the app that will display the following information:"),
                        
                        withTags(
                            ol(
                                li("Total number of Covid cases"),
                                li("Total number of Covid deaths"),
                                li("Evolution of covid cases by Continent"),
                                li("Evolution of covid cases by Country"),
                                li("Checkbox to select remove continents to be displayed"),
                                li("Box to select countries to be displayed. Multiple values available")
                            )
                        ),
                        
                        br(),
                        img(src='body2.png'), 
                 ),    

                    
                tabItem(tabName = "dashboard",
                        fluidRow(
                            infoBox(h2("Yet another Covid App"), fill=TRUE),
                            # valueBox(prettyNum("totalCasesBox", big.mark = ","), "Total Cases", icon = icon("virus")),
                            # valueBox(prettyNum("totalDeathsBox", big.mark = ","), "Total Deaths", icon = icon("skull-crossbones"))
                        ),
                        fluidRow(
                            box(title = "Covid evolution by Continent", solidHeader = TRUE, collapsible = TRUE, status = "primary",
                                plotlyOutput("plot1"), type = 4),
                            box(title = "Daily cases by Country", solidHeader = TRUE, collapsible = TRUE, status = "primary",
                                plotlyOutput("plot2"))
                            ),

                        fluidRow(
                            box(background = "purple", 
                                checkboxGroupInput(inputId = "Continent",
                                                   label = "Select Continents to display",
                                                   c("Europe", "North America", "Africa", "Asia", "Oceania", "South America"),
                                                   inline = TRUE,
                                                   selected = list("Europe", "North America", "Africa", "Asia", "Oceania", "South America")
                                                   )),
                            
                            box(background = "purple", 
                                selectInput("Country", "Select Countries",
                                            choices = unique(covid2$location),
                                            selectize = TRUE,
                                            selected = "Switzerland",
                                            multiple = TRUE),
                                )
                                 )
                        )
            )
        )
    )
)
