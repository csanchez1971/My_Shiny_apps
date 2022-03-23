rm(list = ls())
library(lubridate)
library(shiny)
library(shinyTime)
library(dplyr)
library("FSA")
library(TSEntropies)
library(data.table)
library(mgsub)
library(rsconnect)
library(ggplot2)
library(shinycssloaders)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(bsplus)
library(shinyBS)
library(imputeTS)

VariableName<-"HR"
names <- c("HR", "RR", "SpO2", "Pulse")
disabled_choices <- names==c("HR")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  tagList(
    tags$head(tags$script(type="text/javascript", src = "UKBB_logo.js")),
    tags$style(type="text/css", "body { overflow-y: scroll; }"),
    tags$head(tags$style(".shiny-notification {position: fixed; top: 80% ;left: 80%; width: 270px}")),
    
    navbarPage("Sample Entropy calculation",
                   theme = shinytheme("cerulean"),
                   
                   tabPanel("Sample Entropy for 24 hrs measurements",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                fileInput("file", label = h4("Select input file:")),
                                verbatimTextOutput("filenameText"),
        
                                checkboxGroupButtons(inputId = "selectVariables",
                                                   label = "Select variables to display missing values",
                                                   choices = names,
                                                   selected = "HR",
                                                   # status = "primary",
                                                   checkIcon = list(
                                                     yes = icon("ok", 
                                                                lib = "glyphicon",
                                                                style = "color: steelblue"))),
                                
                                pickerInput(inputId = "Variable_to_SE", 
                                            label = "Select variable to calculate Sample Entropy",
                                            choices = names,
                                            # options = list(
                                            #   style = "btn-primary"),
                                            selected = "HR",
                                            multiple = FALSE),
                                
                                checkboxInput("couples_09_105", label = "Include elapsed times between measurements 0.9 and 1.05s"),
                                checkboxInput("interpolate", label = "Interpolate missing values"),
                                
                                br(),
                                textOutput("percentage_NA"),
                                br(),
                                
                                actionButton(inputId = "plot_button", label = "Display Data")

                              ),
                              
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            
                                            tabPanel("Sample Entropy Calculation",
                                                     br(),
                                                     h4("Select time window:"),
                                                     
                                                     uiOutput("plotUI"),
                                                     
                                                     br(),
                                                     fluidRow(
                                                       column(width = 1),
                                                       column(
                                                         uiOutput("range_selected"), width = 10),
                                                       column(width = 1)
                                                     ),
                                                     
                                                     br(),
                                                     br(),
                                                     fluidRow(
                                                       column(width = 4),
                                                       column(3, uiOutput("calcEntropy")),
                                                       column(3, uiOutput("download_button")),
                                                       column(width = 2)),
                                                     br(),
                                                     br(),
                                                     fluidRow(
                                                       column(width = 1),
                                                       column(10, dataTableOutput("SE_calculations")),
                                                       column(width = 1)
                                                     )
                                            )
                                )
                                
                              )
                            )
                            
                            
                   ),    #start tab

                   tabPanel("File merging",

                            fluidRow(
                              column(5,
                                     fileInput(
                                       inputId = "files_to_merge",
                                       label = "Choose Sample Entropy Files to merge",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")
                                     )

                              ),
                              column(3, uiOutput("download_merged"))
                            ),

                            br(),
                            tableOutput("contents")   #end tab
                            
                   )
                   
)
)
)



