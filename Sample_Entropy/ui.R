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


VariableName<-"HR"


library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  tagList(
    tags$head(tags$script(type="text/javascript", src = "UKBB_logo.js")),
    tags$style(type="text/css", "body { overflow-y: scroll; }"),
    navbarPage("Sample Entropy calculation",
                   theme = shinytheme("cerulean"),
                   
                   tabPanel("Sample Entropy for 24 hrs measurements",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                fileInput("file", label = h4("Select input file:")),
                                verbatimTextOutput("filenameText"),
                                
                                # checkboxInput(inputId = "t25Minutes", 
                                #               label = "Allow datasets with gap > 25 Minutes",
                                #               value = FALSE),
                                
                                checkboxGroupInput(inputId = "selectVariables",
                                                   label = "Select variables to display missing values",
                                                   choices = c("HR", "RR", "SpO2", "Pulse"),
                                                   inline = TRUE,
                                                   selected = "HR"),
                                
                                selectInput(inputId = "Variable_to_SE", 
                                            label = "Select variable to calculate Sample Entropy",
                                            choices = c("HR", "RR", "SpO2", "Pulse"),
                                            selected = "HR",
                                            multiple = FALSE),
                                
                                # actionButton(inputId = "plot_button", label = "Plot graph", style = "background-color:#35a7e8; color: white !important"),
                                actionButton(inputId = "plot_button", label = "Display Data")
                                
                                # br(),
                                # br(),
                                
                                # fluidRow(
                                #   column(6, uiOutput("calcEntropy")),
                                #   column(6, uiOutput("download_button"))
                                # )
                                
                                # br(),
                                # br(),
                                # 
                                # img(src = 'neuLogoGruen500.gif', style = "display: block; margin-left: 10px; margin-right: auto;", 
                                #     height =  "50%", width = "50%", align = "center")
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
                                                       # column(withSpinner(dataTableOutput("SE_calculations"), type =4), width = 10),
                                                       column(width = 1)
                                                     )
                                            )
                                )
                                
                              )
                            )
                            
                            
                   )    #start tab

                   # tabPanel("File merging",
                   # 
                   #          fluidRow(
                   #            column(5,
                   #                   fileInput(
                   #                     inputId = "files_to_merge",
                   #                     label = "Choose Sample Entropy Files to merge",
                   #                     multiple = TRUE,
                   #                     accept = c("text/csv",
                   #                                "text/comma-separated-values,text/plain",
                   #                                ".csv")
                   #                   )
                   # 
                   #            ),
                   #            column(3, uiOutput("download_merged"))
                   #          ),
                   # 
                   #          br(),
                   #          tableOutput("contents")   #end tab
                   #          
                   # )
                   
)
)
)



