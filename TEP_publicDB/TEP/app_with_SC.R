
# load("~/Carlos/TEP/TEP_Shiny_reduced.RData")
# options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16384m"))

load("/dlab/NGS/bioinformatics/PCS/DIPGENOMICS/TEP_Shiny_data/version_0.9/TEP_Shiny.RData")
# source("~/Carlos/TEP/code/functions.R")
# source("http://ava-web.statwb.eu.novartis.net/cfg/installer.R")

library(tidyverse)
library(ava)
library(shiny)
library(DT)
library(data.table)
library(knitr)
library(pander)
library(xlsx)
library(shinyWidgets)
# library(bsplus)         # pop-up info icon
library(shinyjs)
# library(cowplot)      #Testing color blind (https://www.datanovia.com/en/blog/how-to-stimulate-colorblindness-vision-in-r-figures/)
# library(colorspace)   #Testing color blind (https://www.color-blindness.com/coblis-color-blindness-simulator/)
# library(colorblindr)  #Testing color blind
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(xlsx)



TEP_Version <- "version 0.9"


# ui <- avaNavbarPage("Navbar page", id = "tabs",
ui <- navbarPage("Select app", id = "tabs",
                 
                 tabPanel("Home", selectInput("Type", "Select type", choices = c("Select", "TEP", "Single_Cell"), selected = "Select")),
                 
  #shinythemes::themeSelector(),
  tabPanel("TEP Home", value = "TEP_Home", icon = ava::icon("home"),
           
           
           
           sidebarLayout(
             sidebarPanel(
               width = 3,
               style = "position:fixed;width:22%;",
               
               br(),
               br(),
               h1("Target Expression Profiling", align = "center"),
               br(),
               br(),
               img(src = 'logo.jpg', style = "display: block; margin-left: auto; margin-right: auto;"),
               br(),
               br(),
               br()
             ),
             
             
             
             mainPanel(
               width = 9,
               # width = 9.5,
               style = "background: white",
               
               tabsetPanel(
                 # "Documentation",
                 # icon = ava::icon("book"),
                 tabPanel("Welcome to TEP",
                          
                          h1("What's TEP"),
                          HTML('<hr style="color: grey;">'),
                          
                          "Target Expression Profiling is an App that allows you, for a selected gene, display its profile expression
                                   by tissue from different Public and internal Databases.",
                          br(),
                          
                          "The current version is retrieving and displaying information from the following sources:",
                          br(),
                          br(),
                          
                          DT::dataTableOutput("aboutDB") %>% withSpinner(color = "#187bcd"),   #Added Spinner to show when the app is still loading
                          br(),
                          "Please, select 'TEP' tap at the top-left to start working or select any of the following tabs to find some help about the application:",
                          br(),
                          img(src = 'help_menu.png', align = "left")
                          
                          
                 ),
                 
                 tabPanel("Help: Parameters selection",
                          br(),
                          "Once you select the TEP menu, you will be asked to fulfill the following fields:",
                          
                          fluidRow(
                            column(3,
                                   br(),
                                   br(),
                                   
                                   img(src = 'selection.png', style = "display: block; margin-left: 10px; margin-right: auto;")
                                   
                            ),
                            column(1),
                            
                            column(8,
                                   br(),
                                   
                                   h5("Enter Gene Name"),
                                   "Select from the list or introduce the gene you want to be displayed",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Select Tissue System"),
                                   "Select 'All' to display all tissues systems or choose individual values",
                                   "Default value: 'All'",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Select Species"),
                                   "Filter currently not implemented. Use Database filter instead",
                                   "Default value: 'All'",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Select Database"),
                                   "Choose 'Select All' to display all tissues systems or select individual values",
                                   "Default value: 'Select All'",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Display plots with tissues grouped"),
                                   "Select 'Sub Tissues' to display all tissues or 'Tissues' to group sub parts of the organs/tissues",
                                   "Default value: 'Tissue'",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Select Microarray cutoff"),
                                   "Define the desired cutoff to subset microarray databases",
                                   "Default value: 25",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Submit"),
                                   "Launch the query",
                                   
                                   br(),
                                   br(),
                                   
                                   h5("Download document. Please, choose format"),
                                   "Select download type (optional)"
                                   
                            )
                          )
                 ),
                 
                 tabPanel("Help: Data visualization",
                          
                          br(),
                          fluidRow(
                            column(5,
                                   "Once we executed the query pressing the 'Submit' button, the app will display a summary of each database, including a dot plot with 
                     all the values with a horizontal bar marking the median and a bar plot indicating the number of samples that we have for each tissue.",
                                   br(),
                                   
                                   img(src = 'plot_720.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                                   
                            ),
                            
                            column(1),
                            
                            column(5,
                                   "At the 'Heatmap' tab we will find a heatmap plot summarizing the tissue expression (radius of the circle)
                     and enrichment (color) for every tissue along the different databases. On the alternative axis we can find information
                     about the Tissue system and the species",
                                   br(),
                                   br(),
                                   
                                   img(src = 'heatmap_720.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                            )
                          ),
                          
                          br(),
                          
                          fluidRow(
                            "On 'Summary' tab it is displayed the different databases summarized, including probe and chip (in microarray DB),
              enriched tissues, Z-Score, Tau statistic, and the subset values for the selected gene",
                            br(),
                            column(5, img(src = 'table_sum720.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                                   
                            ),
                            
                            column(1),
                            
                            column(5, img(src = 'table_data720.png', style = "display: block; margin-left: 1px; margin-right: auto;"))
                            
                          ),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   "'Mehods' tab contain information about the different databases, normalization that had been performed on the
                     different data, and different statistics calculated.",
                                   br(),
                                   
                                   img(src = 'methods_720.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                                   
                            ),
                            
                            
                            column(1),
                            
                            column(5,
                                   "'Session Info' tab includes version information about R, the OS and attached or loaded packages.",
                                   br(),
                                   
                                   img(src = 'sessionInfo_720.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                                   
                            )
                            
                          )
                 ),
                 
                 tabPanel("Help: Report download",
                          br(),
                          "Once generated the different plots and tables, it is possible to download a report including all this information in
                                         any of the different formats, HTML, PDF or Word. To download, just select the desired output and click on 'Download.",
                          br(),
                          br(),
                          
                          img(src = 'download.png', style = "display: block; margin-left: 1px; margin-right: auto;")
                          
                 )
               )
             )
           )
  ),
  
  
  tabPanel("TEP", value = "TEP_TEP", icon = ava::icon("analytics"),
           
           br(),
           
           sidebarLayout(
             sidebarPanel(
               h2("Target Expression Profiling"),
               # style = "position:fixed;width:22%;",
               br(),
               width = 3,
               
               useShinyjs(),
               
               
               
               div(id= "reset",
                   fluidRow(
                     column(
                       6,
                       selectizeInput(
                         'geneName',
                         label = 'Type Gene Name',
                         choices = NULL             #Gene list moved to Server because app can fail because of slow connection (sometimes in Cambridge). loading speed improved
                         
                       )
                       
                     ),
                     
                     column(6,
                            offset = 0,
                            style = 'padding:33px;',
                            textOutput("gene_descr")         # Include the gene description at the right of the gene symbol
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            
                            selectInput("tissue", label = "Select Tissue System",
                                        choices = c("All", as.character(sort(levels(Atlas_Dictionary_final$Tissue_type)))),  #Load values from Atlas Dictionary
                                        selected = "All",
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        width = NULL,
                                        size = NULL
                            )
                     ),
                     
                     column(6,
                            selectInput('grouped_tissue', "Group Subtissues", choices = c("Sub Tissues", "Tissue")))  #Added functionality to grouped tissues belonging to common 'father' (e.g. "cerebellumk", "hippocampus", etc grouped under "brain")
                     
                   ),
                   
                   selectInput("species", label = "Select Species",
                               
                               choices = c(
                                 "All",
                                 "Homo sapiens",
                                 "Nonhuman primate",
                                 "Canis familiaris",
                                 "Sus scrofa domesticus",
                                 "Rattus norvegicus",
                                 "Mus musculus"
                                 
                               ),
                               
                               selected = "All",
                               multiple = TRUE,
                               selectize = TRUE,
                               width = NULL,
                               size = NULL
                   ),
                   
                   
                   
                   selectInput(inputId = "database", label = "Select Database:",
                               
                               choices = list(
                                 `Select All` = "All",   #If "All" value selected, app will take into account all databases
                                 `Homo sapiens` = list("GTEx", "Human Protein Atlas" = "HPA", "BioGPS" = "BioGPS_Human", "DIS Atlas Human" = "DIS_Atlas_Human"),
                                 `Nonhuman primate` = list("DIS Atlas Monkey" = "DIS_Atlas_Monkey", "NHPRTR"),
                                 
                                 `Canis familiaris` = list("DIS Atlas Dog" = "DIS_Atlas_Dog", "BarkBase"),
                                 
                                 `Sus scrofa domesticus` = list("Minipig Atlas" = "Minipig_Atlas"),
                                 
                                 `Rattus norvegicus` = list( "DIS Atlas Rat" = "DIS_Atlas_Rat",
                                                             "Ratlas", 
                                                             "Rat_MTAB6081"),
                                 
                                 `Mus musculus` = list("DIS Atlas Mouse" = "DIS_Atlas_Mouse",
                                                       "BioGPS (GNF1M)" = "BioGPS_Mouse", 
                                                       "BioGPS (MOE403)" = "BioGPS_Mouse2", 
                                                       "Mouse_MTAB6081")
                                 
                               ),
                               
                               multiple = TRUE,
                               selectize = TRUE,
                               width = NULL,
                               selected = "All"
                   ),
                   
                   
                   #For microarrayd databases, implemented the possibility to filter by mean value (default = 0)
                   sliderInput(inputId = "microarray_cutoff", "Select Microarray cutoff",
                               min = 0,
                               max = 100,
                               value = 0
                   ),
                   
                   prettyRadioButtons('scalePlot', 'Select plot scale:',  #Plots can be displayed with y-axis in linear or logarithmic scale
                                      choices = list('Linear', 'Logarithmic'),
                                      inline = TRUE,
                                      selected = 'Linear',
                                      status = "primary"
                   )
               ),
               
               fluidRow(
                 column(6,
                        tags$style(type = "text/css", "#Submit {background-color:#337ab7; color: white !important}"),   #Color the Submit button in blue
                        actionButton(inputId = "Submit", label = "Submit gene")
                 ),    
                 
                 column(6, 
                        tags$style(type = "text/css", "#reset_input {background-color:#337ab7; color: white !important}"),
                        actionButton(inputId = "reset_input", label = "Reset inputs")
                 )
                 
               ),
               
               br(),
               
               prettyRadioButtons('format', 'Download document. Please, choose format:',   #Implememnted possibility of download the final report in 'Word', 'PDF' and 'html' formats
                                  choices = list('Word', 'PDF', 'HTML'),
                                  inline = TRUE,
                                  selected = 'Word',
                                  status = "primary"
               ),
               
               # downloadButton("download", label="Download", class = "butt"),
               # tags$head(tags$style(".butt{background-color: #337ab7;} .butt{color: white;}")),
               
               # downloadButton(inputId = "download", label = "Download", icon = ava::icon("download"),
               #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               
               fluidRow(
                 column(6,
                        tags$style(type = "text/css", "#download {background-color:#337ab7; color: white !important}"),   #Color the Submit button in blue
                        downloadButton("download", label = "Download", class = "butt")
                 ),    
                 
                 column(6, 
                        tags$style(type = "text/css", "#dl {background-color:#337ab7; color: white !important}"),
                        downloadButton("dl","Download raw data")                              # TO BE IMPLEMENTED LATER WHEN NO MEMORY ISSUES
                 )
                 
               ),
               
               # tags$style(type = "text/css", "#download {background-color:#337ab7; color: white !important}"),
               # 
               # downloadButton("download", label = "Download", class = "butt"),
               # 
               # tags$style(type = "text/css", "#download {background-color:#337ab7; color: white !important}"),
               # 
               # downloadButton("dl","Export in Excel"),                              # TO BE IMPLEMENTED LATER WHEN NO MEMORY ISSUES
               
               
               # tags$head(tags$style(".butt{color: #FFFFFF;}")),
               
               br(),
               br()
             ),
             
             mainPanel(width = 9, align = "center", style = "background: white",
                       
                       tabsetPanel(
                         
                         tabPanel("Homo sapiens",   #Different plots arragend by species in different tabs
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('DIS_Atlas_Human') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Homo sapiens') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Human: DIS Atlas DB, microarray"),
                                    br(),
                                    
                                    #For each of the microarray DB, a second filter by 'probe' has been implemented. By default system takes the one with highest mean and user
                                    #can select a new one. This new one will be moved into the different plots and summary tables
                                    
                                    selectInput(inputId = 'probeDIS_Human', label = 'Select Probe', choices = "NULL"
                                    ),
                                    
                                    plotOutput("graph_DIS_Human", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  
                                  conditionalPanel(   #If database or species is not selected (or 'All') or no gene name is still selected, the panel won't be shown.
                                    condition = "(input.database.includes('GTEx') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Homo sapiens') || input.species.includes('All'))",
                                    br(),
                                    
                                    # h4(paste0("Human: GTEx DB, RNAseq ", input$geneName, collapse = "")),  #Not posible since input$geneName should be in server and need to create one output for each time we use it
                                    h4("Human: GTEx DB, RNAseq"),
                                    
                                    br(),
                                    plotOutput("graph_GTEx", height=600) %>% withSpinner(color = "#187bcd", type = 1)
                                  ),
                                  
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('HPA') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Homo sapiens') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4(
                                      "Human: The Human Protein Atlas DB, RNAseq"
                                    ),
                                    br(),
                                    
                                    plotOutput("graph_HPA", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('BioGPS_Human') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Homo sapiens') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Human: BioGPS - Human DB, microarray"),
                                    br(),
                                    
                                    selectInput(inputId = 'probeBioGPS_Human', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_BioGPS_Human", height=600) %>% withSpinner(color = "#187bcd")
                                  )
                         ),
                         
                         tabPanel("Nonhuman primate",
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('DIS_Atlas_Monkey') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Monkey: DIS Atlas DB, microarray"),
                                    br(),
                                    
                                    selectInput(inputId = 'probeDIS_Monkey', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_DIS_Monkey", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  
                                  #For the case of NHPRTR, it has been created a dropdown menu to select the database to display. By default plotting all DB in one single plot
                                  #because of small number of samples (1-2) per tissue.
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('NHPRTR') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Monkey: NHPRTR, RNAseq"),
                                    br(),
                                    
                                    selectInput(inputId = 'NHPRTR_merge', label = 'Select merged DB or specific',
                                                
                                                choices = c("All macaque"    = "NHPRTR_all_mac", 
                                                            "Cyno Chinese"   = "NHPRTR_CMC",
                                                            "Cyno Mauritian" = "NHPRTR_CMM",
                                                            "Cyno Japanese"  = "NHPRTR_JMI",
                                                            "Rhesus Indian"  = "NHPRTR_RMI"),
                                                
                                                selected = "NHPRTR_all_mac"
                                                
                                    ),
                                    
                                    plotOutput("graph_macaque", height=600) %>% withSpinner(color = "#187bcd"))
                                  
                                  
                                  
                                  # conditionalPanel(
                                  #   condition = "(input.database.includes('NHPRTR_CMC') || input.database.includes('All')) && input.geneName!='Select Gene'
                                  #               && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                  #   
                                  #   br(),
                                  #   h4("Monkey: NHPRTR - Cynomolgus Macaque Chinese DB, RNAseq"),
                                  #   br(),
                                  #   plotOutput("graph_CMC", height=600) %>% withSpinner(color = "#187bcd")
                                  # ),
                                  
                                  # conditionalPanel(
                                  #   condition = "(input.database.includes('NHPRTR_CMM') || input.database.includes('All')) && input.geneName!='Select Gene'
                                  #               && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                  #   
                                  #   br(),
                                  #   h4("Monkey: NHPRTR - Cynomolgus Macaque Mauritian DB, RNAseq"),
                                  #   br(),
                                  #   plotOutput("graph_CMM", height=600) %>% withSpinner(color = "#187bcd")
                                  # ),
                                  
                                  # conditionalPanel(
                                  #   condition = "(input.database.includes('NHPRTR_RMI') || input.database.includes('All')) && input.geneName!='Select Gene'
                                  #               && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                  #   
                                  #   br(),
                                  #   h4("Monkey: NHPRTR - Rhesus Macaque Indian DB, RNAseq"),
                                  #   br(),
                                  #   plotOutput("graph_RMI", height=600) %>% withSpinner(color = "#187bcd")
                                  # ),
                                  
                                  # conditionalPanel(
                                  #   condition = "(input.database.includes('NHPRTR_JMI') || input.database.includes('All')) && input.geneName!='Select Gene'
                                  #               && (input.species.includes('Nonhuman primate') || input.species.includes('All'))",
                                  #   
                                  #   br(),
                                  #   h4("Monkey: NHPRTR - Japanese Macaque DB, RNAseq"),
                                  #   br(),
                                  #   plotOutput("graph_JMI", height=600) %>% withSpinner(color = "#187bcd")
                                  # )
                                  
                                  
                         ),
                         
                         
                         tabPanel("Canis familiaris",
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('DIS_Atlas_Dog') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Canis familiaris') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Dog: DIS Atlas DB, microarray"),
                                    br(),
                                    
                                    selectInput(inputId = 'probeDIS_Dog', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_DIS_Dog", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('BarkBase') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Canis familiaris') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Dog: BarkBase DB, RNAseq"),
                                    br(),
                                    
                                    plotOutput("graph_BarkBase", height=600) %>% withSpinner(color = "#187bcd")
                                  )
                                  
                         ),
                         
                         
                         tabPanel("Sus scrofa domesticus",
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('Minipig_Atlas') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Sus scrofa domesticus') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Sus scrofa domesticus: Minipig Atlas, RNAseq"),
                                    br(),
                                    plotOutput("graph_Minipig_Atlas", height=600) %>% withSpinner(color = "#187bcd")
                                  )
                                  
                         ),
                         
                         tabPanel("Rattus norvegicus",
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('DIS_Atlas_Rat') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Rattus norvegicus') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Rat: DIS Atlas DB, microarray"),
                                    br(),
                                    
                                    
                                    selectInput(inputId = 'probeDIS_Rat', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_DIS_Rat", height=600) %>% withSpinner(color = "#187bcd")
                                    
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('Ratlas') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Rattus norvegicus') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Rat: Ratlas, RNAseq"),
                                    br(),
                                    plotOutput("graph_Ratlas", height=600) %>% withSpinner(color = "#187bcd")
                                    
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('Rat_MTAB6081') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Rattus norvegicus') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Rattus norvegicus: MTAB6081, RNAseq"),
                                    br(),
                                    plotOutput("graph_Rat_MTAB6081", height=600) %>% withSpinner(color = "#187bcd")
                                  )
                         ),
                         
                         tabPanel("Mus musculus",
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('DIS_Atlas_Mouse') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Mus musculus') || input.species.includes('All'))",
                                    
                                    br(),
                                    
                                    h4("Mus musculus: DIS Atlas DB, microarray"),
                                    br(),
                                    
                                    selectInput(inputId = 'probeDIS_Mouse', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_DIS_Mouse", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('BioGPS_Mouse') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Mus musculus') || input.species.includes('All'))",
                                    
                                    br(),
                                    
                                    h4("Mus musculus: BioGPS - Mus musculus DB (GNF1M), microarray"),
                                    br(),
                                    
                                    selectInput(inputId = 'probeBioGPS_Mouse', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_BioGPS_Mouse", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('BioGPS_Mouse2') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Mus musculus') || input.species.includes('All'))",
                                    
                                    br(),
                                    
                                    h4("Mus musculus: BioGPS - Mus musculus DB (MOE403), microarray"),
                                    br(),
                                    selectInput(inputId = 'probeBioGPS_Mouse2', label = 'Select Probe', choices = "NULL"),
                                    
                                    plotOutput("graph_BioGPS_Mouse2", height=600) %>% withSpinner(color = "#187bcd")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "(input.database.includes('Mouse_MTAB6081') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Mus musculus') || input.species.includes('All'))",
                                    
                                    br(),
                                    h4("Mus musculus: MTAB6081, RNAseq"),
                                    br(),
                                    plotOutput("graph_Mouse_MTAB6081", height=600) %>% withSpinner(color = "#187bcd")
                                  )
                         )
                       )
                       
             )
           )
  ),
  
  
  tabPanel("Heatmap", value = "TEP_Heatmap", icon = ava::icon("braille"),
           
           fluidRow(                                                                 #In this row it is plotted the gene name and description, tissues selected and ZScore
             style = "background-color:#cce6ff;",
             column(1),
             column(4, 
                    h4("Selected Gene"), 
                    textOutput("gene_descr2")),
             # br()),
             column(5,
                    h4("Selected tissue system/s"),
                    textOutput("tissue2")
             ),
             
             column(2,                                                                       #Include selection of ZScore threshold for the heatmap plot
                    prettyRadioButtons('ZScore_cutoff', 'Select ZScore to filter:',
                                       choices = list(0,1,2,3),
                                       inline = TRUE,
                                       selected = 2,
                                       status = "primary")
             )
             
           ),
           
           br(),
           
           
           plotOutput("heatmap", height = "900px", width = "100%") %>% withSpinner(color = "#187bcd")
           #,
           # br(),
           # br(),
           # plotOutput("Tau")
           
  ),
  
  tabPanel("Summary", value = "TEP_Summary", icon = ava::icon("list-ol"),
           
           # h4("Selected parameters"),
           
           fluidRow(
             style = "background-color:#cce6ff;",
             column(1),
             column(4,
                    h4("Selected Gene"),
                    textOutput("gene_descr3")
             ),
             
             column(7,
                    h4("Selected tissue system/s"),
                    textOutput("tissue3")      
             )
           ),
           
           br(),
           
           h4("Database Summary"),
           br(),
           
           
           conditionalPanel(
             condition = 'input.geneName!="Select Gene"',
             
             dataTableOutput("tissueExp") %>% withSpinner(color = "#187bcd"),
             
             
             "*** zScore > 3   |   ** 3 > ZScore > 2   |   * 2 > ZScore > 1",
             br(),
             br(),
             
             h4("Summary by Tissues"),
             dataTableOutput("tissueSpecies2"),
             
             br(),
             br(),
             
             h4("Summary by Species"),
             dataTableOutput("speciesTissue2"),
             
             br(),
             br()
             
           )
  ),
  
  tabPanel("Data", value = "TEP_Data", icon = ava::icon("database"),
           
           fluidRow(
             style = "background-color:#cce6ff;",
             column(1),
             column(4,
                    h4("Selected Gene"),
                    textOutput("gene_descr4")
             ),
             
             column(7,
                    h4("Selected tissue system/s"),
                    textOutput("tissue4")
             )
           ),
           
           br(),
           
           tabsetPanel(
             
             tabPanel("Homo sapiens",
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("Human DIS Atlas"),
                                      br(),
                                      dataTableOutput("geneExp_Human_DIS_Atlas")),
                               
                               column(6, h4("GTEx Summary"),
                                      br(),
                                      dataTableOutput("geneExp_GTEx"))),
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("HPA Summary"),
                                      br(),
                                      dataTableOutput("geneExp_HPA")),
                               
                               column(6, h4("BioGPS: Human Summary"),
                                      br(),
                                      dataTableOutput("geneExp_BioGPS_Human")))
                      ),
             
             tabPanel("Nonhuman primate",
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("Monkey DIS Atlas"),
                                      br(),
                                      dataTableOutput("geneExp_Monkey_DIS_Atlas")),
                               
                               column(6, h4("NHPRTR: Macaques Summary"),
                                      br(),
                                      dataTableOutput("geneExp_MAC"))),
                      
                               
                      fluidRow(style = "background-color:#e6f3ff;",

                               column(6, h4("NHPRTR: CMC Summary"),
                                      br(),
                                      dataTableOutput("geneExp_CMC")),
                               
                               column(6, h4("NHPRTR: CMM Summary"),
                                      br(),
                                      dataTableOutput("geneExp_CMM"))),
                                
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("NHPRTR: RMI Summary"),
                                      br(),
                                      dataTableOutput("geneExp_RMI")),
                               
                               column(6, h4("NHPRTR: JMI Summary"),
                                      br(),
                                      dataTableOutput("geneExp_JMI")))
                      
             ),
             
             tabPanel("Canis familiaris",
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("Dog DIS Atlas"),
                                      br(),
                                      dataTableOutput("geneExp_Dog_DIS_Atlas")),
                               
                               column(6, h4("BarkBase Summary"),
                                      br(),
                                      dataTableOutput("geneExp_BarkBase")))

                      ),
                      
             tabPanel("Sus scrofa domesticus",
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("Minipig Atlas Summary"),
                                      br(),
                                      dataTableOutput("geneExp_Minipig_Atlas")))
                      
             ),
             
             tabPanel("Rattus norvegicus",
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6, h4("Rat DIS Atlas"),
                                      br(),
                                      dataTableOutput("geneExp_Rat_DIS_Atlas")),
                               
                               column(6, h4("Ratlas Summary"),
                                      br(),
                                      dataTableOutput("geneExp_Ratlas"))),
                      
                      fluidRow(style = "background-color:#e6f3ff;",

                               column(6, h4("Rat MTAB6081 Summary"),
                                      br(),
                                      dataTableOutput("geneExp_Rat_MTAB6081")))

             ),

             tabPanel("Mus musculus",
                      
                      fluidRow(style = "background-color:#e6f3ff;",

                               column(6,
                                      h4("Mouse DIS Atlas"),
                                      br(),
                                      dataTableOutput("geneExp_Mouse_DIS_Atlas")),
                               
                               column(6, h4("BioGPS: Mus musculus (GNF1M) Summary"),
                                      br(),
                                      dataTableOutput("geneExp_BioGPS_Mouse"))),
                      
                      fluidRow(style = "background-color:#e6f3ff;",
                               
                               column(6,
                                      h4("BioGPS: Mus musculus (MOE403) Summary"),
                                      br(),
                                      dataTableOutput("geneExp_BioGPS_Mouse2")),
                               
                               column(6,
                                      h4("Mouse MTAB6081 Summary"),
                                      br(),
                                      dataTableOutput("geneExp_Mouse_MTAB6081")))
                               
                      )
             )
           ),

  
  tabPanel("Methods", value = "TEP_Methods", icon = ava::icon("list-alt"),
           h4("Methods Summary"),
           
           tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2800225/", "Z-Score", target = "_blank"),
           
           withMathJax(),
           
           
           
           helpText('$$z_{score} =\\frac{x_i - \\mu}{\\sigma} \\hspace{1cm}  $$'),
           helpText('$$ \\mu \\hspace{0.1cm} is \\hspace{0.1cm} the \\hspace{0.1cm} mean \\hspace{0.1cm} of \\hspace{0.1cm} gene \\hspace{0.1cm} expression;\\hspace{0.1cm} \\sigma \\hspace{0.1cm} is \\hspace{0.1cm} the \\hspace{0.1cm} standard \\hspace{0.1cm} deviation$$'
           ),
           
           
           br(),
           
           "Tau statistic",
           
           helpText('$$\\tau =\\frac{\\sum_{i=1}^{n}(1-{\\hat{x_i}})}{n-1}  ; \\hspace{1cm}  \\hat{x_i} = \\frac{x_i}{max _{1\\leq i\\leq n}(x_i)}$$'),
           
           br(),
           
           tags$a(href = "https://academic.oup.com/bioinformatics/article/21/5/650/220059", "Tau tissue specificity index", target = "_blank"),
           
           br(),
           br(),
           
           h2("DATABASES"),
           br(),
           
           
           h3("DIS Atlas (Novartis)"), tags$a(href = 'http://compare.eu.novartis.net/atlas', 'The DIS Atlas ', target = '_blank'),
           
           "is a Novartis (PCS/DIS) database containing microarray gene data from human, monkey, mouse, rat, and dog normal tissues. 
    The data is MAS5 normalized. The human HG-U133_Plus_2 chip was used for the analysis of the Nonhuman primate transcript expression.",
           
           br(),   
           br(),
           
           h3("BarkBase"),
           
           tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6627511/", "BarkBase TMM Normalization", target = "_blank"),
           
           br(),
           
           "TMM Normalization to Calculate Counts Per Million (CPM)" ,
           br(),
           
           "The trimmed mean of M (TMM) normalization method as implemented in the R package edgeR was used to normalize raw RNA-seq read counts.
      TMM normalization helps avert underestimation of the abundance of lowly or moderately expressed genes in samples with very high expression
      from a subset of genes, thereby avoiding inflation of the number of genes inferred to be differentially expressed between samples. A TMM
      normalization factor was calculated for each sample, then applied to calculate counts per million (CPM) from raw read count. For most
      analyses, transcripts were kept if they were expressed at >0.16 counts per million (CPM) in two or more samples, which is equivalent
      to requiring approximately 10 reads in our median library size of 64 million [62]. For the comparison with human RNA-seq data, a more
      stringent cutoff of 1 CPM in two or more samples was implemented. Filtered, normalized CPM counts at both cutoffs are available
      at: https://data.broadinstitute.org/barkbase/.",
           
           br(),
           br(),
           
           h3("GTEx (The Genotype-Tissue Expression project)"),
           
           tags$a(href = "https://gtexportal.org/home/", "GTEx Database", target = "_blank"),
           br(),
           
           "The Genotype-Tissue Expression (GTEx) project is an ongoing effort to build a comprehensive public resource to study tissue-specific gene
    expression and regulation. Samples were collected from 54 non-diseased tissue sites across nearly 1000 individuals, primarily for molecular
    assays including WGS, WES, and RNA-Seq. Remaining samples are available from the GTEx Biobank. The GTEx Portal provides open access to data
    including gene expression, QTLs, and histology images.",
           br(),
           br(),
           
           h3("NHPRTR (Non-human primate reference transcriptome resource)"),
           
           tags$a(href = "https://academic.oup.com/nar/article/43/D1/D737/2437575", "NHPRTR Database", target =
                    "_blank"),
           br(),
           
           h4("Abstract"),
           
           "The non-human primate reference transcriptome resource (",
           tags$a(href = "http://nhprtr.org/", "NHPRTR", target = "_blank"),
           ")",
           " aims to generate comprehensive RNA-seq data from a wide variety of non-human primates (NHPs), from lemurs to hominids. In the 2012 Phase I of the NHPRTR project, 19
    billion fragments or 3.8 terabases of transcriptome sequences were collected from pools of ∼20 tissues in 15 species and subspecies.
    Here we describe a major expansion of NHPRTR by adding 10.1 billion fragments of tissue-specific RNA-seq data. For this effort, we
    selected 11 of the original 15 NHP species and subspecies and constructed total RNA libraries for the same ∼15 tissues in each. The
    sequence quality is such that 88% of the reads align to human reference sequences, allowing us to compute the full list of expression
    abundance across all tissues for each species, using the reads mapped to human genes. This update also includes improved transcript
    annotations derived from RNA-seq data for rhesus and cynomolgus macaques, two of the most commonly used NHP models and additional
    RNA-seq data compiled from related projects. Together, these comprehensive reference transcriptomes from multiple primates serve as a
    valuable community resource for genome annotation, gene dynamics and comparative functional analysis.",
           
           br(),
           br(),
           
           h3("BioGPS"),
           
           tags$a(href = "http://biogps.org/#goto=welcome", "BioGPS", target =
                    "_blank"),
           br(),
           
           h4("Abstract"),
           
           "BioGPS is a gene portal built with two guiding principles in mind -- customizability and extensibility." ,
           br(),
           
           "Online gene annotation resources are indispensable for analysis of genomics data. However, the landscape of these online resources is
    highly fragmented, and scientists often visit dozens of these sites for each gene in a candidate gene list. Here, we introduce BioGPS ",
           tags$a(href = "http://biogps.org", "http://biogps.org", target = "_blank"),
           " a centralized gene portal for aggregating distributed gene annotation resources. Moreover, BioGPS embraces the
    principle of community intelligence, enabling any user to easily and directly contribute to the BioGPS platform.",
           "http://amp.pharm.mssm.edu/Harmonizome/dataset/BioGPS+Mouse+Cell+Type+and+Tissue+Gene+Expression+Profiles",
           br(),
           tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE1133", "Mouse GNF1M Gene Atlas", target =
                    "_blank"),
           
           h3("Ratlas"),
           br(),
           h3("Minipig Atlas"),
           br(),
           "For tissues where several transcripts were available, those with highest mean on the transcript have been selected.",
           br(),
           
           h3("Mouse/Rat MTAB6081"),
           br(),
           
           
           tags$a(href = "https://www.nature.com/articles/sdata2017185", "Mouse/Rat MTAB6081 webpage link", target =
                    "_blank"),
           br(),
           
           h4("Abstract"),
           
           "Gene functionality is closely connected to its expression specificity across tissues and cell types. RNA-Seq is a powerful 
    quantitative tool to explore genome wide expression. The aim of this study is to provide a comprehensive RNA-Seq dataset across 
    the same 13 tissues for mouse and rat, two of the most relevant species for biomedical research. The dataset provides the 
    transcriptome across tissues from three male C57BL6 mice and three male Han Wistar rats. They also describe their bioinformatics 
    pipeline to process and technically validate the data. Principal component analysis shows that tissue samples from both species 
    cluster similarly. They show by comparative genomics that many genes with high sequence identity with respect to their human 
    orthologues also have a highly correlated tissue distribution profile and are in agreement with manually curated literature 
    data for human. In summary, the present study provides a unique resource for comparative genomics and will facilitate the 
    analysis of tissue specificity and cross-species conservation in higher organisms."
  ),
  
  
  tabPanel("Version history", value = "TEP_Version_history", "Version history", icon = ava::icon("slack-hash"),
           
    br(),
    h2("Beta 0.9 August '20"),
    br(),
    
    withTags(
      ul(
        li("DIS Atlas Human database included"),
        li("ZScore filter available in Heatmap"),
        li("NHPRTR databases merged in one. Posibility of displaying only one DB"),
        li("Downloadable report adapted to Novartis style")
        
      )),    
    
    
    br(),
    h2("Beta 0.8 July '20"),
    br(),
    
    withTags(
      ul(
        li("Four new DIS Atlas databases included for Monkey, Dog, Rat and Mouse"),
        li("Plots grouped in tabs by species"),
        li("Logaritmic scale available for plots")
      )),
    
    br(),
    h2("Beta 0.71 July '20"),
    br(),
    
    "Two new databases included for Mouse and Rat:",
    
    withTags(
      ul(
        li("Mouse MTAB6081"),
        li("Rat MTAB6081")
      )),
    
    
    
    
    
    h2("Beta 0.7 June '20"),
    br(),
    "First version of the app, including the following databases: ",
    
    withTags(
      ul(
        li("GTEx v8"),
        li("Human Protein Atlas v19.3"),
        li("BioGPS"),
        ul(
          li("Human"),
          li("Monkey (chip GNF1M)"),
          li("Monkey (chip MOE403)")
        ),
        li("BarkBase vJune '19"),
        li("NonHuman Primate Reference Transcriptome Resource vSept '14"),
        ul(
          li("Cynomolgus Macaque Chinese"),
          li("Cynomolgus Macaque Mauritian"),
          li("Rhesus Macaque Indian")
        ),
        li("Ratlas"),
        li("Minipig Atlas")
        
        
      )
    )
    

  ), 
  
  tabPanel("Session Info", value = "TEP_Session_Info", icon = ava::icon("info-square"),
           mainPanel(verbatimTextOutput("sessionInfo"))
           
  ),
  
  tabPanel("Home SC",  value = "SC_Home", icon = ava::icon("home")
           
  ),
  
  tabPanel("Single Cell",  value = "SC", icon = icon("microscope")
  ),
  
  tabPanel("UMAP",  value = "SC_UMAP", icon = ava::icon("disease")
  ),
  
  tabPanel("Data",  value = "SC_Data", icon = ava::icon("database")
  )
  # ,
  # 
  # deployVersion = TEP_Version,
  # appName = "TEP",
  # appDesc = "Target Expression Profiling",
  # author = "Carlos Sanchez",
  # email = "carlos.sanchez_roldan@novartis.com",
  # loading = T

)






########################################################################################################################

###############################################    SERVER    ###########################################################

########################################################################################################################




# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observeEvent(input$Type, {
    
    selected <- input$Type
    
    if (selected=="Select"){
      hideTab(inputId = "tabs", target = "TEP_Home")
      hideTab(inputId = "tabs", target = "TEP_TEP")
      hideTab(inputId = "tabs", target = "TEP_Heatmap")
      hideTab(inputId = "tabs", target = "TEP_Summary")
      hideTab(inputId = "tabs", target = "TEP_Data")
      hideTab(inputId = "tabs", target = "TEP_Methods")
      hideTab(inputId = "tabs", target = "TEP_Version_history")
      hideTab(inputId = "tabs", target = "TEP_Session_Info")
      hideTab(inputId = "tabs", target = "SC_Home")
      hideTab(inputId = "tabs", target = "SC")
      hideTab(inputId = "tabs", target = "SC_UMAP")
      hideTab(inputId = "tabs", target = "SC_Data")
      
    } else if (selected == "Single_Cell"){
      
      hideTab(inputId = "tabs", target = "TEP_Home")
      hideTab(inputId = "tabs", target = "TEP_TEP")
      hideTab(inputId = "tabs", target = "TEP_Heatmap")
      hideTab(inputId = "tabs", target = "TEP_Summary")
      hideTab(inputId = "tabs", target = "TEP_Data")
      hideTab(inputId = "tabs", target = "TEP_Methods")
      hideTab(inputId = "tabs", target = "TEP_Version_history")
      hideTab(inputId = "tabs", target = "TEP_Session_Info")
      showTab(inputId = "tabs", target = "SC_Home")
      showTab(inputId = "tabs", target = "SC")
      showTab(inputId = "tabs", target = "SC_UMAP")
      showTab(inputId = "tabs", target = "SC_Data")
      
      
    } else {
      
      showTab(inputId = "tabs", target = "TEP_Home")
      showTab(inputId = "tabs", target = "TEP_TEP")
      showTab(inputId = "tabs", target = "TEP_Heatmap")
      showTab(inputId = "tabs", target = "TEP_Summary")
      showTab(inputId = "tabs", target = "TEP_Data")
      showTab(inputId = "tabs", target = "TEP_Methods")
      showTab(inputId = "tabs", target = "TEP_Version_history")
      showTab(inputId = "tabs", target = "TEP_Session_Info")
      hideTab(inputId = "tabs", target = "SC_Home")
      hideTab(inputId = "tabs", target = "SC")
      hideTab(inputId = "tabs", target = "SC_UMAP")
      hideTab(inputId = "tabs", target = "SC_Data")
      
      
    }
    
    
  })
  
  # input <- data.frame(grouped_tissue="Tissue") Data sample (tests of data filtering)
  # genIDs <- "APOE"
  # Moved the gene list for selectInput to avoid long loading times and breaks. Issues reported from US now solved with this option
  
  observe({
    updateSelectizeInput(session = session, inputId = 'geneName', choices = c("Select Gene", sort(geneID$SYMBOL)), options = list(maxOptions = 100), server = TRUE)
  })
  
  
  gene_description_reactive <- eventReactive(input$Submit, {   #Include gene input into a eventReactive to avoid changing gene names on texts and tables without "Submit"
    gene_description_reactive <- input$geneName
  })
  
  #Gene translation to homologs   asd
  
  translation_gene <- function(specie){
    gene <- human_all_organism[human_all_organism$HomoloGene.ID==unique(human_all_organism$HomoloGene.ID[human_all_organism$Symbol==gene_description_reactive()]),]
    genIDs <- gene$Symbol[gene$Common.Organism.Name==specie]
    genIDs <- ifelse (length(genIDs)!=0, genIDs, get(paste0('geneID_', specie))$SYMBOL[get(paste0('geneID_', specie))$HGNC.symbol==gene_description_reactive()])
    # genIDs <- ifelse (is.na(genIDs), input$geneName, genIDs)
    return(genIDs)
  }
  
  
  #Code that will be used by the different plots to display individual or grouped tissues 
  
  E_gene <- function(x){
    
    remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")  #Select individual tissue or grouped tissue (upper level. e.g. "brain")
    keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
    x <- x[-which(colnames(x) == remove_column)]
    colnames(x)[which(colnames(x) == keep_column)] <- "groupTissue"
    return(x)      }
  
  
  
  #####CASE WHEN GTEx_final HAS NO ROWS#########
  
  E_gene1 <- eventReactive(input$Submit, {
    
    genIDs <- toupper(input$geneName)
    ensembl <- geneID$ENSEMBL[geneID$SYMBOL == genIDs]
    
    GTEx_final <- GTEx_v8 %>%
      filter(rownames(GTEx_v8) %in% ensembl)
    
    if (dim(GTEx_final)[1] == 0) {
      E_gene1 <- data.frame(tpm = numeric(0), tissue = character(0))
      
    } else {
      #Select the row with highest expression mean in case there are several register
      
      id <- which.max(rowMeans(GTEx_final))
      GTEx_final <- as.data.frame(t(GTEx_final[id, ]))
      
      
      
      # as.data.frame(GTEx_v8[grep(paste0("^", ensembl, ""), rownames(GTEx_v8)), ])
      
      row.names(GTEx_final) <- gsub("\\.", "-", row.names(GTEx_final))
      GTEx_final$SAMPID <- row.names(GTEx_final)
      E_gene1 <- merge(GTEx_final, samp, by = 'SAMPID')
      E_gene1 <- E_gene1[, c(colnames(E_gene1)[2], "SMTSD")]
      colnames(E_gene1) <- c("tpm", "tissue")
      
      E_gene1 <- merge(E_gene1,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "GTEx.Carlos")
      
      
      E_gene1 <- dplyr::select(E_gene1, tpm, Master, Tissue_type, level1)
      
      E_gene(E_gene1)      #Replaced logic for selecting grouped or individual tissues (below) for a function used on every DB
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")  #Select individual tissue or grouped tissue (upper level. e.g. "brain")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # 
      # E_gene1 <- E_gene1[-which(colnames(E_gene1) == remove_column)]
      # colnames(E_gene1)[which(colnames(E_gene1) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene1)
      
    }
  })
  
  # browser()
  #HPA DB:
  
  #ensembl <- "ENSG00000130203"
  #genIDs <- "APOE"
  E_gene2 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    ensembl <- geneID$ENSEMBL[geneID$SYMBOL == genIDs]
    
    HPA_DB <- HPA_v19_3 %>%
      filter(ensgid %in% ensembl) %>%
      dplyr::select(-enstid)
    # filter(rowSums(.[c(-1, -2)]) > 1)
    
    ##Select all tissues
    
    if (dim(HPA_DB)[1] == 0) {
      
      E_gene2 <- data.frame(tpm = numeric(0), tissue = character(0))
      
    } else {
      
      HPA_DB <- HPA_DB %>% group_by(ensgid) %>% summarise_all(list(sum))
      idx <- which.max(apply(HPA_DB[, 2:ncol(HPA_DB)], 1, mean))
      HPA_DB <- HPA_DB[idx, -1]
      
      
      colnames(HPA_DB) <- gsub('(\\.V).*|(\\.a$)|(\\.b$)|(\\.c$)|(\\.d$)|(\\.e$)|(\\.f$)', "", colnames(HPA_DB))
      
      E_gene2 <- setNames(data.frame(t(HPA_DB), grp = colnames(HPA_DB)), c("tpm", "tissue"))
      rownames(E_gene2) <- NULL
      E_gene2 <- merge(E_gene2,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "HPA")
      
      # validate(
      #   need(input$tissue, message = 'Please, select a Tissue System or "All"')
      # )
      #
      # if("All" %in% input$tissue){
      #
      #   E_gene2 <- dplyr::select(E_gene2, tpm, Master, Tissue_type, level1)
      #
      # } else {
      
      E_gene2 <- dplyr::select(E_gene2, tpm, Master, Tissue_type, level1)
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene2 <- E_gene2[-which(colnames(E_gene2) == remove_column)]
      # colnames(E_gene2)[which(colnames(E_gene2) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene2)
      
      E_gene(E_gene2)
      
    }
  })
  
  
  ##BarkBase DB
  
  #genIDs <- "A1BG-AS1"
  E_gene3 <- eventReactive(input$Submit, {
    genIDs <- input$geneName    ###Not converting to homolog since gene names are the human version
    
    # if (genIDs %in% BarkBase$GeneSymbol){
    
    BB_gene <- BarkBase %>%
      filter(GeneSymbol == genIDs) %>%
      dplyr::select(-c(EnsemblGeneID, GeneSymbol))
    
    colnames(BB_gene) <- gsub(pattern = ".*_", replacement = "", colnames(BB_gene))
    #    E_gene3 <- setNames(data.frame(t(BB_gene), grp = colnames(BB_gene)), c("RPKM", "tissue"))   #to be used when calculation RPKM
    
    if (dim(BB_gene)[1] == 0) {
      E_gene3 <- data.frame(CPM = numeric(0), tissue = character(0))
      
    } else {
      E_gene3 <- setNames(data.frame(t(BB_gene), grp = colnames(BB_gene)), c("CPM", "tissue"))
      
      rownames(E_gene3) <- NULL
      
      E_gene3 <- merge(E_gene3,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "BarkBase")
      
      E_gene3 <- dplyr::select(E_gene3, CPM, Master, Tissue_type, level1)
      
      E_gene(E_gene3)
      
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene3 <- E_gene3[-which(colnames(E_gene3) == remove_column)]
      # colnames(E_gene3)[which(colnames(E_gene3) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene3)
      
    }
    
  })
  
  #NHPRTR CYNOMOLGUS MACAQUE CHINESE:
  
  #genIDs <- "APOE"
  E_gene4 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    NHPRTR_CMC2 <- NHPRTR_CMC %>%
      filter(Gene == genIDs) %>%
      dplyr::select(-c(Gene))
    
    colnames(NHPRTR_CMC2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_CMC2))
    
    if (dim(NHPRTR_CMC2)[1] == 0) {
      E_gene4 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      E_gene4 <- setNames(data.frame(t(NHPRTR_CMC2), grp = colnames(NHPRTR_CMC2)), c("FPKM", "tissue"))
      
      
      rownames(E_gene4) <- NULL
      E_gene4 <- merge(E_gene4,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "NHPRTR")
      
      E_gene4 <- dplyr::select(E_gene4, FPKM, Master, Tissue_type, level1)
      
      E_gene(E_gene4)
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene4 <- E_gene4[-which(colnames(E_gene4) == remove_column)]
      # colnames(E_gene4)[which(colnames(E_gene4) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene4)
      # 
      
    }
  })
  
  
  #NHPRTR CYNOMOLGUS MACAQUE MAURITIAN:
  
  #genIDs <- "APOE"
  E_gene5 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    NHPRTR_CMM2 <- NHPRTR_CMM %>%
      filter(Gene == genIDs) %>%
      dplyr::select(-c(Gene))
    
    colnames(NHPRTR_CMM2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_CMM2))
    
    if (dim(NHPRTR_CMM2)[1] == 0) {
      E_gene5 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      E_gene5 <- setNames(data.frame(t(NHPRTR_CMM2), grp = colnames(NHPRTR_CMM2)), c("FPKM", "tissue"))
      
      
      rownames(E_gene5) <- NULL
      E_gene5 <- merge(E_gene5,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "NHPRTR")
      
      E_gene5 <- dplyr::select(E_gene5, FPKM, Master, Tissue_type, level1)
      
      E_gene(E_gene5)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene5 <- E_gene5[-which(colnames(E_gene5) == remove_column)]
      # colnames(E_gene5)[which(colnames(E_gene5) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene5)
      
      
    }
    
  })
  
  
  #NHPRTR RHESUS MACAQUE INDIAN:
  
  #genIDs <- "APOE"
  E_gene6 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    NHPRTR_RMI2 <- NHPRTR_RMI %>%
      filter(Gene == genIDs) %>%
      dplyr::select(-c(Gene))
    
    colnames(NHPRTR_RMI2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_RMI2))
    
    if (dim(NHPRTR_RMI2)[1] == 0) {
      E_gene6 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      E_gene6 <-setNames(data.frame(t(NHPRTR_RMI2), grp = colnames(NHPRTR_RMI2)), c("FPKM", "tissue"))
      
      
      rownames(E_gene6) <- NULL
      E_gene6 <- merge(E_gene6,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "NHPRTR")
      
      E_gene6 <- dplyr::select(E_gene6, FPKM, Master, Tissue_type, level1)
      
      E_gene(E_gene6)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene6 <- E_gene6[-which(colnames(E_gene6) == remove_column)]
      # colnames(E_gene6)[which(colnames(E_gene6) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene6)
      
    }
    
  })
  
  
  
  #BioGPS - MOUSE (GNF1M):
  
  #genIDs <- "HYDIN"
  #genIDs <- "APOE"
  
  #Filter the database based on selected gene
  
  BioGPS2M <- eventReactive(input$Submit, {
    
    genIDs <- translation_gene("mouse")
    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    
    
    BioGPS2M  <- BioGPS_M %>%
      filter(Symbol == genIDs) %>%
      dplyr::select(-c(2:4)) %>%
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
    # BioGPS2M <- BioGPS2M[which(apply(BioGPS2M[,2:length(BioGPS2M)],1,mean)>input$microarray_cutoff),]
    
    
  })
  
  
  #Update the probes names on SelectInput
  
  observe({
    x <- BioGPS2M()$ProbesetID
    #I have removed "session" before "probeBioGPS"
    
    updateSelectInput(session, inputId = "probeBioGPS_Mouse", label = "Select Probe",
                      choices = x,
                      selected = BioGPS2M()$ProbesetID[which.max(apply(BioGPS2M()[, 2:148], 1, mean))])
  })
  
  # probes <- reactive(BioGPS2()[,1])
  
  E_gene7 <- reactive({
    # idx <-which.max(apply(BioGPS2[,2:148], 1, mean))
    
    BioGPS2M_subset <-
      BioGPS2M()[BioGPS2M()$ProbesetID == input$probeBioGPS_Mouse, ]
    
    # colnames(BioGPS2_subset) <- gsub(pattern = "(\\.1)", replacement = "", colnames(BioGPS2_subset))
    
    if (dim(BioGPS2M_subset)[1] == 0) {
      E_gene7 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene7 <-
        setNames(data.frame(t(BioGPS2M_subset[, 2:148]), grp = colnames(BioGPS2M_subset[, 2:148])), c("Signal", "tissue"))
      
      rownames(E_gene7) <- NULL
      E_gene7$tissue <- gsub(pattern = "(\\.1)", replacement = "", E_gene7$tissue)
      
      E_gene7 <- merge(E_gene7,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "BioGPS_Mouse")
      
      E_gene7 <- dplyr::select(E_gene7, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene7)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene7 <- E_gene7[-which(colnames(E_gene7) == remove_column)]
      # colnames(E_gene7)[which(colnames(E_gene7) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene7)
      
    }
  })
  
  
  
  #BioGPS - MOUSE2 (MOE403):
  
  #genIDs <- "APOE"
  
  #Filter the database based on selected gene
  
  BioGPS2M2 <- eventReactive(input$Submit, {
    genIDs <- translation_gene("mouse")
    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    
    BioGPS2M2  <- BioGPS_M2 %>%
      filter(Gene.Symbol == genIDs) %>%
      dplyr::select(-c(2)) %>%
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
  })
  
  # BioGPS2M2 <- BioGPS2M2[which(apply(BioGPS2M2[,2:length(BioGPS2M2)],1,mean)>input$microarray_cutoff),]
  
  
  
  
  #Update the probes names on SelectInput
  
  observe({
    x <- BioGPS2M2()$Probe.Set.ID
    #I have removed "session" before "probeBioGPS"
    
    updateSelectInput(session, inputId = "probeBioGPS_Mouse2", label = "Select Probe",
                      choices = x,
                      selected = BioGPS2M2()$Probe.Set.ID[which.max(apply(BioGPS2M2()[, 2:190], 1, mean))]
    )
  })
  
  # probes <- reactive(BioGPS2()[,1])
  
  E_gene8 <- reactive({
    # idx <-which.max(apply(BioGPS2[,2:148], 1, mean))
    
    BioGPS2M2_subset <-
      BioGPS2M2()[BioGPS2M2()$Probe.Set.ID == input$probeBioGPS_Mouse2, ]
    
    # colnames(BioGPS2_subset) <- gsub(pattern = "(\\.1)", replacement = "", colnames(BioGPS2_subset))
    
    if (dim(BioGPS2M2_subset)[1] == 0) {
      E_gene8 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene8 <- setNames(data.frame(t(BioGPS2M2_subset[, 2:190]), grp = colnames(BioGPS2M2_subset[, 2:190])), c("Signal", "tissue"))
      
      rownames(E_gene8) <- NULL
      E_gene8$tissue <- gsub(pattern = '(\\.1)|(\\.)|(\\..1).*', replacement = "", E_gene8$tissue)
      
      E_gene8 <- merge(E_gene8,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "BioGPS_Mouse2")
      
      E_gene8 <- dplyr::select(E_gene8, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene8)
      
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene8 <- E_gene8[-which(colnames(E_gene8) == remove_column)]
      # colnames(E_gene8)[which(colnames(E_gene8) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene8)
      
      #   %>%
      #     filter(Tissue_type %in% input$tissue)
      #
      # }
    }
  })
  
  
  #BioGPS - HUMAN:
  
  #genIDs <- "ITGB3"
  #genIDs <- "APOE"
  
  #Filter the database based on selected gene
  
  BioGPS2H <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    BioGPS2H  <- BioGPS_H %>%
      filter(toupper(Gene.Symbol) == genIDs) %>%
      dplyr::select(-c(2:4)) %>%
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
    # BioGPS2H <- BioGPS2H[which(apply(BioGPS2H[,2:length(BioGPS2H)],1,mean)>input$microarray_cutoff),]
    
    #  BioGPS2$ProbesetID <- as.character(BioGPS2$ProbesetID)
  })
  
  
  #Update the probes names on SelectInput
  
  observe({
    # x <- BioGPS2() %>% dplyr::select(ProbesetID)
    x <- BioGPS2H()$Probe.Set.ID
    #I have removed "session" before "probeBioGPS"
    
    updateSelectInput(session, inputId = "probeBioGPS_Human", label = "Select Probe",
                      choices = x,
                      selected = BioGPS2H()$Probe.Set.ID[which.max(apply(BioGPS2H()[, 2:175], 1, mean))]
    )
  })
  
  # probes <- reactive(BioGPS2H()[,1])
  
  E_gene9 <- reactive({
    # idx <-which.max(apply(BioGPS2H[,2:148], 1, mean))
    
    BioGPS2H_subset <-
      BioGPS2H()[BioGPS2H()$Probe.Set.ID ==	input$probeBioGPS_Human, ]
    
    # colnames(BioGPS2H_subset) <- gsub(pattern = "(\\.1)", replacement = "", colnames(BioGPS2H_subset))
    
    if (dim(BioGPS2H_subset)[1] == 0) {
      E_gene9 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene9 <- setNames(data.frame(t(BioGPS2H_subset[, 2:175]), grp = colnames(BioGPS2H_subset[, 2:175])), c("Signal", "tissue"))
      
      rownames(E_gene9) <- NULL
      E_gene9$tissue <- gsub(pattern = "(\\.).*", replacement = "", E_gene9$tissue)
      E_gene9 <- merge(E_gene9,
                       Atlas_Dictionary_final,
                       by.x = "tissue",
                       by.y = "BioGPS_Human")
      
      E_gene9 <- dplyr::select(E_gene9, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene9)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene9 <- E_gene9[-which(colnames(E_gene9) == remove_column)]
      # colnames(E_gene9)[which(colnames(E_gene9) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene9)
      
    }
    
  })
  
  # Ratlas
  
  #genIDs <- "APLP2"
  E_gene10 <- eventReactive(input$Submit, {
    genIDs <- translation_gene("rat")
    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    
    E_gene10 <- Ratlas %>%
      filter(gene_name == genIDs) %>%
      dplyr::select(c(fpkm, organ))
    
    
    
    if (dim(Ratlas)[1] == 0) {
      E_gene10 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      colnames(E_gene10) <- c("FPKM", "tissue")
      
      E_gene10 <- merge(E_gene10,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "Ratlas")
      
      E_gene10 <- dplyr::select(E_gene10, FPKM, Master, Tissue_type, level1)
      
      E_gene(E_gene10)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene10 <- E_gene10[-which(colnames(E_gene10) == remove_column)]
      # colnames(E_gene10)[which(colnames(E_gene10) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene10)
    }
  })
  
  
  # Minipig Atlas
  
  #genIDs <- "APOE"
  E_gene11 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    E_gene11 <- Minipig_Atlas %>%
      filter(toupper(gene_name) == genIDs) %>%
      group_by(organ,transcript) %>%
      mutate(D = mean(tpm)) %>%    #For tissues where several transcripts were available, those with highest mean on the transcript have been selected.
      group_by(organ) %>%
      filter(D==max(D)) %>%
      dplyr::select(c(tpm, organ))
    
    
    if (dim(Minipig_Atlas)[1] == 0) {
      E_gene11 <- data.frame(tpm = numeric(0), tissue = character(0))
      
    } else {
      
      colnames(E_gene11) <- c("tpm", "tissue")
      E_gene11 <- merge(E_gene11,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "Minipig_Atlas")
      
      E_gene11 <- dplyr::select(E_gene11, tpm, Master, Tissue_type, level1)
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene11 <- E_gene11[-which(colnames(E_gene11) == remove_column)]
      # colnames(E_gene11)[which(colnames(E_gene11) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene11)
      E_gene(E_gene11)
      
    }
  })
  
  
  # Mouse MTAB6081
  
  #genIDs <- "APOE"
  
  
  
  E_gene12 <- eventReactive(input$Submit, {
    genIDs <- translation_gene("mouse")

    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    ensembl <- geneID_mouse$ENSEMBL[geneID_mouse$SYMBOL == genIDs]
    
    E_gene12 <- Mouse_MTAB6081[row.names(Mouse_MTAB6081)==ensembl,]
    
    if (dim(E_gene12)[1] == 0) {
      
      E_gene12 <- data.frame(TPM = numeric(0), tissue = character(0))
      
    } else {
      
      E_gene12 <- data.frame(t(E_gene12))
      
      E_gene12$tissue <- row.names(E_gene12)
      row.names(E_gene12) <- NULL
      
      E_gene12$tissue <- gsub('.*_', '', E_gene12$tissue)
      colnames(E_gene12) <- c("TPM", "tissue")
      
      E_gene12 <- merge(E_gene12,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "Mouse_MTAB6081")
      
      E_gene12 <- dplyr::select(E_gene12, TPM, Master, Tissue_type, level1)
      
      E_gene(E_gene12)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene12 <- E_gene12[-which(colnames(E_gene12) == remove_column)]
      # colnames(E_gene12)[which(colnames(E_gene12) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene12)
    }
  })
  
  
  # Rat MTAB6081
  
  E_gene13 <- eventReactive(input$Submit, {
    
    genIDs <- translation_gene("rat")
    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    ensembl <- geneID_rat$ENSEMBL[geneID_rat$SYMBOL == genIDs]
    
    E_gene13 <- Rat_MTAB6081[row.names(Rat_MTAB6081)==ensembl,]
    
    if (dim(E_gene13)[1] == 0) {
      
      E_gene13 <- data.frame(TPM = numeric(0), tissue = character(0))
      
    } else {
      
      E_gene13 <- data.frame(t(E_gene13))
      
      E_gene13$tissue <- row.names(E_gene13)
      row.names(E_gene13) <- NULL
      
      
      E_gene13$tissue <- gsub('.*_', '', E_gene13$tissue)
      colnames(E_gene13) <- c("TPM", "tissue")
      
      
      E_gene13 <- merge(E_gene13,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "Rat_MTAB6081")
      
      E_gene13 <- dplyr::select(E_gene13, TPM, Master, Tissue_type, level1)
      
      E_gene(E_gene13)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene13 <- E_gene13[-which(colnames(E_gene13) == remove_column)]
      # colnames(E_gene13)[which(colnames(E_gene13) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene13)
    }
  })  
  
  
  # Monkey DIS Atlas
  
  DIS_Atlas_Monkey2 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    DIS_Atlas_Monkey2  <- DIS_Atlas_Monkey[toupper(DIS_Atlas_Monkey$Gene.Symbol) == genIDs,]
    DIS_Atlas_Monkey2 <- DIS_Atlas_Monkey2[-c(2:3)] %>% 
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
  })
  
  
  #Update the probes names on SelectInput
  
  observe({
    x <- DIS_Atlas_Monkey2()$Probe.Set.ID
    #I have removed "session" before "probeBioGPS"
    
    updateSelectInput(session, inputId = "probeDIS_Monkey", label = "Select Probe",
                      choices = x,
                      selected = DIS_Atlas_Monkey2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Monkey2()[, 2:length(DIS_Atlas_Monkey2())], 1, mean))]
    )
  })
  
  # probes <- reactive(BioGPS2()[,1])
  
  E_gene14 <- reactive({
    
    DIS_Atlas_Monkey2_subset <-
      DIS_Atlas_Monkey2()[DIS_Atlas_Monkey2()$Probe.Set.ID == input$probeDIS_Monkey, ]
    
    if (dim(DIS_Atlas_Monkey2_subset)[1] == 0) {
      E_gene14 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene14 <-
        setNames(data.frame(t(DIS_Atlas_Monkey2_subset[, 2:length(DIS_Atlas_Monkey2())]), grp = colnames(DIS_Atlas_Monkey2_subset[, 2:length(DIS_Atlas_Monkey2())])), c("Signal", "tissue"))
      
      rownames(E_gene14) <- NULL
      E_gene14$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene14$tissue)
      
      E_gene14 <- merge(E_gene14,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "DIS_Atlas2")
      
      E_gene14 <- dplyr::select(E_gene14, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene14)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene14 <- E_gene14[-which(colnames(E_gene14) == remove_column)]
      # colnames(E_gene14)[which(colnames(E_gene14) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene14)
      
    }
  })
  
  
  # Dog DIS Atlas
  
  DIS_Atlas_Dog2 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    DIS_Atlas_Dog2  <- DIS_Atlas_Dog[toupper(DIS_Atlas_Dog$Gene.Symbol) == genIDs,]
    DIS_Atlas_Dog2 <- DIS_Atlas_Dog2[-c(2:3)] %>% 
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
  })
  
  
  observe({
    x <- DIS_Atlas_Dog2()$Probe.Set.ID
    
    updateSelectInput(session, inputId = "probeDIS_Dog", label = "Select Probe",
                      choices = x,
                      selected = DIS_Atlas_Dog2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Dog2()[, 2:length(DIS_Atlas_Dog2())], 1, mean))]
    )
  })
  
  
  E_gene15 <- reactive({
    
    DIS_Atlas_Dog2_subset <- DIS_Atlas_Dog2()[DIS_Atlas_Dog2()$Probe.Set.ID == input$probeDIS_Dog, ]
    
    if (dim(DIS_Atlas_Dog2_subset)[1] == 0) {
      E_gene15 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene15 <- setNames(data.frame(t(DIS_Atlas_Dog2_subset[, 2:length(DIS_Atlas_Dog2())]), grp = colnames(DIS_Atlas_Dog2_subset[, 2:length(DIS_Atlas_Dog2())])), c("Signal", "tissue"))
      
      rownames(E_gene15) <- NULL
      E_gene15$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene15$tissue)
      
      E_gene15 <- merge(E_gene15,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "DIS_Atlas2")
      
      E_gene15 <- dplyr::select(E_gene15, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene15)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene15 <- E_gene15[-which(colnames(E_gene15) == remove_column)]
      # colnames(E_gene15)[which(colnames(E_gene15) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene15)
      
    }
  })
  
  # Rat DIS Atlas
  
  DIS_Atlas_Rat2 <- eventReactive(input$Submit, {
    genIDs <- translation_gene("rat")

    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    DIS_Atlas_Rat2  <- DIS_Atlas_Rat[DIS_Atlas_Rat$Gene.Symbol == genIDs,]
    DIS_Atlas_Rat2 <- DIS_Atlas_Rat2[-c(2:3)] %>% 
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)

  })
  
  
  observe({
    x <- DIS_Atlas_Rat2()$Probe.Set.ID
    
    updateSelectInput(session, inputId = "probeDIS_Rat", label = "Select Probe",
                      choices = x,
                      selected = DIS_Atlas_Rat2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Rat2()[, 2:length(DIS_Atlas_Rat2())], 1, mean))]
    )
  })
  
  
  E_gene16 <- reactive({
    
    DIS_Atlas_Rat2_subset <- DIS_Atlas_Rat2()[DIS_Atlas_Rat2()$Probe.Set.ID == input$probeDIS_Rat, ]
    
    if (dim(DIS_Atlas_Rat2_subset)[1] == 0) {
      E_gene16 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene16 <- setNames(data.frame(t(DIS_Atlas_Rat2_subset[, 2:length(DIS_Atlas_Rat2())]), grp = colnames(DIS_Atlas_Rat2_subset[, 2:length(DIS_Atlas_Rat2())])), c("Signal", "tissue"))
      
      rownames(E_gene16) <- NULL
      E_gene16$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene16$tissue)
      
      E_gene16 <- merge(E_gene16,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "DIS_Atlas2")
      
      E_gene16 <- dplyr::select(E_gene16, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene16)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene16 <- E_gene16[-which(colnames(E_gene16) == remove_column)]
      # colnames(E_gene16)[which(colnames(E_gene16) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene16)
      
    }
  })
  
  
  # Mouse DIS Atlas
  
  DIS_Atlas_Mouse2 <- eventReactive(input$Submit, {
    genIDs <- translation_gene("mouse")

    genIDs <- ifelse (length(genIDs)==0 | is.na(genIDs), gene_description_reactive(), genIDs)
    
    DIS_Atlas_Mouse2  <- DIS_Atlas_Mouse[DIS_Atlas_Mouse$Gene.Symbol == genIDs,]
    DIS_Atlas_Mouse2 <- DIS_Atlas_Mouse2[-c(2:3)] %>% 
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
  })
  
  
  observe({
    x <- DIS_Atlas_Mouse2()$Probe.Set.ID
    
    updateSelectInput(session, inputId = "probeDIS_Mouse", label = "Select Probe",
                      choices = x,
                      selected = DIS_Atlas_Mouse2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Mouse2()[, 2:length(DIS_Atlas_Mouse2())], 1, mean))]
    )
  })
  
  
  E_gene17 <- reactive({
    
    DIS_Atlas_Mouse2_subset <- DIS_Atlas_Mouse2()[DIS_Atlas_Mouse2()$Probe.Set.ID == input$probeDIS_Mouse, ]
    
    if (dim(DIS_Atlas_Mouse2_subset)[1] == 0) {
      E_gene17 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene17 <- setNames(data.frame(t(DIS_Atlas_Mouse2_subset[, 2:length(DIS_Atlas_Mouse2())]), grp = colnames(DIS_Atlas_Mouse2_subset[, 2:length(DIS_Atlas_Mouse2())])), c("Signal", "tissue"))
      
      rownames(E_gene17) <- NULL
      E_gene17$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene17$tissue)
      
      E_gene17 <- merge(E_gene17,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "DIS_Atlas2")
      
      E_gene17 <- dplyr::select(E_gene17, Signal, Master, Tissue_type, level1)
      
      E_gene(E_gene17)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene17 <- E_gene17[-which(colnames(E_gene17) == remove_column)]
      # colnames(E_gene17)[which(colnames(E_gene17) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene17)
      
    }
  })
  
  #NHPRTR ALL MACAQUES:
  
  #genIDs <- "APOE"
  E_gene18 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    NHPRTR_Macaque2 <- NHPRTR_Macaque %>%
      filter(Gene == genIDs) %>%
      dplyr::select(-c(Gene))
    
    colnames(NHPRTR_Macaque2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_Macaque2))
    
    if (dim(NHPRTR_Macaque2)[1] == 0) {
      E_gene18 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      E_gene18 <- setNames(data.frame(t(NHPRTR_Macaque2), grp = colnames(NHPRTR_Macaque2)), c("FPKM", "tissue"))
      
      
      rownames(E_gene18) <- NULL
      # E_gene18$DB <- c(rep("JMI",14), rep("RMI",14), rep("CMM",15), rep("CMC",16)) ####CLASISF
      E_gene18 <- merge(E_gene18,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "NHPRTR")
      
      E_gene18 <- dplyr::select(E_gene18, FPKM, Master, Tissue_type, level1)    ####CLASISF
      
      E_gene(E_gene18)
      
      # 
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene18 <- E_gene18[-which(colnames(E_gene18) == remove_column)]
      # colnames(E_gene18)[which(colnames(E_gene18) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene18)
      
    }
  })
  
  
  #NHPRTR JAPANESE MACAQUE:
  
  #genIDs <- "APOE"
  E_gene19 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    NHPRTR_JMI2 <- NHPRTR_JMI %>%
      filter(Gene == genIDs) %>%
      dplyr::select(-c(Gene))
    
    colnames(NHPRTR_JMI2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_JMI2))
    
    if (dim(NHPRTR_JMI2)[1] == 0) {
      E_gene19 <- data.frame(FPKM = numeric(0), tissue = character(0))
      
    } else {
      E_gene19 <- setNames(data.frame(t(NHPRTR_JMI2), grp = colnames(NHPRTR_JMI2)), c("FPKM", "tissue"))
      
      
      rownames(E_gene19) <- NULL
      E_gene19 <- merge(E_gene19,
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "NHPRTR")
      
      
      E_gene19 <- dplyr::select(E_gene19, FPKM, Master, Tissue_type, level1)
      E_gene(E_gene19)
      
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene19 <- E_gene19[-which(colnames(E_gene19) == remove_column)]
      # colnames(E_gene19)[which(colnames(E_gene19) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene19)
      
    }
    
  })
  
  
  # Human DIS Atlas
  
  DIS_Atlas_Human2 <- eventReactive(input$Submit, {
    genIDs <- input$geneName
    
    DIS_Atlas_Human2  <- DIS_Atlas_Human[toupper(DIS_Atlas_Human$Gene.Symbol) == genIDs,]
    DIS_Atlas_Human2 <- DIS_Atlas_Human2[-c(2:3)] %>% 
      filter(rowMeans(dplyr::select(.,-1)) > input$microarray_cutoff)
    
  })
  
  
  #Update the probes names on SelectInput
  
  observe({
    x <- DIS_Atlas_Human2()$Probe.Set.ID
    
    updateSelectInput(session, inputId = "probeDIS_Human", label = "Select Probe",
                      choices = x,
                      selected = DIS_Atlas_Human2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Human2()[, 2:length(DIS_Atlas_Human2())], 1, mean))]
    )
  })
  
  
  E_gene20 <- reactive({
    
    DIS_Atlas_Human2_subset <-
      DIS_Atlas_Human2()[DIS_Atlas_Human2()$Probe.Set.ID == input$probeDIS_Human, ]
    
    if (dim(DIS_Atlas_Human2_subset)[1] == 0) {
      E_gene20 <- data.frame(Signal = numeric(0), tissue = character(0))
      
    } else {
      E_gene20 <- setNames(data.frame(t(DIS_Atlas_Human2_subset[, 2:length(DIS_Atlas_Human2())]), grp = colnames(DIS_Atlas_Human2_subset[, 2:length(DIS_Atlas_Human2())])), c("Signal", "tissue"))
      
      rownames(E_gene20) <- NULL
      E_gene20$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene20$tissue)
      
      E_gene20 <- merge(E_gene20, 
                        Atlas_Dictionary_final,
                        by.x = "tissue",
                        by.y = "DIS_Atlas2")
      
      
      E_gene20 <- dplyr::select(E_gene20, Signal, Master, Tissue_type, level1)
      E_gene(E_gene20)
      # remove_column <- ifelse(input$grouped_tissue == "Tissue", "Master", "level1")
      # keep_column <- ifelse(input$grouped_tissue == "Tissue", "level1", "Master")
      # E_gene20 <- E_gene20[-which(colnames(E_gene20) == remove_column)]
      # colnames(E_gene20)[which(colnames(E_gene20) == keep_column)] <- "groupTissue"
      # 
      # return(E_gene20)
      
    }
  })

  
  
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------ P L O T S -------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  scaleY <- reactive(if (input$scalePlot=="Linear"){     #Default scale for y-axis is linear and log10 can be selected
    "identity" 
    
  } else{ "log10" })
  
  ##Plots
  
  graph <- function(geneDB, ytext = 15, angle_x = 45, vjust_x = 1, dotsize = 2, showLegend = "bottom") {  #ytext, angle_x vjust_x dotsize variables created to be adjusted on downloadable report
    # browser()
    
    validate(need(nrow(geneDB) > 0, message = 'No data available for this Gene'))
    
    
    
    validate(need(input$tissue, message = 'Please, select a Tissue System or "All"'))
    
    if ("All" %in% input$tissue) {
      geneDB <- geneDB
      
    } else {
      geneDB <- geneDB[geneDB$Tissue_type %in% input$tissue, ]
      
    }
    
    # selectedData <- geneDB[, c(test, colnames(geneDB[1]))]
    # group_column <-ifelse(test == "Tissue", "level1", "Master")     VERIFY
    selectedData <- geneDB[, c("groupTissue", colnames(geneDB[1]))]                 # VERIFY
    
    
    #    myColors <- rainbow(13)
    myColors <- c("#F8766D",
                  "#CC3232",
                  "#E7861B",
                  "#AFA100",
                  "#72B000",
                  "#00B81F",
                  "#00C08D",
                  "#00BFC4",
                  "#00ABFD",
                  "#7997FF",
                  "#DC71FA",
                  "#F763E0",
                  "#FF65AE"
    )
    
    names(myColors) <- levels(Atlas_Dictionary_final$Tissue_type)
    colScale <- scale_colour_manual(name = "Tissue System", values = myColors)
    
    ##Function created to have only integer values on y-axis when values too small
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    p1 <- ggplot(geneDB, aes(x = reorder(unlist(selectedData[1]),-unlist(geneDB[1])), fill = Tissue_type)) +
      geom_bar(stat = "count") +
      scale_fill_manual(values = myColors) +
      
      scale_y_continuous(breaks = integer_breaks()) +
      
      theme_bw() +
      facet_grid(~ Tissue_type, scale = "free", space = "free_x") +
      ylab("n") +
      theme(
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = ytext)
      )
    
    
    
    p2 <- ggplot(data = geneDB, aes(x = reorder(unlist(selectedData[1]),-unlist(geneDB[1])), y = unlist(geneDB[1]), colour = Tissue_type)) +
      
      geom_point(size = dotsize, stroke = 0, shape = 16) +
      stat_summary(fun = "median", geom = "crossbar", width = 0.5, size =0.2, colour = "black") +
      
      colScale +
      scale_y_continuous(trans = scaleY()) +
      labs(color = "") +
      theme_bw() +
      facet_grid(~ Tissue_type, scale = "free", space = "free_x") +
      ylab(colnames(geneDB[1])) +
      theme(
        legend.position = showLegend,
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(
          angle = angle_x,
          vjust = vjust_x,
          hjust = 1,
          size = (ytext / 1.5)
        ),
        axis.title.x = element_blank(),
        text = element_text(size = ytext)
      )
    
    
    egg::ggarrange(p1, p2, heights = c(1, 4))
    
    
  }
  
  
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------ T A B L E S -----------------------------------------------
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  DBs <- c("GTEx",
           "HPA",
           "BarkBase",
           "NHPRTR_CMC",
           "NHPRTR_CMM",
           "NHPRTR_RMI",
           "BioGPS_Mouse",
           "BioGPS_Mouse2",
           "BioGPS_Human",
           "Ratlas",
           "Minipig_Atlas",
           "Mouse_MTAB6081",
           "Rat_MTAB6081",
           "DIS_Atlas_Monkey",
           "DIS_Atlas_Dog",
           "DIS_Atlas_Rat",
           "DIS_Atlas_Mouse",
           "NHPRTR_all_mac",
           "NHPRTR_JMI",
           "DIS_Atlas_Human")

  mean_DB <- function(geneDB) {
    # browser()
    if (nrow(geneDB) > 0) {
      mean_DB <- aggregate(geneDB[1],
                           by = list(groupTissue = geneDB$groupTissue, Tissue_type = geneDB$Tissue_type),
                           FUN = mean)
      mean_DB[3] <- round(mean_DB[3], 3)
      
      mean_DB$Enrichment <- round(scale(mean_DB[3]), 4)
      
      x <- mean_DB[3]
      x <- (1 - (x / max(x)))
      res <- sum(x)
      mean_DB$Tau <- round(res / (nrow(x) - 1), 4)
      
      if (colnames(mean_DB[3]) == "Signal") {
        
        mean_DB$Expression <- cut(mean_DB[, 3],
                                  breaks = c(0, 25, 100, 1000, Inf),
                                  labels = c("below cutoff", "low", "moderate", "high"),
                                  ordered_result = TRUE
        )
        
      } else {
        
        mean_DB$Expression <- cut(mean_DB[, 3],
                                  breaks = c(0, 0.5, 10, 1000, Inf),
                                  labels = c("below cutoff", "low", "moderate", "high"),
                                  ordered_result = TRUE
        )
        
      }
      
      
      
      db.index <- deparse(substitute(geneDB))  #obtain number from E_genex1-9
      db.index <- as.numeric(gsub("\\D", "", db.index))
      

      Species <- c("Homo sapiens",
                   "Homo sapiens",
                   "Canis familiaris",
                   "Nonhuman primate",
                   "Nonhuman primate",
                   "Nonhuman primate",
                   "Mus musculus",
                   "Mus musculus",
                   "Homo sapiens",
                   "Rattus norvegicus",
                   "Sus scrofa domesticus",
                   "Mus musculus",
                   "Rattus norvegicus",
                   "Nonhuman primate",
                   "Canis familiaris",
                   "Rattus norvegicus",
                   "Mus musculus",
                   "Nonhuman primate",
                   "Nonhuman primate",
                   "Homo sapiens"
      )
      
      mean_DB$Database  <- DBs[db.index]
      mean_DB$Species    <- Species[db.index]
      mean_DB$Species <- factor(mean_DB$Species,
                                levels = c("Homo sapiens",
                                           "Nonhuman primate",
                                           "Canis familiaris",
                                           "Sus scrofa domesticus",
                                           "Rattus norvegicus",
                                           "Mus musculus"
                                )
      )
      return(mean_DB)
      
    } else {
      mean_DB <- data.frame()
    }
  }
  
  
  
  
  singleDB <- function(DB) {
    validate(need(nrow(mean_DB(DB)) > 0, message = 'No data available for this Gene'))
    
    singleDB <- mean_DB(DB)[, 1:4][order(mean_DB(DB)$Enrichment, decreasing = T), ]
    colnames(singleDB)[c(1, 2)] <- c("Tissue", "Tissue type")
    rownames(singleDB) <- NULL
    
    
    if ("All" %in% input$tissue) {
      return(singleDB)
      
    } else {
      singleDB <- singleDB[singleDB$`Tissue type` %in% input$tissue, ]
      return(singleDB)
      
    }
    
  }
  
  # mean_expression <- function(geneDB){
  #   # browser()
  #   if (colnames(geneDB[1]) == "Signal"){
  #     
  #     if(mean(geneDB[,1])>input$microarray_cutoff){
  #       round(mean(geneDB[,1]), 3)
  #       
  #     } else {
  #       round(mean(geneDB[,1]), 3)
  #       
  #       # "below cutoff"
  #     }
  #     
  #   } else {
  #     if(mean(geneDB[,1])>0.5){
  #       round(mean(geneDB[,1]), 3)
  #       
  #     } else {
  #       
  #       round(mean(geneDB[,1]), 3)
  #       
  #       # "below cutoff"
  #     }
  #     
  #   }
  #   
  # }
  
  enrich_tissues <- function(geneDB){
    enrich_tissues<- if (input$ZScore_cutoff== 3){
      
      paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", ")
      
    } else if ((input$ZScore_cutoff== 2)){
      paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
            sep = ", ")
      
    } else if ((input$ZScore_cutoff== 1)){
      
      paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 1 & mean_DB(geneDB)$Enrichment < 2 ], "*", collapse = ", "), 
            sep = ", ")
    } else {
      paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 1 & mean_DB(geneDB)$Enrichment < 2 ], "*", collapse = ", "), 
            paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 0 & mean_DB(geneDB)$Enrichment < 1 ], "", collapse = ", "), 
            sep = ", ")
    }
    
    asterisks_alone <- gsub('(^\\*{1,3})|(, \\*{1,2})', '', enrich_tissues)
    gsub('^, ', '', asterisks_alone)  
  }
  
  macaque_DB_summary <- function(){
    switch(input$NHPRTR_merge,
           NHPRTR_all_mac  = E_gene18(),
           NHPRTR_CMC = E_gene4(),
           NHPRTR_CMM = E_gene5(),
           NHPRTR_JMI = E_gene19(),
           NHPRTR_RMI = E_gene6())
  }
  
  DB_summary <- function() {
     # browser()

    gene <- human_all_organism[human_all_organism$HomoloGene.ID==unique(human_all_organism$HomoloGene.ID[human_all_organism$Symbol==input$geneName]),]
    # genIDs <- gene$Symbol[gene$Common.Organism.Name==specie]
    #####Intermediate function created to be used on RMarkdown

    if (gene_description_reactive() != "Select Gene" || gene_description_reactive() != "" ) {

      # gene_mouse = ifelse(rlang::is_empty(geneID_mouse$SYMBOL[geneID_mouse$HGNC.symbol==gene_description_reactive()]), "", geneID_mouse$SYMBOL[geneID_mouse$HGNC.symbol==input$geneName])
      # gene_rat   = ifelse(rlang::is_empty(geneID_rat$SYMBOL[geneID_rat$HGNC.symbol==input$geneName]), "", geneID_rat$SYMBOL[geneID_rat$HGNC.symbol==input$geneName])
      # gene_mouse = ifelse(rlang::is_empty(gene$Symbol[gene$Common.Organism.Name=="mouse"]), "", gene$Symbol[gene$Common.Organism.Name=="mouse"])
      # gene_rat   = ifelse(rlang::is_empty(gene$Symbol[gene$Common.Organism.Name=="rat"]), "", gene$Symbol[gene$Common.Organism.Name=="rat"])

      if (nrow(E_gene1()) > 0) {
        GTEx_sum <- c("Homo sapiens", "GTEx", "RNASeq", " ", " ", gene_description_reactive(), enrich_tissues(E_gene1()), round(mean(E_gene1()[,1]), 3),
                      round(mean_DB(E_gene1())$Tau[1], 3),
                      round(max(mean_DB(E_gene1())[3]) / max(mean_DB(E_gene1())[3][mean_DB(E_gene1())[3] != max(mean_DB(E_gene1())[3])]), 3))

      } else {
        GTEx_sum <- c("Homo sapiens", "GTEx", "RNASeq", " ", " ", gene_description_reactive(), "", "", "", "")
      }



      if (nrow(E_gene2()) > 0) {
        HPA_sum <- c("Homo sapiens", "HPA", "RNASeq", " ", " ", gene_description_reactive(), enrich_tissues(E_gene2()), round(mean(E_gene2()[,1]), 3),
                     round(mean_DB(E_gene2())$Tau[1], 3),
                     round(max(mean_DB(E_gene2())[3]) / max(mean_DB(E_gene2())[3][mean_DB(E_gene2())[3] != max(mean_DB(E_gene2())[3])]), 3))

      } else {
        HPA_sum <- c("Homo sapiens", "HPA", "RNASeq", " ", " ", gene_description_reactive(), "", "", "", "")
      }



      if (nrow(E_gene3()) > 0) {
        BarkBase_sum <- c("Canis familiaris", "BarkBase", "RNASeq", " ", " ", gene_description_reactive(), enrich_tissues(E_gene3()), round(mean(E_gene3()[,1]), 3),
                          round(mean_DB(E_gene3())$Tau[1], 3),
                          round(max(mean_DB(E_gene3())[3]) / max(mean_DB(E_gene3())[3][mean_DB(E_gene3())[3] != max(mean_DB(E_gene3())[3])]), 3)
        )

      } else {
        BarkBase_sum <- c("Canis familiaris", "BarkBase", "RNASeq", " ", " ", gene_description_reactive(), "", "", "", "")
      }



      # if (nrow(E_gene4()) > 0) {
      #   CMC_sum <- c( "Nonhuman primate", "NHPRTR_CMC", "RNASeq", " ", " ", input$geneName, enrich_tissues(E_gene4()), round(mean(E_gene4()[,1]), 3),
      #                 round(mean_DB(E_gene4())$Tau[1], 3),
      #                 round(max(mean_DB(E_gene4())[3]) / max(mean_DB(E_gene4())[3][mean_DB(E_gene4())[3] != max(mean_DB(E_gene4())[3])]), 3)
      #   )
      #
      # } else {
      #   CMC_sum <- c("Nonhuman primate", "NHPRTR_CMC", "RNASeq", " ", " ", input$geneName, "", "", "", "")
      # }



      # if (nrow(E_gene5()) > 0) {
      #   CMM_sum <- c( "Nonhuman primate", "NHPRTR_CMM", "RNASeq", " ", " ", input$geneName, enrich_tissues(E_gene5()), round(mean(E_gene5()[,1]), 3),
      #                 round(mean_DB(E_gene5())$Tau[1], 3), round(max(mean_DB(E_gene5())[3]) / max(mean_DB(E_gene5())[3][mean_DB(E_gene5())[3] != max(mean_DB(E_gene5())[3])]), 3)
      #   )
      #
      # } else {
      #   CMM_sum <- c("Nonhuman primate", "NHPRTR_CMM", "RNASeq", " ", " ", input$geneName, "", "", "", "")
      # }

      # if (nrow(E_gene6()) > 0) {
      #   RMI_sum <- c( "Nonhuman primate", "NHPRTR_RMI", "RNASeq", " ", " ", input$geneName, enrich_tissues(E_gene6()), round(mean(E_gene6()[,1]), 3),
      #                 round(mean_DB(E_gene6())$Tau[1], 3), round(max(mean_DB(E_gene6())[3]) / max(mean_DB(E_gene6())[3][mean_DB(E_gene6())[3] != max(mean_DB(E_gene6())[3])]), 3)
      #   )
      #
      # } else {
      #   RMI_sum <- c("Nonhuman primate", "NHPRTR_RMI", "RNASeq", " ", " ", input$geneName, "", "", "", "")
      # }

      if (nrow(E_gene7()) > 0) {
        BioGPS_M_sum <- c( "Mus musculus", "BioGPS_Mouse", "microarray", "Mouse GNF1M", input$probeBioGPS_Mouse, translation_gene("mouse"),
                           enrich_tissues(E_gene7()), round(mean(E_gene7()[,1]), 3),
                           round(mean_DB(E_gene7())$Tau[1], 3), round(max(mean_DB(E_gene7())[3]) / max(mean_DB(E_gene7())[3][mean_DB(E_gene7())[3] != max(mean_DB(E_gene7())[3])]), 3)
        )

      } else {
        BioGPS_M_sum <- c("Mus musculus", "BioGPS_Mouse", "microarray", " ", " ", translation_gene("mouse"), "", "", "", "")
      }

      if (nrow(E_gene8()) > 0) {
        BioGPS_M2_sum <- c( "Mus musculus", "BioGPS_Mouse2", "microarray", "Mouse MOE430", input$probeBioGPS_Mouse2,  translation_gene("mouse"),
                            enrich_tissues(E_gene8()), round(mean(E_gene8()[,1]), 3),
                            round(mean_DB(E_gene8())$Tau[1], 3), round(max(mean_DB(E_gene8())[3]) / max(mean_DB(E_gene8())[3][mean_DB(E_gene8())[3] != max(mean_DB(E_gene8())[3])]), 3)
        )

      } else {
        BioGPS_M2_sum <- c("Mus musculus", "BioGPS_Mouse2", "microarray", " ", " ", translation_gene("mouse"), "", "", "", "")
      }

      if (nrow(E_gene9()) > 0) {
        BioGPS_H_sum <- c("Homo sapiens", "BioGPS_Human", "microarray", ifelse(grepl("gnf", input$probeBioGPS_Human), "Human GNF1H", "Human U133A"), input$probeBioGPS_Human, gene_description_reactive(),
                          enrich_tissues(E_gene9()), round(mean(E_gene9()[,1]), 3),
                          round(mean_DB(E_gene9())$Tau[1], 3), round(max(mean_DB(E_gene9())[3]) / max(mean_DB(E_gene9())[3][mean_DB(E_gene9())[3] != max(mean_DB(E_gene9())[3])]), 3)
        )

      } else {
        BioGPS_H_sum <- c("Homo sapiens", "BioGPS_Human ", "microarray", " ", " ", gene_description_reactive(),"", "", "", "")
      }

      if (nrow(E_gene10()) > 0) {
        Ratlas_sum <- c("Rattus norvegicus", "Ratlas", "RNASeq", " ", " ",
                        translation_gene("rat"),
                        enrich_tissues(E_gene10()), round(mean(E_gene10()[,1]), 3),  round(mean_DB(E_gene10())$Tau[1], 3),
                        round(max(mean_DB(E_gene10())[3]) / max(mean_DB(E_gene10())[3][mean_DB(E_gene10())[3] != max(mean_DB(E_gene10())[3])]), 3))

      } else {
        Ratlas_sum <- c("Rattus norvegicus", "Ratlas", "RNASeq", " ", " ", translation_gene("rat"), "", "", "", "")
      }

      if (nrow(E_gene11()) > 0) {
        Minipig_Atlas_sum <- c("Sus scrofa domesticus", "Minipig_Atlas", "RNASeq", " ", " ", gene_description_reactive(), enrich_tissues(E_gene11()), round(mean(E_gene11()[,1]), 3),
                               round(mean_DB(E_gene11())$Tau[1], 3), round(max(mean_DB(E_gene11())[3]) / max(mean_DB(E_gene11())[3][mean_DB(E_gene11())[3] != max(mean_DB(E_gene11())[3])]), 3))

      } else {
        Minipig_Atlas_sum <- c("Sus scrofa domesticus", "Minipig_Atlas", "RNASeq", " ", " ", gene_description_reactive(), "", "", "", "")
      }

      if (nrow(E_gene12()) > 0) {
        Mouse_MTAB6081_sum <- c("Mus musculus", "Mouse_MTAB6081", "RNASeq", " ", " ", translation_gene("mouse"),
                                enrich_tissues(E_gene12()), round(mean(E_gene12()[,1]), 3),
                                round(mean_DB(E_gene12())$Tau[1], 3), round(max(mean_DB(E_gene12())[3]) / max(mean_DB(E_gene12())[3][mean_DB(E_gene12())[3] != max(mean_DB(E_gene12())[3])]), 3))

      } else {
        Mouse_MTAB6081_sum <- c("Mus musculus", "Mouse_MTAB6081", "RNASeq", " ", " ", translation_gene("mouse"), "", "", "", "")
      }

      if (nrow(E_gene13()) > 0) {
        Rat_MTAB6081_sum <- c("Rattus norvegicus", "Rat_MTAB6081", "RNASeq", " ", " ", translation_gene("rat"),
                              enrich_tissues(E_gene13()), round(mean(E_gene13()[,1]), 3),
                              round(mean_DB(E_gene13())$Tau[1], 3), round(max(mean_DB(E_gene13())[3]) / max(mean_DB(E_gene13())[3][mean_DB(E_gene13())[3] != max(mean_DB(E_gene13())[3])]), 3))

      } else {
        Rat_MTAB6081_sum <- c("Rattus norvegicus", "Rat_MTAB6081", "RNASeq", " ", " ", translation_gene("rat"), "", "", "", "")
      }

      if (nrow(E_gene14()) > 0) {
        DIS_Monkey_sum <- c( "Nonhuman primate", "DIS_Atlas_Monkey", "microarray", "Human Genome U133 Plus 2.0", input$probeDIS_Monkey, gene_description_reactive(), enrich_tissues(E_gene14()), round(mean(E_gene14()[,1]), 3),
                             round(mean_DB(E_gene14())$Tau[1], 3), round(max(mean_DB(E_gene14())[3]) / max(mean_DB(E_gene14())[3][mean_DB(E_gene14())[3] != max(mean_DB(E_gene14())[3])]), 3)
        )

      } else {
        DIS_Monkey_sum <- c("Nonhuman primate", "DIS_Atlas_Monkey", "microarray", " ", " ", gene_description_reactive(), "", "", "", "")
      }

      if (nrow(E_gene15()) > 0) {
        DIS_Dog_sum <- c( "Canis familiaris", "DIS_Atlas_Dog", "microarray", "Canine_2 Genome", input$probeDIS_Dog, gene_description_reactive(), enrich_tissues(E_gene15()), round(mean(E_gene15()[,1]), 3),
                          round(mean_DB(E_gene15())$Tau[1], 3), round(max(mean_DB(E_gene15())[3]) / max(mean_DB(E_gene15())[3][mean_DB(E_gene15())[3] != max(mean_DB(E_gene15())[3])]), 3)
        )

      } else {
        DIS_Dog_sum <- c("Canis familiaris", "DIS_Atlas_Dog", "microarray", " ", " ", gene_description_reactive(), "", "", "", "")
      }

      if (nrow(E_gene16()) > 0) {
        DIS_Rat_sum <- c( "Rattus norvegicus", "DIS_Atlas_Rat", "microarray", "HT Rat Genome 230", input$probeDIS_Rat, translation_gene("rat"), enrich_tissues(E_gene16()), round(mean(E_gene16()[,1]), 3),
                          round(mean_DB(E_gene16())$Tau[1], 3), round(max(mean_DB(E_gene16())[3]) / max(mean_DB(E_gene16())[3][mean_DB(E_gene16())[3] != max(mean_DB(E_gene16())[3])]), 3)
        )

      } else {
        DIS_Rat_sum <- c("Rattus norvegicus", "DIS_Atlas_Rat", "microarray", " ", " ", translation_gene("rat"), "", "", "", "")
      }

      if (nrow(E_gene17()) > 0) {
        DIS_Mouse_sum <- c( "Mus musculus", "DIS_Atlas_Mouse", "microarray", "HT Mouse Genome 230", input$probeDIS_Mouse, translation_gene("mouse"), enrich_tissues(E_gene17()), round(mean(E_gene17()[,1]), 3),
                            round(mean_DB(E_gene17())$Tau[1], 3), round(max(mean_DB(E_gene17())[3]) / max(mean_DB(E_gene17())[3][mean_DB(E_gene17())[3] != max(mean_DB(E_gene17())[3])]), 3)
        )

      } else {
        DIS_Mouse_sum <- c("Mus musculus", "DIS_Atlas_Mouse", "microarray", " ", " ", translation_gene("mouse"), "", "", "", "")
      }


      # if (nrow(E_gene18()) > 0) {
      #   MAC_sum <- c( "Nonhuman primate", "NHPRTR_Macaque", "RNASeq", " ", " ", input$geneName, enrich_tissues(E_gene18()), round(mean(E_gene18()[,1]), 3),
      #                 round(mean_DB(E_gene18())$Tau[1], 3),
      #                 round(max(mean_DB(E_gene18())[3]) / max(mean_DB(E_gene18())[3][mean_DB(E_gene18())[3] != max(mean_DB(E_gene18())[3])]), 3)
      #   )
      #
      # } else {
      #   MAC_sum <- c("Nonhuman primate", "NHPRTR_Macaque", "RNASeq", " ", " ", input$geneName, "", "", "", "")
      # }
      #
      # if (nrow(E_gene19()) > 0) {
      #   JMI_sum <- c( "Nonhuman primate", "NHPRTR_JMI", "RNASeq", " ", " ", input$geneName, enrich_tissues(E_gene19()), round(mean(E_gene19()[,1]), 3),
      #                 round(mean_DB(E_gene19())$Tau[1], 3),
      #                 round(max(mean_DB(E_gene19())[3]) / max(mean_DB(E_gene19())[3][mean_DB(E_gene19())[3] != max(mean_DB(E_gene19())[3])]), 3)
      #   )
      #
      # } else {
      #   JMI_sum <- c("Nonhuman primate", "NHPRTR_JMI", "RNASeq", " ", " ", input$geneName, "", "", "", "")
      # }

      if (nrow(macaque_DB_summary()) > 0) {
        NHPRTR_sum <- c( "Nonhuman primate", input$NHPRTR_merge, "RNASeq", " ", " ", gene_description_reactive(), enrich_tissues(macaque_DB_summary()), 
                         round(mean(macaque_DB_summary()[,1]), 3), round(mean_DB(macaque_DB_summary())$Tau[1], 3),
                         round(max(mean_DB(macaque_DB_summary())[3]) / max(mean_DB(macaque_DB_summary())[3][mean_DB(macaque_DB_summary())[3] != max(mean_DB(macaque_DB_summary())[3])]), 3)
        )
        
      } else {
        NHPRTR_sum <- c("Nonhuman primate", input$NHPRTR_merge, "RNASeq", " ", " ", gene_description_reactive(), "", "", "", "")
      }
      
      if (nrow(E_gene20()) > 0) {
        DIS_Human_sum <- c( "Homo sapiens", "DIS_Atlas_Human", "microarray", "Human Genome U133 Plus 2.0", input$probeDIS_Human, gene_description_reactive(), enrich_tissues(E_gene20()), round(mean(E_gene20()[,1]), 3),
                            round(mean_DB(E_gene20())$Tau[1], 3), round(max(mean_DB(E_gene20())[3]) / max(mean_DB(E_gene20())[3][mean_DB(E_gene20())[3] != max(mean_DB(E_gene20())[3])]), 3)
        )
        
      } else {
        DIS_Human_sum <- c("Homo sapiens", "DIS_Atlas_Human", "microarray", " ", " ", gene_description_reactive(), "", "", "", "")
      }
      
      
      
      sum_table <- data.frame(rbind(
        
        DIS_Human_sum,
        GTEx_sum,
        HPA_sum,
        BioGPS_H_sum,
        DIS_Monkey_sum,
        NHPRTR_sum,
        # NHPRTR_sum,
        # CMC_sum,
        # CMM_sum,
        # RMI_sum,
        # JMI_sum,
        DIS_Dog_sum,
        BarkBase_sum,
        Minipig_Atlas_sum,
        DIS_Rat_sum,
        Ratlas_sum,
        Rat_MTAB6081_sum,
        DIS_Mouse_sum,
        BioGPS_M_sum,
        BioGPS_M2_sum,
        Mouse_MTAB6081_sum
        
      )
      )
      
      colnames(sum_table) <- c("Species",
                               "Databases",
                               "Technology",
                               "Chip",
                               "Probes",
                               "Gene",
                               "Enrichment",
                               "Mean Expression",
                               "Tau",
                               "Tau ratio"
      )


      # DB_summary$Species <- as.factor(DB_summary$Species, levels = c('Homo sapiens', 'Monkey', 'Dog', 'Mus musculus'))

      if ("All" %in% input$database) {
        sum_table <- sum_table
        
      } else {
        selected_DBs <- input$database
        selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', input$NHPRTR_merge)
        
        sum_table <- sum_table[sum_table$Database %in% selected_DBs, ]
        
      }
      
      if ("All" %in% input$species) {
        sum_table <- sum_table
        
      } else {
        sum_table <- sum_table[sum_table$Species %in% input$species, ]
        
      }
      
      return(sum_table)
      
    }
  }
  
  summary_enriched <- function(x){
    # browser()
    
    macaque_mean_DB <- 
      switch(input$NHPRTR_merge,
             NHPRTR_all_mac  = mean_DB(E_gene18()),
             NHPRTR_CMC = mean_DB(E_gene4()),
             NHPRTR_CMM = mean_DB(E_gene5()),
             NHPRTR_JMI = mean_DB(E_gene19()),
             NHPRTR_RMI = mean_DB(E_gene6()))
    
    DIS_Atlas_Human <-  mean_DB(E_gene20())[-3]
    GTEx <- mean_DB(E_gene1())[-3]  #Remove TPM/CPM.. column
    HPA <- mean_DB(E_gene2())[-3]
    BioGPS_H <- mean_DB(E_gene9())[-3]
    DIS_Atlas_Monkey <-  mean_DB(E_gene14())[-3]
    NHPRTR_Macaque <- macaque_mean_DB[-3] 
    # NHPRTR_Macaque <- mean_DB(E_gene18())[-3]
    # NHPRTR_CMC <- mean_DB(E_gene4())[-3]
    # NHPRTR_CMM <- mean_DB(E_gene5())[-3]
    # NHPRTR_RMI <- mean_DB(E_gene6())[-3]
    # NHPRTR_JMI <- mean_DB(E_gene19())[-3]
    DIS_Atlas_Dog <-  mean_DB(E_gene15())[-3]
    BarkBase <- mean_DB(E_gene3())[-3]
    Minipig_Atlas <- mean_DB(E_gene11())[-3]
    DIS_Atlas_Rat <-  mean_DB(E_gene16())[-3]
    Ratlas <- mean_DB(E_gene10())[-3]
    Rat_MTAB6081 <- mean_DB(E_gene13())[-3]
    DIS_Atlas_Mouse <-  mean_DB(E_gene17())[-3]
    BioGPS_M <- mean_DB(E_gene7())[-3]
    BioGPS_M2 <- mean_DB(E_gene8())[-3]
    Mouse_MTAB6081 <- mean_DB(E_gene12())[-3]
    
    summary_enriched <- data.frame(rbind(
      DIS_Atlas_Human,
      GTEx,
      HPA,
      BioGPS_H,
      DIS_Atlas_Monkey,
      NHPRTR_Macaque,
      # NHPRTR_CMC,
      # NHPRTR_CMM,
      # NHPRTR_RMI,
      # NHPRTR_JMI,
      DIS_Atlas_Dog,
      BarkBase,
      Minipig_Atlas,
      DIS_Atlas_Rat,
      Ratlas,
      Rat_MTAB6081,
      DIS_Atlas_Mouse,
      BioGPS_M,
      BioGPS_M2,
      Mouse_MTAB6081
    )
    )
    
    # summary_enriched <- summary_enriched[summary_enriched$Enrichment > input$ZScore_cutoff & summary_enriched$Enrichment > 0.01, ]  #0.01 verification added to solve issue in "APOE" with ZScore=0.0005 but being displayed
    # summary_enriched$Enrichment <- as.numeric(summary_enriched$Enrichment)
    ZS <- as.numeric(input$ZScore_cutoff)    #To avoid some filter done wrong since it consider the value as a character
    
    summary_enriched <- summary_enriched %>% 
      filter(Enrichment > ZS)
    
    validate(need(nrow(summary_enriched) > 0, message = 'No enriched tissues for this Gene'))
    
    ####PLACE TO INTRODUCE THE ***
    
    if ("All" %in% input$database) {
      summary_enriched <- summary_enriched
    } else {
      selected_DBs <- input$database
      selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', NHPRTR_Macaque$Database)      
      summary_enriched <- summary_enriched[summary_enriched$Database %in% selected_DBs,]}
    
    if ("All" %in% input$tissue) {summary_enriched <- summary_enriched
    } else {summary_enriched <- summary_enriched[summary_enriched$Tissue_type %in% input$tissue,]}
    
    if ("All" %in% input$species) {summary_enriched <- summary_enriched
    } else {summary_enriched <- summary_enriched[summary_enriched$Species %in% input$species,]}
    
    

    summary_enriched <- summary_enriched %>%
      # mutate(row = row_number()) %>%
      dplyr::select(groupTissue, Species, Expression, Enrichment, Database)
    # %>% 
      # arrange(match(Expression, c('high', 'moderate', 'low', 'below cutoff'))) 
    # %>%
    #   distinct(groupTissue, Species, Database, .keep_all = TRUE)
    
    return(summary_enriched)   #ADDED
    
  }
  
  tissueSpecies <- reactive({
      # browser()
    
    # tissueSpecies <- summary_enriched()[-5]
    tissueSpecies <- summary_enriched()
    
    tissueSpecies$Species <- ifelse (tissueSpecies$Enrichment>3,  paste0(tissueSpecies$Species, "***"), 
                                     ifelse (tissueSpecies$Enrichment>2, paste0(tissueSpecies$Species, "**"), 
                                             ifelse (tissueSpecies$Enrichment>1, paste0(tissueSpecies$Species, "*"), as.character(tissueSpecies$Species))))
    tissueSpecies <- tissueSpecies[,-c(4)]
    tissueSpecies <- tissueSpecies %>% 
      group_by(groupTissue, Species, Expression) %>% 
      summarise_all(funs(trimws(paste(., collapse = ', ')))) %>% 
      ungroup() %>% 
      mutate(Species = paste0(Species," (", Database, ")")) %>% 
      select(-Database) %>% 
      arrange(match(Expression, c('high', 'moderate', 'low', 'below cutoff'))) 
    
    
    tissueSpecies <- pivot_wider(tissueSpecies, names_from = Expression, values_from = Species, values_fn = toString)  
    
    colnames(tissueSpecies) <- c("Tissue", paste0(colnames(tissueSpecies)[2:length(tissueSpecies)], " expression"))

    return(tissueSpecies) 
    
  })

  speciesTissue <- function() {
      # browser()
    # speciesTissue <- summary_enriched()[-5]
    speciesTissue <- summary_enriched()
    
    speciesTissue$groupTissue <- ifelse (speciesTissue$Enrichment>3,  paste0(speciesTissue$groupTissue, "***"), 
                                         ifelse (speciesTissue$Enrichment>2, paste0(speciesTissue$groupTissue, "**"), 
                                                 ifelse (speciesTissue$Enrichment>1, paste0(speciesTissue$groupTissue, "*"), as.character(speciesTissue$groupTissue))))
    speciesTissue <- speciesTissue[-4]
    
    speciesTissue <- speciesTissue %>% 
      group_by(groupTissue, Species, Expression) %>% 
      summarise_all(funs(trimws(paste(., collapse = ', ')))) %>% 
      ungroup() %>% 
      mutate(groupTissue = paste0(groupTissue," (", Database, ")")) %>% 
      select(-Database) %>% 
      arrange(match(Expression, c('high', 'moderate', 'low', 'below cutoff')))
     # speciesTissue$Species <- factor(speciesTissue$Species, levels=c("Human", "Monkey", "Dog", "Minipig", "Rat", "Mouse")) %>% 
      # arrange(factor(Species, levels = c("Homo sapiens", "Nonhuman primate", "Canis familiaris", "Sus scrofa domesticus", "Rattus norvegicus", "Mus musculus")))
    # speciesTissue$Species <- droplevels(speciesTissue$Species)
    speciesTissue <- pivot_wider(speciesTissue, names_from = Expression, values_from = groupTissue, values_fn = toString) %>% 
      arrange(factor(Species, levels = c("Homo sapiens", "Nonhuman primate", "Canis familiaris", "Sus scrofa domesticus", "Rattus norvegicus", "Mus musculus")))
    
    # speciesTissue$groupTissue <- as.character(speciesTissue$groupTissue)
        # speciesTissue$Species <- factor(speciesTissue$Species, levels = c("Homo sapiens", 
                                                                      # "Nonhuman primate", 
                                                                      # "Canis familiaris",
                                                                      # "Sus scrofa domesticus",
                                                                      # "Rattus norvegicus",
                                                                      # "Mus musculus"))
    # speciesTissue <- aggregate( ~ Species, unique(speciesTissue), paste, collapse = ", ")
    
    # speciesTissue <- as.data.frame(speciesTissue)
    # speciesTissue <- speciesTissue[, 2:3]
    colnames(speciesTissue) <- c("Species", paste0(colnames(speciesTissue)[2:length(speciesTissue)], " expression"))
    
    return(speciesTissue)
    
  }
  
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #-------------------------------------   H E A T M A P   ----------------------------------------
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #Consider situation where not all DB are selected. Recode E_gene into GTEX, HPA, etc??
  
  heatmap_sum <- reactive({
     # browser()
    
    macaque_mean_DB <- switch(input$NHPRTR_merge,
                              
                              NHPRTR_all_mac  = mean_DB(E_gene18()),
                              NHPRTR_CMC = mean_DB(E_gene4()),
                              NHPRTR_CMM = mean_DB(E_gene5()),
                              NHPRTR_JMI = mean_DB(E_gene19()),
                              NHPRTR_RMI = mean_DB(E_gene6()))
    
    
    heatmap_sum <- bind_rows(
      
      mean_DB(E_gene1())[-3],
      mean_DB(E_gene2())[-3],
      mean_DB(E_gene3())[-3],
      # mean_DB(E_gene4())[-3],
      # mean_DB(E_gene5())[-3],
      # mean_DB(E_gene6())[-3],
      mean_DB(E_gene7())[-3],
      mean_DB(E_gene8())[-3],
      mean_DB(E_gene9())[-3],
      mean_DB(E_gene10())[-3],
      mean_DB(E_gene11())[-3],
      mean_DB(E_gene12())[-3],
      mean_DB(E_gene13())[-3],
      mean_DB(E_gene14())[-3],
      mean_DB(E_gene15())[-3],
      mean_DB(E_gene16())[-3],
      mean_DB(E_gene17())[-3],
      # mean_DB(E_gene18())[-3],
      # mean_DB(E_gene19())[-3]
      macaque_mean_DB[-3],
      mean_DB(E_gene20())[-3]
      
    )
    
    if ("All" %in% input$database) {
      heatmap_sum <- heatmap_sum
      
    } else {
      selected_DBs <- input$database
      selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', input$NHPRTR_merge)
      heatmap_sum <- heatmap_sum[heatmap_sum$Database %in% selected_DBs, ]}
    
    
    if ("All" %in% input$tissue) {heatmap_sum <- heatmap_sum
    } else {heatmap_sum <- heatmap_sum[heatmap_sum$Tissue_type %in% input$tissue,]}
    
    if ("All" %in% input$species) {heatmap_sum <- heatmap_sum
    } else {heatmap_sum <- heatmap_sum[heatmap_sum$Species %in% input$species,]}
    
    #Select only the tissues where at least one Enrichment > 0 and then subset the data to plot all values where the tissue is for all DB
    
    heatmap_sum_ZS0 <- heatmap_sum %>% 
      filter(Enrichment >= as.numeric(input$ZScore_cutoff))
    
    heatmap_sum <- heatmap_sum[heatmap_sum$groupTissue %in% heatmap_sum_ZS0$groupTissue ,]   
  })
  
  #Create a function that will be used by the Shiny app and the Rmarkdown with different parameters due to different visualization
  
  heatmap_plot <- function(circle_size, axis_size=14){
    # browser()
    # p3 <-
    ggplot(data = heatmap_sum(), aes(x = Database, y = groupTissue, size = Expression)) +
      geom_point(aes(colour = cut(Enrichment, list(-Inf, 1, 2, 3, 999)))) +
      
      scale_color_manual(
        name = "Enrichment (ZScore)",
        values = list(
          "(-Inf,1]" = "skyblue1",
          "(1,2]" = "gold",
          "(2,3]" = "orangered2",
          "(3,999]" = "red4"
        ),
        
        labels = list("<1", "2", "3", ">3"),
        drop = FALSE) +
      scale_size_manual(
        values = c(0.7*circle_size, 1.5*circle_size, 3*circle_size, 5*circle_size),
        breaks = list("below cutoff", "low", "moderate", "high"),
        drop = FALSE) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        strip.text.x = element_text(angle = 0, size = axis_size),
        strip.text.y = element_text(angle = 0, size = axis_size),
        
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = axis_size
        ),
        
        axis.text.y = element_text(size = axis_size)
        
      ) +
      
      facet_grid(Tissue_type ~ Species, scale = "free", space = "free") +
      
      labs(x = "Tissue", y = "Database") +
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = (axis_size+2), face = "bold"),
        strip.background = element_rect(fill = "#cce6ff")
      )
    
    #Not included extra column providing Tau value (p4) because the loading time increase drastically
    # p4 <-
    # ggplot(heatmap_sum(), aes(x=Tau, y=Database))+
    #   facet_grid(Species ~. , scale = "free", space = "free") +
    #
    #
    #   stat_bin2d(aes(fill = Tau), binwidth = c(1,0.05))+
    #   scale_fill_gradient(aes(Tau), low="#56B1F7", high="#132B43", limits=c(0, 1)) +
    #   theme_void()+
    #   theme(legend.position = "bottom",
    #         strip.text = element_blank())
    
    #
    # egg::ggarrange(p3,p4, widths = c(12, 1))
    
  }
  output$heatmap <- renderPlot({
    
    heatmap_plot(circle_size=1, axis_size=14)
    
  })
  
  
  # output$Tau <- renderPlot({
  #
  #   ggplot(heatmap_sum(), aes(x=Database, y=Tau, fill=Tau))+
  #     # geom_tile()+
  #     stat_bin2d(aes(fill = Tau), binwidth = c(3,0.05))+
  #
  #     scale_fill_gradient(aes(Tau), low="#56B1F7", high="#132B43") +
  #     geom_hline(aes(yintercept = 0.85),linetype = "dashed", size = 1.4, color="#1261A0")
  #
  # })
  
  
  
  # -----------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  # ----------------------------- O U T P U T  V A R I A B L E S ----------------------------------
  # -----------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  
  observeEvent(input$reset_input, {
    reset("reset")
  })
  # 
  # geneName_doc <- eventReactive(input$Submit, {     #Variable created to avoid printing new gene name at the Downloadable report without having 'Submit' the query
  #   geneName_doc <- input$geneName
  #   return(geneName_doc)
  #   
  # })
  
  # output$gene_name <- renderText({
  #   input$geneName
  # })
  
  # texto <- renderText({
  #   geneID$GENENAME[geneID$SYMBOL == input$geneName]
  # })
  
  output$gene_descr <- renderText({
    unique(geneID$GENENAME[geneID$SYMBOL == input$geneName])
  })
  #
  

  
  # # text output
  # output$text <- renderText({
  #   text_reactive()
  # })
  
  
  output$gene_descr2 <- output$gene_descr3 <- output$gene_descr4 <- renderText({
    # output$gene_descr2 <- output$gene_descr3 <- output$gene_descr4 <- renderText({
    paste0(unique(gene_description_reactive()), ": ", unique(geneID$GENENAME[geneID$SYMBOL == gene_description_reactive()]))
  })
  
  output$tissue2 <- output$tissue3 <- output$tissue4 <- renderText({
    paste0(input$tissue, ",")
  })
  
  # output$tissues <- renderText({
  #   input$tissue
  # })
  
  
  # output$graph_HPA <- renderPlot({
  #   graph(E_gene2())
  # })
  #
  
  plotGTEx <- eventReactive(input$Submit, {
    
    ##Added intermidiate step to avoid updating plots when selecting tissues without Submit
    graph(E_gene1())
    
  })
  
  output$graph_GTEx <- renderPlot({
    plotGTEx()
  })
  
  
  plotHPA <- eventReactive(input$Submit, {
    graph(E_gene2())
    
  })
  
  output$graph_HPA <- renderPlot({
    plotHPA()
  })
  
  
  plotBarkBase <- eventReactive(input$Submit, {
    graph(E_gene3())
    
  })
  
  output$graph_BarkBase <- renderPlot({
    plotBarkBase()
  })
  
  
  
  
  macaque_DB <- eventReactive(list(input$NHPRTR_merge, input$Submit), {
    switch(input$NHPRTR_merge,
           NHPRTR_all_mac  = graph(E_gene18()),
           NHPRTR_CMC = graph(E_gene4()),
           NHPRTR_CMM = graph(E_gene5()),
           NHPRTR_JMI = graph(E_gene19()),
           NHPRTR_RMI = graph(E_gene6()))
  })
  
  
  # plot_macaque <- eventReactive(input$Submit, {
  #   macaque_DB()
  # })
  
  output$graph_macaque <- renderPlot({
    macaque_DB()
  }) 
  
  
  
  
  
  # plotMAC <- eventReactive(input$Submit, {
  #   graph(E_gene18())
  # })
  # 
  # output$graph_MAC <- renderPlot({
  #   plotMAC()
  # })
  # 
  # plotCMC <- eventReactive(input$Submit, {
  #   graph(E_gene4())
  # })
  # 
  # output$graph_CMC <- renderPlot({
  #   plotCMC()
  # })
  # 
  # plotCMM <- eventReactive(input$Submit, {
  #   graph(E_gene5())
  # })
  # 
  # output$graph_CMM <- renderPlot({
  #   plotCMM()
  # })
  # 
  # plotRMI <- eventReactive(input$Submit, {
  #   graph(E_gene6())
  # })
  # 
  # output$graph_RMI <- renderPlot({
  #   plotRMI()
  # })  
  # 
  # 
  # plotJMI <- eventReactive(input$Submit, {
  #   graph(E_gene19())
  # })
  # 
  # output$graph_JMI <- renderPlot({
  #   plotJMI()
  # })
  
  # plotBioGPS_Mouse <- eventReactive(input$Submit, {
  
  plotBioGPS_Mouse <- eventReactive(c(input$probeBioGPS_Mouse, input$Submit), {
    #Added logic to execute automatically when probe is updated.
    graph(E_gene7())
  })
  
  output$graph_BioGPS_Mouse <- renderPlot({
    plotBioGPS_Mouse()
  })
  
  
  plotBioGPS_Mouse2 <- eventReactive(c(input$probeBioGPS_Mouse2, input$Submit), {
    graph(E_gene8())
  })
  
  output$graph_BioGPS_Mouse2 <- renderPlot({
    plotBioGPS_Mouse2()
  })
  
  plotBioGPS_Human <- eventReactive(c(input$probeBioGPS_Human, input$Submit), {
    graph(E_gene9())
  })
  
  output$graph_BioGPS_Human <- renderPlot({
    plotBioGPS_Human()
  })
  
  
  plotRatlas <- eventReactive(input$Submit, {
    graph(E_gene10())
  })
  
  output$graph_Ratlas <- renderPlot({
    plotRatlas()
  })
  
  plotMinipig_Atlas <- eventReactive(input$Submit, {
    graph(E_gene11())
  })
  
  output$graph_Minipig_Atlas <- renderPlot({
    plotMinipig_Atlas()
  })
  
  plotMouse_MTAB6081 <- eventReactive(input$Submit, {
    graph(E_gene12())
  })
  
  output$graph_Mouse_MTAB6081 <- renderPlot({
    plotMouse_MTAB6081()
  })
  
  plotRat_MTAB6081 <- eventReactive(input$Submit, {
    graph(E_gene13())
  })
  
  output$graph_Rat_MTAB6081 <- renderPlot({
    plotRat_MTAB6081()
  })
  
  plotDIS_Monkey <- eventReactive(c(input$probeDIS_Monkey, input$Submit), {
    graph(E_gene14())
  })
  
  output$graph_DIS_Monkey <- renderPlot({
    plotDIS_Monkey()
  })
  
  plotDIS_Dog <- eventReactive(c(input$probeDIS_Dog, input$Submit), {
    graph(E_gene15())
  })
  
  output$graph_DIS_Dog <- renderPlot({
    plotDIS_Dog()
  })
  
  plotDIS_Rat <- eventReactive(c(input$probeDIS_Rat, input$Submit), {
    graph(E_gene16())
  })
  
  output$graph_DIS_Rat <- renderPlot({
    plotDIS_Rat()
  }) 
  
  plotDIS_Mouse <- eventReactive(c(input$probeDIS_Mouse, input$Submit), {
    graph(E_gene17())
  })
  
  output$graph_DIS_Mouse <- renderPlot({
    plotDIS_Mouse()
  })
  
  
  plotDIS_Human <- eventReactive(c(input$probeDIS_Human, input$Submit), {
    graph(E_gene20())
  })
  
  output$graph_DIS_Human <- renderPlot({
    plotDIS_Human()
  })
  
  
  
  
  output$tissueExp <- DT::renderDataTable(
    DB_summary(),
    # data.table(DB_summary()) %>%   formatStyle(0, target = "row", backgroundColor = styleEqual(which(DB_summary()$`Mean Expression` < 10), "red")),
    options = list(
      dom = 't',
      pageLength = 25,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
        "}"
      ),
      autoWidth = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = 1:5
      ))
    ),
    rownames = FALSE
    
  )
  
  output$tissueSpecies2 <- DT::renderDataTable(
    tissueSpecies(),
    options = list(
      "pageLength" = 100,
      dom = 't',
      
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
        "}"
      ),
      autoWidth = FALSE
      # columnDefs = list(list(className = 'dt-center', targets = 1:2))
    ),
    rownames = FALSE
    
    
  )
  
  output$speciesTissue2 <- DT::renderDataTable(
    speciesTissue(),
    options = list(
      dom = 't',
      
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
        "}"
      ),
      autoWidth = FALSE
      # columnDefs = list(list(className = 'dt-center', targets = 1:2))
    ),
    rownames = FALSE
    
    
  )
  
  
  
  output$geneExp_GTEx <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene1())
    }
  } , rownames = FALSE)
  
  output$geneExp_HPA <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene2())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_BarkBase <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene3())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_MAC <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene18())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_CMC <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene4())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_CMM <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene5())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_RMI <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene6())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_JMI <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene19())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_BioGPS_Mouse <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene7())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_BioGPS_Mouse2 <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene8())
      
    }
  } , rownames = FALSE)
  
  
  output$geneExp_BioGPS_Human <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene9())
      
    }
  } , rownames = FALSE)
  
  output$geneExp_Ratlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene10())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_Minipig_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene11())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_Mouse_MTAB6081 <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene12())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_Rat_MTAB6081 <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene13())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_Monkey_DIS_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene14())
    }
  } , rownames = FALSE)
  
  output$geneExp_Dog_DIS_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene15())
    }
  } , rownames = FALSE)
  
  
  output$geneExp_Rat_DIS_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene16())
    }
  } , rownames = FALSE) 
  
  output$geneExp_Mouse_DIS_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene17())
    }
  } , rownames = FALSE)
  
  output$geneExp_Human_DIS_Atlas <- renderDataTable({
    if (input$geneName != "Select Gene") {
      singleDB(E_gene20())
    }
  } , rownames = FALSE)
  
  
  
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
    
  })
  
  
  
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #----------------------------------- W E L C O M E  P A G E S -----------------------------------
  # -----------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  DB_Used <- reactive({
    
    GTEx <- c("GTEx", "v8", "Homo sapiens", "58'156", "52", "17'382", "976'868'400")
    HPA  <- c("HPA", "v 19.3", "Homo sapiens", "22'643", "56", "281", "6'362'683")
    BioGPS <- c("BioGPS", "Aug '18", "Homo sapiens", "14'590", "66", "176", "4'288'768")
    DIS_Atlas_Dog <- c("DIS Atlas Dog", "-", "Canis familiaris", "18'348", "96", "1'026", "44'153'910")
    BarkBase <- c("BarkBase", "June '19", "Canis familiaris", "13'631", "27", "131", "1'804'263")
    DIS_Atlas_Monkey <- c("DIS Atlas Monkey", "-", "Nonhuman primate", "22'468", "119", "2'113", "115'528'275")
    NHPRTR_CMC <- c("NHPRTR CMC", "Sept '14", "Nonhuman primate", "26'204", "15", "16", "419'264")
    NHPRTR_CMM <- c("NHPRTR CMM", "Sept '14", "Nonhuman primate", "26'204", "14", "15", "393'060")
    NHPRTR_JMI <- c("NHPRTR JMI", "Sept '14", "Nonhuman primate", "26'204", "14", "14", "366'856")
    NHPRTR_RMI <- c("NHPRTR RMI", "Sept '14", "Nonhuman primate", "26'204", "14", "14", "366'856")
    DIS_Atlas_Mouse <- c("DIS Atlas Mouse", "-", "Mus musculus", "20'665", "40", "2'598", "117'172'398")
    BioGPS_GNF1M <- c("BioGPS (GNF1M)", "Aug '18", "Mus musculus", "10'755", "60", "153", "2'104'362")
    BioGPS_MOE403 <- c("BioGPS (MOE403)", "Feb '19", "Mus musculus", "14'528", "82", "189", "5'519'556")
    Mouse_MTAB6081 <- c("Mouse_MTAB6081", "-", "Mus musculus", "35'053", "13", "39", "1'367'067")
    DIS_Atlas_Rat <- c("DIS Atlas Rat", "-", "Rattus norvegicus", "15'578", "122", "9'144", "284'369'256")
    Ratlas <- c("Ratlas", "-", "Rattus norvegicus", "29'497", "37", "219",  "4'483'759")
    Rat_MTAB6081 <- c("Rat_MTAB6081", "-", "Rattus norvegicus", "24'054", "13", "39", "938'106")
    Minipig_Atlas <- c("Minipig Atlas", "-", "Sus scrofa domesticus", "10'038", "38", "248", "3'939'769")
    DIS_Atlas_Human <- c("DIS Atlas Human", "-", "Homo sapiens", "22'468", "53", "323", "17'660'025")
    
    DB_Used <-data.frame(rbind(DIS_Atlas_Human, GTEx, HPA, BioGPS, 
                               DIS_Atlas_Monkey, NHPRTR_CMC, NHPRTR_CMM, NHPRTR_JMI, NHPRTR_RMI, 
                               DIS_Atlas_Dog, BarkBase, 
                               Minipig_Atlas,
                               DIS_Atlas_Rat, Ratlas, Rat_MTAB6081,
                               DIS_Atlas_Mouse, BioGPS_GNF1M, BioGPS_MOE403, Mouse_MTAB6081), 
                         
                         stringsAsFactors = FALSE)
    
    names(DB_Used) <- c("Database", "Version", "Species", "Genes", "Tissues", "Samples", "Registers")
    
    DB_Used
    
  }) 
  
  output$aboutDB <- DT::renderDataTable(
    DB_Used(),
    
    options = list(
      dom = 't',
      pageLength = 25,
      
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
        "}"
      ),
      autoWidth = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = 1:5
      ))
    ),
    rownames = FALSE
    
  )
  
  
  # -----------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  # -------------------------------------- D O W N L O A D ----------------------------------------
  # -----------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  
  
  # E_gene_list <- reactive({
  #   E_gene_list <- list(E_gene1(), E_gene2(), E_gene3())
  #   DB_final <- as.vector(DB_summary()$Databases)
  #   names(E_gene_list) <- c(deparse(substitute(E_gene1())), deparse(substitute(E_gene2())), deparse(substitute(E_gene3())))
  #   # db.index <- match(DB_final, DBs)
  # })
  
  # E_gene_index <- function(){        #Function to obtain the E_gene"x" indexes
  # 
  #   
  #   if("All" %in% input$database){ DBs2 <- DBs } else { DBs2 <- input$database }
  #   E_gene_index <- match(as.vector(DBs2), as.vector(DBs))
  #   
  #   return(E_gene_index)
  # }

  
  # geneName_doc <- eventReactive(input$Submit, {     #Variable created to avoid printing new gene name at the Downloadable report without having 'Submit' the query
  #   geneName_doc <- gene_description_reactive()
  #   return(geneName_doc)
  #   
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d"), "_", gene_description_reactive(), sep = '.', switch(
        input$format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      withProgress(message = 'Rendering, please wait!',
                   detail = 'Progress bar will disappear when finished...',{

                     src <- normalizePath('report2.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'report2.Rmd', overwrite = TRUE)
                     params <- list(
                       n = E_gene1()$tpm,
                       gene = gene_description_reactive(),
                       DB_selected = input$database,
                       microarrayCutoff = input$microarray_cutoff,
                       ZScoreCutoff = as.numeric(input$ZScore_cutoff),
                       macaque_select = input$NHPRTR_merge,
                       TEP_Version = TEP_Version,
                       species_selected = input$species
                     )
                     
                     library(rmarkdown)
                     out <- render('report2.Rmd', switch(
                       input$format,
                       PDF = pdf_document(number_sections = TRUE),
                       HTML = html_document(number_sections = TRUE),
                       Word = word_document(reference_docx = "~/Carlos/TEP/template.docx")   ####VERY IMPORTANT TO USE TEMPLATE (FULL PATH)
                     ))
                     file.rename(out, file)
                   })
    }
  )
  

   ############################################################################################
   ############################################################################################

   output$dl <- downloadHandler(

     filename = function() {
       
       paste0(paste0(format(Sys.time(), "%Y-%m-%d"), "_", gene_description_reactive(),"_raw_data", ".zip"))
     },
     
     content = function(file) {
        # browser()
       # df <- mget(ls(pattern = "E_gene\\d+\\()"))
       # df <- list(E_gene1(), E_gene2(), E_gene3())
       df <- list(GTEx = E_gene1(), HPA = E_gene2(), BarkBase = E_gene3(), NHPRTR_CMC = E_gene4(), NHPRTR_CMM = E_gene5(), NHPRTR_RMI = E_gene6(),
                  BioGPS_Mouse = E_gene7(), BioGPS_Mouse2 = E_gene8(), BioGPS_Human = E_gene9(), Ratlas = E_gene10(), Minipig_Atlas = E_gene11(),
                  Mouse_MTAB6081 = E_gene12(), Rat_MTAB6081 = E_gene13(), DIS_Atlas_Monkey = E_gene14(), DIS_Atlas_Dog = E_gene15(), DIS_Atlas_Rat = E_gene16(),
                  DIS_Atlas_Mouse = E_gene17(), NHPRTR_all_mac = E_gene18(), NHPRTR_JMI = E_gene19(), DIS_Atlas_Human = E_gene20())
       
       list_with_nrow <- sapply(df, function(x) nrow(x)>0)   #Remove DBs with 0 rows even if they were selected
       df <- df[list_with_nrow]
       
       DB_selected <- DB_summary()$Databases
       non_selected_DB <- setdiff(names(df), DB_selected)
       
       df[non_selected_DB] <- NULL
       
       fs <- c()
       
       # current_dir <- "/home/sanchcat/download/"
       # setwd(current_dir)

       # tmpdir <- tempdir()
       setwd(tempdir())
       
       for (i in 1:length(names(df))){
         path <- paste0(names(df[i]), ".csv")    #Define file name
         write.csv(df[[i]], path)                #write data at previous file name
         fs <- c(fs,path)                        #store list of files to be zipped
         
       }
       
       file.copy(from = "/home/sanchcat/Carlos/TEP/code/report2.Rmd", to = "report2.Rmd", overwrite = TRUE)
       
       fs <- c(fs,"report2.Rmd")
       # browser()
       zip(zipfile=file, files=fs)
     },
     
     contentType = "application/zip"
     
   )
  
    
    

   
   ############################################################################################   
   ############################################################################################
   
   
   
   
   
   
   
   
   
   
  # output$dl_old <- downloadHandler(
  #   
  #   filename = function() {
  #     paste0(paste0(format(Sys.time(), "%Y-%m-%d"), "_", input$geneName,"_raw_data", ".xlsx"))
  #   },
  #   
  #   content = function(file) {
  #     
  #     E_gene_df <- list(E_gene19(), E_gene20()) #woks well if limited number of DB. If not, memoryError in java.lang
  # 
  # 
  #     write.xlsx(E_gene1(), file, sheetName = "GTEx", row.names=FALSE)       #the 3 write.xlsx work well creating 3 sheets
  #     write.xlsx(E_gene2(), file, sheetName = "HPA", row.names=FALSE, append = TRUE)         #the 3 write.xlsx work well creating 3 sheets
  #     write.xlsx(E_gene3(), file, sheetName = "BarkBase", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene4(), file, sheetName = "NHPRTR_Cyno_Chinese", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene5(), file, sheetName = "NHPRTR_Cyno_Mauritian", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene6(), file, sheetName = "NHPRTR_Rhesus_Indian", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene19(), file, sheetName = "NHPRTR_Japanese_mac", append = TRUE)
  #     write.xlsx(E_gene7(), file, sheetName = "BioGPS_M", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene8(), file, sheetName = "BioGPS_M2", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene9(), file, sheetName = "BioGPS_H", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene10(), file, sheetName = "Ratlas", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene11(), file, sheetName = "Minipig_Atlas", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene12(), file, sheetName = "Mouse_MTAB6081", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene13(), file, sheetName = "Rat_MTAB6081", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene14(), file, sheetName = "DIS_Monkey", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene15(), file, sheetName = "DIS_Dog", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene16(), file, sheetName = "DIS_Rat", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene17(), file, sheetName = "DIS_Mouse", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene18(), file, sheetName = "NHPRTR_all_mac", row.names=FALSE, append = TRUE)
  #     write.xlsx(E_gene20(), file, sheetName = "DIS_Human", row.names=FALSE, append = TRUE)
  # 
  # 
  #   }
  # )
  
}


# -----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#--------------------------------------- F O R M U L E S ----------------------------------------
# -----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

# output$tau <- renderUI({
#
#   withMathJax(
#     helpText('Tau
#                $$\tau =\frac{\sum_{i=1}^{n}(1-{\hat{x_i}})}{n-1}  ; \hspace{1cm}  \hat{x_i} = \frac{x_i}{max _{1\leq i\leq n}(x_i)}$$'))
#
# })

# Run the application
shinyApp(ui = ui, server = server)

