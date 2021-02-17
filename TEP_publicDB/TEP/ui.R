

library(shiny)


shinyUI(navbarPage("Menu",
                   theme = shinytheme("cerulean"),
                   
    tabPanel("Home", icon = icon("home"),
           
             
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
                     style = "background: white",
                     
                     tabsetPanel(
                         
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
    
    
    
    tabPanel("TEP", icon = icon("analytics"),
             br(),
             
             sidebarLayout(
                 sidebarPanel(
                     h2("Target Expression Profiling"),
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
                                         `Homo sapiens` = list("GTEx", "Human Protein Atlas" = "HPA", "BioGPS" = "BioGPS_Human"),
                                         `Nonhuman primate` = list("NHPRTR"),
                                         
                                         `Canis familiaris` = list("BarkBase"),
                                         
                                         `Rattus norvegicus` = list("Rat_MTAB6081"),
                                         
                                         `Mus musculus` = list("BioGPS (GNF1M)" = "BioGPS_Mouse", 
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
                         
                         fluidRow(
                             column(6,
                                    prettyRadioButtons('ZScore_cutoff', 'Select ZScore to filter:',
                                                       choices = list(0,1,2,3),
                                                       inline = TRUE,
                                                       selected = 2,
                                                       status = "primary")
                             ),
                             
                             column(6,
                                    prettyRadioButtons('scalePlot', 'Select plot scale:',  #Plots can be displayed with y-axis in linear or logarithmic scale
                                                       choices = list('Linear', 'Logarithmic'),
                                                       inline = TRUE,
                                                       selected = 'Linear',
                                                       status = "primary")
                             )
                         )
                     ),
                     
                     br(),
                     
                     fluidRow(
                         column(6,
                                # tags$style(type = "text/css", "#Submit {background-color:#337ab7; color: white !important}"),   #Color the Submit button in blue
                                actionButton(inputId = "Submit", label = "Submit gene")
                         ),    
                         
                         column(6, 
                                # tags$style(type = "text/css", "#reset_input {background-color:#337ab7; color: white !important}"),
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
                     
                     
                     
                     fluidRow(
                         column(6,
                                # tags$style(type = "text/css", "#download {background-color:#337ab7; color: white !important}"),   #Color the Submit button in blue
                                downloadButton("download", label = "Download", class = "butt")
                         ),    
                         
                         column(6, 
                                # tags$style(type = "text/css", "#dl {background-color:#337ab7; color: white !important}"),
                                downloadButton("dl","Download raw data")                              
                         )
                         
                     ),
                     
                     
                     br(),
                     br()
                 ),
                 
                 mainPanel(width = 9, align = "center", style = "background: white",
                           
                           tabsetPanel(
                               
                               tabPanel("Homo sapiens",   #Different plots arragend by species in different tabs
                                        

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
                                        
                               ),
                               
                               
                               tabPanel("Canis familiaris",
                                        
                                        conditionalPanel(
                                            condition = "(input.database.includes('BarkBase') || input.database.includes('All')) && input.geneName!='Select Gene'
                                 && (input.species.includes('Canis familiaris') || input.species.includes('All'))",
                                            
                                            br(),
                                            h4("Dog: BarkBase DB, RNAseq"),
                                            br(),
                                            
                                            plotOutput("graph_BarkBase", height=600) %>% withSpinner(color = "#187bcd")
                                        )
                                        
                               ),
                               
                               
                               tabPanel("Rattus norvegicus",
                                        
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
    
    
    
    tabPanel("Heatmap", icon = icon("braille"),
             
             fluidRow(                                                                 #In this row it is plotted the gene name and description, tissues selected and ZScore
                 style = "background-color:#cce6ff;",
                 column(1),
                 column(4, 
                        h4("Selected Gene"), 
                        textOutput("gene_descr2")),
                 
                 column(5,
                        h4("Selected tissue system/s"),
                        textOutput("tissue2")
                 ),
                 
                 column(2,                                                                       #Include selection of ZScore threshold for the heatmap plot
                        prettyRadioButtons('ZScore_cutoff2', 'Select ZScore to filter:',
                                           choices = list(0,1,2,3),
                                           inline = TRUE,
                                           selected = 2,
                                           status = "primary")
                 )
                 
             ),
             
             br(),
             
             
             plotOutput("heatmap", height = "900px", width = "100%") %>% withSpinner(color = "#187bcd")
             
    ),
    
    tabPanel("Summary", icon = icon("list-ol"),
             
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
    
    tabPanel("Data", icon = icon("database"),
             
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
                                 
                                 column(6, h4("GTEx Summary"),
                                        br(),
                                        dataTableOutput("geneExp_GTEx")),
                                 
                                 column(6, h4("HPA Summary"),
                                        br(),
                                        dataTableOutput("geneExp_HPA"))
                        ),
                        
                        fluidRow(style = "background-color:#e6f3ff;",
                                 
                                 column(6, h4("BioGPS: Human Summary"),
                                        br(),
                                        dataTableOutput("geneExp_BioGPS_Human")))
               ),
               
               tabPanel("Nonhuman primate",
                        
                        fluidRow(style = "background-color:#e6f3ff;",
                                 
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
                                 
                                 column(6, h4("BarkBase Summary"),
                                        br(),
                                        dataTableOutput("geneExp_BarkBase")))
                        
               ),
               

               tabPanel("Rattus norvegicus",

                        fluidRow(style = "background-color:#e6f3ff;",
                                 
                                 column(6, h4("Rat MTAB6081 Summary"),
                                        br(),
                                        dataTableOutput("geneExp_Rat_MTAB6081")))
                        
               ),
               
               tabPanel("Mus musculus",
                        
                        fluidRow(style = "background-color:#e6f3ff;",
                                 
                                 column(6, h4("BioGPS: Mus musculus (GNF1M) Summary"),
                                        br(),
                                        dataTableOutput("geneExp_BioGPS_Mouse")),
                                 
                                 column(6,
                                        h4("BioGPS: Mus musculus (MOE403) Summary"),
                                        br(),
                                        dataTableOutput("geneExp_BioGPS_Mouse2"))
                        ),
                        
                        fluidRow(style = "background-color:#e6f3ff;",
                                 
                                 column(6,
                                        h4("Mouse MTAB6081 Summary"),
                                        br(),
                                        dataTableOutput("geneExp_Mouse_MTAB6081")))
                        
               )
             )
    ),
    
    
    tabPanel("Methods", icon = icon("list-alt"),
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
             
             "The non-human primate reference transcriptome resource (", tags$a(href = "http://nhprtr.org/", "NHPRTR", target = "_blank"), ")",
             
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
             
             tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE1133", "Mouse GNF1M Gene Atlas", target = "_blank"),
             
             h3("Ratlas"),
             
             br(),
             
             h3("Minipig Atlas"),
             
             br(),
             
             "For tissues where several transcripts were available, those with highest mean on the transcript have been selected.",
             
             br(),
             
             h3("Mouse/Rat MTAB6081"),
             
             br(),
             
             tags$a(href = "https://www.nature.com/articles/sdata2017185", "Mouse/Rat MTAB6081 webpage link", target = "_blank"),
             
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
    
    
    tabPanel("Version history", icon = icon("slack-hash"),
             
             
             br(),
             h2("Beta 0.9 August '20"),
             br(),
             
             withTags(
                 ul(
                     li("ZScore filter available in Heatmap"),
                     li("NHPRTR databases merged in one. Posibility of displaying only one DB"),

                 )),    
             
             
             br(),
             h2("Beta 0.8 July '20"),
             br(),
             
             withTags(
                 ul(
                     li("Plots grouped in tabs by species"),
                     li("Logarithmic scale available for plots")
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
                         
                     )
                     
                 )
             )
             
             
    ), 
    
    tabPanel("Session Info", icon = icon("info-square"),
             mainPanel(verbatimTextOutput("sessionInfo"))
             
    )

    ))
