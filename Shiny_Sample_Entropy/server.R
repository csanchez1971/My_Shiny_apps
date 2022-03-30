

library(shiny)

shinyServer(function(input, output, session) {
  
  filename_new <- reactive(input$file$name)
  filename_old <- eventReactive(input$plot_button, {
    filename_old <- input$file$name
    
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    x <- input$Variable_to_SE
    updateCheckboxGroupButtons(session, "selectVariables", selected = x)
  })
  
  vals <- reactiveValues(data = NULL)
  percentage_NA <- reactiveVal(0)

  dataModal <- function() {
    modalDialog(
      title = "Warning: Large gap detected in data",
      "Measurement contains very large gaps, check quality of data before proceeding with the sample entropy calculation!",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Discard measurement"),
        actionButton("ok", "Continue analysis")
      )
    )
  }
  
  
  data0 <- eventReactive(input$plot_button,{
          # browser()
          # if (input$t25Minutes){longest_gap_tolerated=Inf} else {longest_gap_tolerated=1500}
          if (is.null(input$file))
                  return()
          
          if (input$Variable_to_SE %in% input$selectVariables){
                  inFile <- input$file
                  data0 <- read.csv(inFile$datapath)
                  data0 <- AdjustColumnNames(data0)
                  data0 <- FormattingOfMeasurements(data0)
                  data0$na <- ifelse(is.na(data0[,input$Variable_to_SE]), 'y', 'n')
                  
                  if(input$interpolate){
                          
                          data0[,input$Variable_to_SE] <- na_interpolation(data0[,input$Variable_to_SE], option = "spline")
                          
                  } 
                  
                  if(input$couples_09_105){
                          
                          data <- SamplingFrequencyStats(data0,longest_gap_tolerated=1500, lower_limit = 0.9, upper_limit = 1.06)
                          
                  } else {
                          
                          data <- SamplingFrequencyStats(data0,longest_gap_tolerated=1500)
                          
                  }
                  
                  
                  if(identical(data[1,2], "t > 25 minutes")){
                          showModal(dataModal())
                          
                          
                           data <- data0
                          
                  } else{
                          data <- data %>% select(-time_diff)
                          data$group <- ifelse(apply(as.data.frame(data[, colnames(data) %in% input$selectVariables]), 1, anyNA), "Missing Value", "Valid Range")
                          data$colgroup <- ifelse(data$group=="Valid Range" & data$na=="y", "Corrected", data$group)
                          data$na <- NULL
                          
                  }
                  
                  return(data)
                  
          } else {
                  
                  showModal(modalDialog(
                          title = "Variable of study not selected",
                          paste0("Selected variables to display missing values must include ", input$Variable_to_SE)
                  ))
          }
  })
  
  observeEvent(ncol(data0())== 7,{   #only have the 6th column when pressed "Plot button" before
    vals$data <- data0()
  })

    observeEvent(input$ok, {    # Case when we continue with the analysis even though a t>25 minutes gap found
      
      data <- data0()
      
      
       if(input$couples_09_105){
         data <- SamplingFrequencyStats(data,longest_gap_tolerated=Inf, lower_limit = 0.9, upper_limit = 1.06)
         
       } else {
               
       data <- SamplingFrequencyStats(data,longest_gap_tolerated=Inf)
         
       }
      
      data <- data %>% select(-time_diff)
      data$group <- ifelse(apply(as.data.frame(data[, colnames(data) %in% input$selectVariables]), 1, anyNA), "Missing Value", "Valid Range")
      data$colgroup <- ifelse(data$group=="Valid Range" & data$na=="y", "Corrected", data$group)
      data$na <- NULL
      
      vals$data <- data
      removeModal()
      })

    
    
    
    observeEvent(data0(), {
      perc_NA <- sum(is.na(data0()[,input$Variable_to_SE]))/nrow(data0())
      percentage_NA(perc_NA)
    })
    
  #Display file description
  output$filenameText  <- renderText({
    paste("Filename:", (input$file$name))
  })
  
  
  output$calcEntropy <- renderUI({
    req(input$plot_button, subset_data())
    req(filename_new() == filename_old())
    
    actionButton("SE_button", "Calculate Sample Entropy")
  })
  
  
  output$download_button <- renderUI({
    req(input$plot_button, SE_results())
    req(filename_new() == filename_old())
    
    downloadButton("download1", "Download Results")
  }) 
  
  output$download_merged <- renderUI({
    if(is.null(input$files_to_merge))
      return()
    
    downloadButton("download2", "Download merged file")
  }) 
  
  # plot -------------
  output$plot <- renderPlot({

    req("group" %in% names(vals$data))
    req(filename_new() == filename_old())
    
    myColors <- c("Valid Range"="#00BFC4", "Corrected"="#0905f0", "Missing Value"="#F8766D") 
    # names(myColors) <- c("Valid Range", "Missing Value", "OTHER")
    # names(myColors) <- c("n", NA, "y")
    
    # ggplot(vals$data, aes(x = Date.Time, y =  group, color= group)) +
    ggplot(vals$data, aes(x = Date.Time, y =  group, color= colgroup)) +
            scale_colour_manual(values = myColors) +
      geom_point() +
      labs(x = "Date and Time", y = "Data Quality", colour = "Data Quality") +
      coord_cartesian(xlim = ranges$x, ylim = NULL, expand = TRUE) 

    
  }, height = 200)
  
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(as_datetime(brush$xmin, tz= "Europe/Berlin"), as_datetime(brush$xmax, tz= "Europe/Berlin"))
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  data_range <- reactive({
       
    if (is.null(input$file))
      return()
    req(nrow(vals$data)>0)
    data_range_real <- interval_no_interruptions(subset_data(), input$selectVariables)
    sub_data <- subset_data()
    data_range <- data.frame(Selected_Initial_Time = as.character(first(subset_data()$Date.Time)),
                             Selected_Final_Time = as.character(last(subset_data()$Date.Time)),
                             Selected_duration = as.character(round(as.duration(last(subset_data()$Date.Time) - first(subset_data()$Date.Time)),2)),
                             Effective_Initial_Time = as.character(first(data_range_real$Date.Time)),
                             Effective_Final_Time = as.character(last(data_range_real$Date.Time)),
                             Effective_duration = round(as.duration(last(data_range_real$Date.Time) - first(data_range_real$Date.Time))/60,2),
                             percentage_NA_brush = ifelse(nrow(sub_data)==0, 0, round(sum(is.na(sub_data[,input$Variable_to_SE]))/nrow(sub_data)*100,2)))
    colnames(data_range) <- c("Selected Initial Time", 
                              "Selected Final Time", 
                              "Selected Duration", 
                              "Effective Initial Time",
                              "Effective Final Time", 
                              "Effective Duration (min)", 
                              "% of NAs in selection")
    return(data_range) # if not included, system will convert into a vector and give error
  })
  
  output$plot_dblclickinfo <- renderDataTable({
    if (is.null(input$file))
      return()
    req(filename_new() == filename_old())    # Requires that we have click on the "Display Data" button, to avoid loading a new file but not displayed
    datatable(data_range(),
              options = list(
                dom = 't',
                ordering=F,    #Remove sorting values
                columnDefs = list(list(className = 'dt-center', targets ="_all")),   #Center fields in header
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#35a7e8', 'color': 'black'});",
                  "}"),
                language = list(
                  zeroRecords = "No window selected"),
                autoWidth = TRUE), rownames = FALSE) %>% 
      formatStyle(('Effective Duration (min)'),
                  color = 'white',
                  backgroundColor = styleInterval(70, c('red', 'green'))) %>%
      formatStyle(columns = c(1:6), 'text-align' = 'center')    #Values centered
      

  })
  
  
  output$plotUI <- renderUI({
    if (is.null(input$selectVariables))
      return()
    
    withSpinner(plotOutput("plot", height = 200,
                           # click = "plot_click",
                           dblclick = "plot_dblclick",
                           brush = brushOpts(
                             id = "plot_brush",
                             direction = "x",
                             resetOnNew = TRUE)), type = 4)
    
  })
  
  
  output$range_selected <- renderUI({
    req(input$plot_button)
    req(nrow(subset_data())>0)
    
    dataTableOutput("plot_dblclickinfo")
    
  })
  
  subset_data <- reactive({
   # browser()
    subset_data <- brushedPoints(vals$data, input$plot_brush)
    
     return(subset_data)
  })
  
  
  # Sample Entropy ----------------------------------------------------------
  
  
  SE_results <- eventReactive(input$SE_button,{
    req(nrow(subset_data()) > 0)
    withProgress(message = paste('Calculating sample entropies for', input$Variable_to_SE), detail = 'This may take about 10 to 20 seconds.', value = 0.7, {
       
      entropy_results <- calculateEntropies(subset_data(), VariableName = input$Variable_to_SE, selectVariables = input$selectVariables)
      entropy_results$Effective_Initial_Time_Point <- as.character(entropy_results$Effective_Initial_Time_Point)
      entropy_results$Effective_Final_Time_Point <- as.character(entropy_results$Effective_Final_Time_Point)
      entropy_results$Total_Duration_of_Measurements <- as.duration(last(vals$data$Date.Time) - first(vals$data$Date.Time))
      # entropy_results$Date_And_time <- first(data()$Date.Time)
      File_Name <- input$file$name
      Patient_ID <- gsub("(HARVEY_[FT]G)_([a-zA-Z0-9]+).*", "\\1_\\2", File_Name)
      entropy_results <- cbind(File_Name, Patient_ID, entropy_results)
      
      colnames(entropy_results)[4:7] <- c("Entropy_Of_Top_Part_Of_Selected_Window",	"Entropy_Of_Middle_Part_Of_Selected_Window",
                                          "Entropy_Of_Bottom_Part_Of_Selected_Window",	"Entropy_Of_Whole_Selected_Window")
      return(entropy_results)
    
    })  })
  
  output$SE_calculations <- renderDataTable({
    req(input$SE_button)
    req(nrow(subset_data()) > 0)

    sub_data <- subset_data()
    req(nrow(SE_results()) > 0)
    

    if(data_range()$'Effective Duration (min)'<70){
      # error_message <- data.frame("Select a time window with effective duration of at least 70 minutes")
      error_message <-  data.frame(rbind("It is recommended to select a time window with effective duration of at least 70 minutes", 
                                         "Please select a new window or 'Download Result' if you want to continue with this measurement"))
      # colnames(error_message) <-  "Error in time window"
      colnames(error_message) <-  "Warning in time window"
      datatable(error_message,options = list(
        dom = 't',
        ordering=F,    #Remove sorting values
        columnDefs = list(list(className = 'dt-center', targets ="_all")),  #Center fields in header
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#35a7e8', 'color': 'black'});",
          "}"),
        autoWidth = FALSE), rownames = FALSE)
        
      } else {

          SE_results2 <- SE_results()[4:11]
          SE_results2$Effective_Duration <- as.character(round(SE_results2$Effective_Duration),3)
          SE_results2$Total_Duration_of_Measurements <- as.character(round(SE_results2$Total_Duration_of_Measurements),3)
          colnames(SE_results2) <- c("Top Part", "Middle Part", "Bottom Part", "Whole", "Effective Initial Time", "Effective Final Time", "Effective Duration", 
                                     "Total Duration of Measurement")
          datatable(SE_results2,   #Avoid printing 3 first columns (only in download doc) - "File_Name", "Patient_ID", "Date_and_Time"
                    options = list(
                      dom = 't',
                      ordering=F,    #Remove sorting values
                      columnDefs = list(list(className = 'dt-center', targets ="_all")),  #Center fields in header
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#35a7e8', 'color': 'black'});",
                        "}"),
                      autoWidth = FALSE), rownames = FALSE) %>%  
            formatStyle(columns = c(1:8), 'text-align' = 'center') %>%    #Values centered
            
            formatRound(c(1:4), 3) 
        
     
      }

  })
  
  
  upload <- eventReactive(input$files_to_merge,{
    File_to_merge <- input$files_to_merge
    
    req(File_to_merge)
    upload = list()
    
    for(nr in 1:length(File_to_merge[, 1])){
      upload[[nr]] <- read.csv(
        file = File_to_merge[[nr, 'datapath']]
      )
    }
    upload <- bind_rows(upload)
    upload <- cbind(File_to_merge[, 1], upload)   # Adaptation to pass the new file name into the column instead generic (avoid case 1 dataset calculates 2 SE)
    upload <- upload[,-2]
    colnames(upload)[1] <- "File_Name"
    return(upload)
    
  })
  
  output$contents <- renderTable({
    
    upload()
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {
      gsub(".csv", paste0("_SE_", input$Variable_to_SE, ".csv"), input$file)
    },
    content = function(file) {
      write.csv(SE_results(), file, row.names = FALSE)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      "Summary_Sample_Entropy_24h.csv"
    },
    content = function(file) {
      write.csv(upload(), file, row.names = FALSE)
    }
  )
  
  output$percentage_NA <- renderText({
    paste0("Percentage of NAs: ",round(percentage_NA()*100,2), " %")
    
  })
  
})
