
Sys.setenv("LANGUAGE"="EN")


##################################################################################################
#Function to retrieve data from Labkey
##################################################################################################

GetFolderPathFileNames <- function(MyLabKeySession){
  UniversalListOfContents <- getSchema(MyLabKeySession, "lists")
  return(names(UniversalListOfContents))
}


FolderContent <- function(DurationOfMeasurement){  # "90Mins";"NEW_90Mins";#"Tests";#"90Mins"; "24Hrs";"Tests"
  
  #In the follwing variable you basically store the string with the name of the LabKey folder from which you want to download the data (the "lists")
  # DurationOfMeasurement<-"90Mins";# "NEW_90Mins";#"Tests";#"90Mins"; "24Hrs";"Tests"
  
  folderPath <- paste0("/CaffeineStudy/Data/Measurements/", DurationOfMeasurement);
  
  session<-getSession(baseUrl = baseUrl,folderPath = folderPath);#This is your LabKey session
  
  getFolderPath(session)#With this command you can quickly check whether the LabKey session has been succesfully started
  
  #Retrieve data contained in LabKey-folderPath defined above. To this end, you need two short subroutines:
  
  GetFolderPathFileNames<-function(MyLabKeySession){
    UniversalListOfContents<-getSchema(MyLabKeySession, "lists");
    return(names(UniversalListOfContents));
  }
  
  GetFolderPathContents<-function(MyLabKeySession){
    UniversalListOfContents<-getSchema(MyLabKeySession, "lists");
    NumberOfFilesInFolder<-length(UniversalListOfContents);
    DataInFolder<-vector("list",NumberOfFilesInFolder);
    DataInFolder<-lapply(UniversalListOfContents, getRows, session=MyLabKeySession);
    names(DataInFolder)<-names(UniversalListOfContents);
    return(DataInFolder)
  }
  
  ListNames<-GetFolderPathFileNames(session);#This retrieves the names of the lists located in the LabKey folder 
  
  FolderContent<-GetFolderPathContents(session) #Running this function may take some time, depending on the number of lists stored in the LabKey folder and the size of the lists. The output is a list of data frames
  
  return(FolderContent)
  
}

#Remove temporary files and functions

# rm(session, folderPath, OutputDirectory, AdjustColumnNames, FormattingOfMeasurements, GetFolderPathContents, GetFolderPathFileNames)



##################################################################################################
#Function to rename the column names in a DataFrame
##################################################################################################


AdjustColumnNames<-function(dataFrame){
  colnames(dataFrame) <- mgsub::mgsub(colnames(dataFrame), 
                                      c("Date.*", "RR.*", "HR.*", "Sp.*", "Pulse.*"), 
                                      c("Date.Time", "RR", "HR", "SpO2", "Pulse"))
  
  return(dataFrame)
  
}





##################################################################################################
#Function to Format the values of the datafame, replacing wrong values with "NA"
##################################################################################################


FormattingOfMeasurements <- function(DataFrameOfMeasurements){#This function assumes column names have been standarized using the AdjustColumnNames function 
  IssueWithDateAndTime <- FALSE
  #Here we check for the presence of entirely missing values in the data and replace those by NA
  tryCatch({DataFrameOfMeasurements[DataFrameOfMeasurements==""] <- NA}, error = function(e){
    message('No missing entries in data!')
    print(e)
  }, finally = {message('Moving on to next parsing step...')})
  #Here we check for the presence of question marks in the data and replace those by NA
  tryCatch({DataFrameOfMeasurements[DataFrameOfMeasurements=="?"] <- NA}, error = function(e){
    message('No question marks in data!')
    print(e)
  }, finally = {message('Moving on to next parsing step...')})
  
  VariableNames <- names(DataFrameOfMeasurements)
  
  IndexesOfNumericVariables <- which(!(grepl("Date", VariableNames, ignore.case = TRUE) | grepl("Time", VariableNames, ignore.case = TRUE)))
  #Here we make sure numeric variables are formatted as numeric values (floats) within R
  for (i in IndexesOfNumericVariables){
    
    DataFrameOfMeasurements[, i] <- as.numeric(DataFrameOfMeasurements[, i])
    #Values of zero in the variables SpO2,  HR,  and Pulse are considered unphysiological and thus spurious. Therefore,  such values are replaced by NA
    
    if (!grepl("RR", VariableNames[i])){
      
      RowNumbersOfRowsContainingZeros <- which(DataFrameOfMeasurements[, i] == 0)
      DataFrameOfMeasurements[RowNumbersOfRowsContainingZeros, i] <- NA
    }
  }
  #Here we make sure the date and time are properly formatted within R
  IndexOfDateAndTimeVariable <- which(grepl("Date", VariableNames, ignore.case = TRUE) | grepl("Time", VariableNames, ignore.case = TRUE))
  
  if (length(IndexOfDateAndTimeVariable) > 0){
    
    if (class(DataFrameOfMeasurements[, IndexOfDateAndTimeVariable])=="character"){
      
      DatesAndTimes <- c(rep(origin, nrow(DataFrameOfMeasurements)))
      tz(DatesAndTimes) <- "CET"
      DataFrameOfMeasurements$DatesAndTimes <- DatesAndTimes
      DT  <-  as.data.table(DataFrameOfMeasurements)
      DT[, "DatesAndTimes"] <- DT[, suppressWarnings(ymd_hms(as.character(Date.Time), tz = "CET"))]
      #Here we replace faulty date and time entries in the data by NA
      NumberOfFailedDateParsings <- DT[,  sum(is.na(DatesAndTimes) | DatesAndTimes==ymd(date(origin), tz = "CET"), na.rm =TRUE)]
      
      if (NumberOfFailedDateParsings > 0){
        
        DT[is.na(DatesAndTimes) | DatesAndTimes==ymd(date(origin), tz = "CET"), "DatesAndTimes"] <- DT[is.na(DatesAndTimes) | DatesAndTimes==ymd(date(origin), tz = "CET"), suppressWarnings(ymd_hm(as.character(Date.Time), tz = "CET"))]
        NumberOfFailedDateParsings <- DT[,  sum(is.na(DatesAndTimes) | DatesAndTimes==ymd(date(origin), tz = "CET"), na.rm =TRUE)]
        
        if (NumberOfFailedDateParsings > 0){
          
          IssueWithDateAndTime <- TRUE
          warning("Some entries with faulty date or time format!", call. = FALSE)
          DT[DatesAndTimes==ymd(date(origin), tz = "CET"), "DatesAndTimes"] <- NA
        }
      }
      
      DataFrameOfMeasurements <- as.data.frame(DT)
      DataFrameOfMeasurements$Date.Time <- DataFrameOfMeasurements$DatesAndTimes
      DataFrameOfMeasurements$DatesAndTimes <- NULL
      
    }
    
  }else{
    
    warning("Date does not contain date and time information!", call. = FALSE)
    
  }
  
  #return(IssueWithDateAndTime)
  return(DataFrameOfMeasurements)
}





##################################################################################################
#Function to change values to NA if time diff values out of threshold, remove 1 identical row
##################################################################################################


SamplingFrequencyStats <- function(dataFrame,longest_gap_tolerated=1500){
  
  nms <- c("Date.Time", "RR", "HR", "SpO2", "Pulse")   ####Add column if missing                      #### CHANGED 04/09/2020!!!!
  Missing <- setdiff(nms, names(dataFrame))            ####Add column if missing                      #### CHANGED 04/09/2020!!!!
  # dataFrame[Missing] <- paste0("Parameter ", Missing, " was not recorded during this measurement", collapse="")               ####Add text to missing column 
  dataFrame[Missing] <- "Missing Column"
  
  # replace_na <- function(df){
  while(anyNA(dataFrame[1,2:5])){
    column_index <- which(is.na(dataFrame[1,-6]))
    
    if(any(!is.na(dataFrame[2,column_index]))) {
      dataFrame <- dataFrame[-1,]
    } else {
      break
    }
  }
  # dataFrame$cnt_na <- apply(dataFrame[,2:5], 1, function(x) sum(is.na(x)))
  dataFrame$cnt_na <- apply(dataFrame, 1, function(x) sum(is.na(x)))
  dataFrame <- dataFrame %>%
    # arrange(Date.Time) %>%                                          ##OLD VERSION
    # mutate(time_diff = Date.Time - lag(Date.Time)) %>%              ##OLD VERSION
    # filter(time_diff!=0 | is.na(time_diff))                         ##OLD VERSION
    # 
    arrange(Date.Time, is.na(HR), cnt_na)  %>%
    mutate(time_diff = Date.Time - lag(Date.Time)) %>%
    filter(time_diff!=0 | is.na(time_diff))  %>%
    select(-cnt_na)
  
  
  
  
  # mutate(time_diff = Date.Time - lag(Date.Time)) %>% 
  # mutate(time_diff2 = c(0, diff(Date.Time))) %>% 
  # mutate(time_diff = Date.Time - lag(Date.Time, default = first(Date.Time))) %>% 
  # mutate(time_diff = Date.Time - lag(Date.Time)) %>% 
  # # group_by(Date.Time) %>% summarise_each(funs(max(., na.rm=T))) 
  # filter(time_diff!=0 | is.na(time_diff))                       ###Remove rows where timestamps is exactly the same that previous row (happens every hour). Removing the bottom line since sometimes it includes NAs on values
  
  # dataFrame[1,6] <- 0
  #   return(dataFrame)
  # }
  
  if(max(dataFrame$time_diff, na.rm = TRUE) > longest_gap_tolerated) {
    
    dataFrame[1 ,2:ncol(dataFrame)] <- print("t > 25 minutes")   #### CHANGED 04/09/2020!!!!
    dataFrame <- dataFrame[1,]
    
    return(dataFrame)
    
  } else {
    
    outTime <- which(dataFrame$time_diff < 1.02399900000000 | dataFrame$time_diff > 1.02500010000000)
    
    dataFrame[rownames(dataFrame) %in% outTime, 2:(ncol(dataFrame)-1)] <- NA
    dataFrame[Missing] <- "Missing Column"  #Refill NA values introduced
    
    return(dataFrame)
  }
}

  



##################################################################################################
#Function to create Table displaying Fragmentation Stats by column
##################################################################################################



tableDisplayFragmentation <- function(dataFrame, column_name){
  
  elemListName <- names(dataFrame)
  is.na.rle    <- lapply(dataFrame, function(x) rle(is.na(x[, column_name])|x[, column_name]=="Missing Column"))
  # lapply(dataFrame, function(x) mutate(x, column = ifelse(column %in% names(x), column, NA)))
  
  diff_date <- lapply(dataFrame, function(x){    #Function to extract each range of NA/not-NA and their duration
    x <- x %>% 
      # mutate(time_diff2 = c(0, diff(Date.Time)))%>%
      group_by(Var1_interval_grp = cumsum(c(1, abs(diff(is.na(x[, column_name]))))))%>%
      mutate(time_diff = sum(time_diff, na.rm = T))
    
    # x <- x %>% 
    #   mutate(Var1_interval_grp = cumsum(c(1, abs(diff(is.na(x[, column_name])))))) %>%    ##Name groups of same logic value
    #   group_by(Var1_interval_grp) %>% 
    #   # mutate(Range_Var1 = as.duration(diff(range(Date.Time))) + 1.024) %>% ###### ADD 1.024 SECONDS PER INTERVAL
    #   mutate(Range_Var1 = sum(time_diff)) %>% 
    #   ungroup() %>%
    #   select(! contains("grp"))
    #   x$Range_Var1[1] <- ifelse(x$Range_Var1==0, 1.024, x$Range_Var1[1]) #if only first row is NA, change to 1.024s
    # 
    # x$Range_Var1 <-  ifelse(x$Range_Var1==0, as.duration(x$Date.Time - lag(x$Date.Time)), x$Range_Var1)   ###Added to calculate diff time when all row is NA
    return(x) 
  })
  
  
  
  
  
  File_name    <- elemListName
  Patient_ID   <- gsub(".*_([FT]G)_([a-zA-Z0-9]+).*", "\\1_\\2", elemListName) 
  Measurement_type    <- unlist(lapply(File_name, function(x) if (str_detect(x, "24h")) { "24h" } else { "90Min" }))
  Date_and_Starting_Time  <- as_datetime(sapply(dataFrame, function(x) x[1,1]), tz= "Europe/Berlin")
  End_Time      <- as_datetime(sapply(dataFrame, function(x) x[nrow(x),1]), tz= "Europe/Berlin")
  Number_of_Gaps   <- unlist(lapply(is.na.rle, function(x) sum(x$values==TRUE)), use.names=FALSE) 
  column_value <- unlist(lapply(dataFrame, function(x) max(x[, column_name], na.rm=T)), use.names=FALSE) 
    
  Duration_of_Whole_Measurement <- round(as.duration(End_Time - Date_and_Starting_Time),2)
  
  ###To avoid -Inf as a result, we are setting duration to 0 if NumberGaps=0. 
  
  # Duration_Shortest_Gap  <- as.duration(ifelse(Number_of_Gaps == 0, 0, round(unlist(lapply(diff_date, function(x) min(x$Range_Var1[is.na(x[, column_name])])), use.names=FALSE),2)))  # Don't take into consideration first row because if value in first column is NA, will display NA since no value in Range_Var1. Filter NA rows to avoid -Inf as result when no NAs on the table
  Duration_Shortest_Gap  <- as.duration(ifelse(Number_of_Gaps == 0, 0,
                                               ifelse (column_value == "Missing Column", Duration_of_Whole_Measurement,
                                                              round(unlist(lapply(diff_date, function(x) min(x$time_diff[is.na(x[, column_name])], na.rm = T)), use.names=FALSE),2))))
  Duration_Longest_Gap   <- as.duration(ifelse(Number_of_Gaps == 0, 0,
                                               ifelse (column_value == "Missing Column", Duration_of_Whole_Measurement,
                                                              round(unlist(lapply(diff_date, function(x) max(x$time_diff[is.na(x[, column_name])], na.rm = T)), use.names=FALSE),2))))
  Duration_Longest_Uninterrupted_Interval <- as.duration(ifelse(Number_of_Gaps == 0, Duration_of_Whole_Measurement,
                                                                ifelse (column_value == "Missing Column", 0,
                                                                round(as.duration(unlist(lapply(diff_date, function(x)  ifelse(max(x$time_diff[!is.na(x[, column_name])], na.rm = T)==-Inf, 0, max(x$time_diff[!is.na(x[, column_name])], na.rm = T))), use.names=FALSE)),2))))
  
  # Comments     <- unlist(lapply(dataFrame, function(x) ifelse (x[1,2] != "Elapsed time between measures greater than 10 minutes" |
  #                                                                is.na(x[1,2]), 
  #                                                             "", "t > 10 min")), use.names=FALSE)
  
  Comments     <- unlist(lapply(dataFrame, function(x) {
    
    ifelse (x[1,2] == "t > 25 minutes", "Elapsed time between measures greater than 25 minutes", 
            # ifelse (x[1, column_name] != "Missing column", "", "Missing column") )}) , use.names=FALSE)
            ifelse (x[1, column_name] != "Missing Column", "", paste0("Parameter ", column_name, " was not recorded during this measurement", collapse=""))
            
    )}) , use.names=FALSE)
  # paste0("Parameter ", colnames(dataFrame[, 2:length(dataFrame)])[dataFrame[2, 2:length(dataFrame)] == 'Missing Column'], " was not recorded during this measurement", collapse="")) )}) , use.names=FALSE)
  # paste0("Parameter ", names(which(x=="Missing Column")), " was not recorded during this measurement", collapse="")) )}) , use.names=FALSE)
  
  
  
  output <- data.frame(File_name, Patient_ID, Measurement_type, Date_and_Starting_Time, Number_of_Gaps, Duration_Shortest_Gap, 
                       Duration_Longest_Gap, Duration_Longest_Uninterrupted_Interval, Duration_of_Whole_Measurement, Comments)
  rownames(output) <- NULL
  # 
  # if (dataFrame[1][2] != "Elapsed time between measures greater than 10 minutes"){
  #   dataFrame$Comments <- "t > 10 min"
  # }
  # 
  return(output)
  
}

#############################################################################################################################
#Function to create Table displaying Individual Event Statistics 
#(desaturations, bradycardias, combined desat-bradyc, severe desaturation, severe bradycardias, combined severe desat-brady)
#############################################################################################################################

empty_row_with_message <- function(x){
  x <- x %>% 
    mutate(Date_Time =  NA_integer_) %>% 
    mutate(Event_Number = NA_integer_,
           Event_Start_Time = NA_integer_,
           Event_End_Time = NA_integer_,
           Event_duration = NA_integer_,
           Rel_Error_Pulse = NA_integer_,
           Comments = "NA") %>% 
    select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)

  x[1,] <- NA
  x$df_NAME <- Name
  x$Pat_ID <- Patient_ID
  x$Date_Time <- Date_time
  return(x)
}


###DESATURATION


  individualEventsStats_desaturation <-  function(dataFrame){ 

    Name <- dataFrame[1, "df_NAME"]
    Patient_ID <- dataFrame[1, "Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]

    
    if(nrow(dataFrame)>1){
            dataFrame <- dataFrame %>% 
              mutate(Date_Time = dataFrame[1,1]) %>% 
              mutate(desaturation = cumsum(c(1, abs(diff(SpO2>=80|is.na(SpO2)))))) %>%
              # mutate(desaturation = cumsum(c(1, abs(diff(SpO2<80))))) %>%
              
              filter(SpO2<80) %>% 
              group_by(desaturation) %>% 
              mutate(Event_Start_Time = first(Date.Time)) %>% 
              mutate(Event_End_Time = last(Date.Time)) %>% 
              # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
              mutate(Event_duration = sum(time_diff)) %>% 
              filter(Event_duration>=10 & Event_duration<60) %>% 
              # mutate(Rel_Error_Pulse = abs((HR-Pulse)/HR*100)) %>%                        ##Calculate variation of HR in Pulse
              mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>% 
              mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
              ungroup() 
            
            # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  
              
              ##To avoid having the grouping variables at the final table
            
            if(nrow(dataFrame>0)){
              dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 
                
                       
              dataFrame <- dataFrame %>%   
                mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
                
                select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
              
            } else {
              
              # elemListName <- names(dataFrame)

              dataFrame <- dataFrame %>% 
                mutate(Date_Time =  NA_integer_) %>% 
                mutate(Event_Number = NA_integer_,
                       Event_Start_Time = NA_integer_,
                       Event_End_Time = NA_integer_,
                       Event_duration = NA_integer_,
                       Rel_Error_Pulse = NA_integer_,
                       Comments = "NA") %>% 
                select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
              
              
              dataFrame[1,4:8] <- 0
              dataFrame$Event_duration <- as.duration(0)
              dataFrame$df_NAME <- Name
              dataFrame$Pat_ID <- Patient_ID
              dataFrame$Date_Time <- Date_time
              dataFrame[1,9] <- as.character("No desaturations in this measurement")
            }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time      
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")

    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)
    
    }


  ### SEVERE DESATURATION
  

  
  individualEventsStats_severe_desaturation <-  function(dataFrame){ 
    
    Name <- dataFrame[1,"df_NAME"]
    Patient_ID <- dataFrame[1,"Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]
    
    
    if(nrow(dataFrame)>1){
      dataFrame <- dataFrame %>% 
        mutate(Date_Time = dataFrame[1,1]) %>% 
        mutate(desaturation = cumsum(c(1, abs(diff(SpO2>=80|is.na(SpO2)))))) %>%
        filter(SpO2<80) %>% 
        group_by(desaturation) %>% 
        mutate(Event_Start_Time = first(Date.Time)) %>% 
        mutate(Event_End_Time = last(Date.Time)) %>% 
        # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
        mutate(Event_duration = sum(time_diff)) %>% 
        filter(Event_duration>=60 ) %>% 
        # mutate(Rel_Error_Pulse = abs((HR-Pulse)/HR*100)) %>%                        ##Calculate variation of HR in Pulse
        mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>% 
        mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
        ungroup() 
      
      # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  
      
      ##To avoid having the grouping variables at the final table
      
      if(nrow(dataFrame>0)){
        dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 
        
        
        dataFrame <- dataFrame %>%   
          mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
          
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)

      } else {
        
        # elemListName <- names(dataFrame)
        
        dataFrame <- dataFrame %>% 
          mutate(Date_Time =  NA_integer_) %>% 
          mutate(Event_Number = NA_integer_,
                 Event_Start_Time = NA_integer_,
                 Event_End_Time = NA_integer_,
                 Event_duration = NA_integer_,
                 Rel_Error_Pulse = NA_integer_,
                 Comments = "NA") %>% 
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
        dataFrame[1,4:8] <- 0
        dataFrame$Event_duration <- as.duration(0)
        dataFrame$df_NAME <- Name
        dataFrame$Pat_ID <- Patient_ID
        dataFrame$Date_Time <- Date_time
        dataFrame[1,9] <- as.character("No severe desaturations in this measurement")
      }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")
      
    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)

  }
  


  ###BRADYCARDIA
  
  
  individualEventsStats_bradycardia <-  function(dataFrame){ 
    
    Name <- dataFrame[1,"df_NAME"]
    Patient_ID <- dataFrame[1,"Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]
    
    
    if(nrow(dataFrame)>1){
      dataFrame <- dataFrame %>% 
        mutate(Date_Time = dataFrame[1,1]) %>% 
        mutate(bradycardia = cumsum(c(1, abs(diff(HR>=80|is.na(HR)))))) %>%
        filter(HR<80) %>% 
        group_by(bradycardia) %>% 
        mutate(Event_Start_Time = first(Date.Time)) %>% 
        mutate(Event_End_Time = last(Date.Time)) %>% 
        # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
        mutate(Event_duration = sum(time_diff)) %>% 
        filter(Event_duration>=10 & Event_duration<60) %>% 
        mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>%   ##If Pulse doesn't exist, Rel_Error_Pulse=100
        mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
        ungroup() 
        
      # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  ##Conversion to character to display on Labkey
        
      ##To avoid having the grouping variables at the final table
      
      if(nrow(dataFrame>0)){
        dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 
        
        
        dataFrame <- dataFrame %>%   
          mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
          
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
      } else {
        
        # elemListName <- names(dataFrame)
        
        dataFrame <- dataFrame %>% 
          mutate(Date_Time =  NA_integer_) %>% 
          mutate(Event_Number = NA_integer_,
                 Event_Start_Time = NA_integer_,
                 Event_End_Time = NA_integer_,
                 Event_duration = NA_integer_,
                 Rel_Error_Pulse = NA_integer_,
                 Comments = "NA") %>% 
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
        dataFrame[1,4:8] <- 0
        dataFrame$Event_duration <- as.duration(0)
        dataFrame$df_NAME <- Name
        dataFrame$Pat_ID <- Patient_ID
        dataFrame$Date_Time <- Date_time
        dataFrame[1,9] <- as.character("No bradycardias in this measurement")
      }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time      
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")
      
    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)
    
    
  }
  
  

  ###SEVERE BRADYCARDIA
  
  
  individualEventsStats_severe_bradycardia <-  function(dataFrame){ 
    
    Name <- dataFrame[1,"df_NAME"]
    Patient_ID <- dataFrame[1,"Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]
    
    
    if(nrow(dataFrame)>1){
      dataFrame <- dataFrame %>% 
        mutate(Date_Time = dataFrame[1,1]) %>% 
        mutate(bradycardia = cumsum(c(1, abs(diff(HR>=80|is.na(HR)))))) %>%
        filter(HR<80) %>% 
        group_by(bradycardia) %>% 
        mutate(Event_Start_Time = first(Date.Time)) %>% 
        mutate(Event_End_Time = last(Date.Time)) %>% 
        # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
        mutate(Event_duration = sum(time_diff)) %>% 
        filter(Event_duration>=60) %>% 
        mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>% 
        mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
        ungroup() 

      # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  ##Conversion to character to display on Labkey
      
      if(nrow(dataFrame>0)){
        dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 
        
        
        dataFrame <- dataFrame %>%   
          mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
          
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
      } else {
        

        dataFrame <- dataFrame %>% 
          mutate(Date_Time =  NA_integer_) %>% 
          mutate(Event_Number = NA_integer_,
                 Event_Start_Time = NA_integer_,
                 Event_End_Time = NA_integer_,
                 Event_duration = NA_integer_,
                 Rel_Error_Pulse = NA_integer_,
                 Comments = "NA") %>% 
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
        dataFrame[1,4:8] <- 0
        dataFrame$Event_duration <- as.duration(0)
        dataFrame$df_NAME <- Name
        dataFrame$Pat_ID <- Patient_ID
        dataFrame$Date_Time <- Date_time
        dataFrame[1,9] <- as.character("No severe bradycardias in this measurement")
      }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time      
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")
      
    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)

  }
  


  ###COMBINED DESATURATION - BRADYCARDIA
  
  
  individualEventsStats_desaturation_bradycardia <-  function(dataFrame){ 
    
    Name <- dataFrame[1,"df_NAME"]
    Patient_ID <- dataFrame[1,"Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]
    # End_time <- dataFrame[nrow(dataFrame), "Date.Time"]
    
    if(nrow(dataFrame)>1){
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time = dataFrame[1,1]) %>% 
        mutate(combined_normal = cumsum(c(1, abs(diff(HR<80 & SpO2<80 & !is.na(HR) & !is.na(SpO2)))))) %>%
        filter(SpO2<80 & HR<80 ) %>% 
        group_by(combined_normal) %>% 
        mutate(Event_Start_Time = first(Date.Time)) %>% 
        mutate(Event_End_Time = last(Date.Time)) %>% 
        # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
        mutate(Event_duration = sum(time_diff)) %>% 
        filter(Event_duration>=10 & Event_duration<60) %>% 
        # mutate(Rel_Error_Pulse = abs((HR-Pulse)/HR*100)) %>%                        ##Calculate variation of HR in Pulse
        mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>% 
        mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
        ungroup() 

      # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  ##Conversion to character to display on Labkey

      ##To avoid having the grouping variables at the final table
      
      if(nrow(dataFrame>0)){
        dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 

        dataFrame <- dataFrame %>%   
          mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
          
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)

      } else {
        
        # elemListName <- names(dataFrame)
        
        dataFrame <- dataFrame %>% 
          mutate(Date_Time =  NA_integer_) %>% 
          mutate(Event_Number = NA_integer_,
                 Event_Start_Time = NA_integer_,
                 Event_End_Time = NA_integer_,
                 Event_duration = NA_integer_,
                 Rel_Error_Pulse = NA_integer_,
                 Comments = "NA") %>% 
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
        dataFrame[1,4:8] <- 0
        dataFrame$Event_duration <- as.duration(0)
        dataFrame$df_NAME <- Name
        dataFrame$Pat_ID <- Patient_ID
        dataFrame$Date_Time <- Date_time
        dataFrame[1,9] <- as.character("No combined desaturation-bradycardia events in this measurement")
      }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time      
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")
      
    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)
    
  }
  


  ###COMBINED SEVERE DESATURATION - SEVERE BRADYCARDIA
  
  
  individualEventsStats_severe_desaturation_bradycardia <-  function(dataFrame){ 
    
    Name <- dataFrame[1,"df_NAME"]
    Patient_ID <- dataFrame[1,"Pat_ID"]
    Date_time <- dataFrame[1, "Date.Time"]
    
    
    if(nrow(dataFrame)>1){
      dataFrame <- dataFrame %>% 
        mutate(Date_Time = dataFrame[1,1]) %>% 
        mutate(combined_normal = cumsum(c(1, abs(diff(HR<80 & SpO2<80 & !is.na(HR) & !is.na(SpO2)))))) %>%
        filter(SpO2<80 & HR<80 ) %>% 
        group_by(combined_normal) %>% 
        mutate(Event_Start_Time = first(Date.Time)) %>% 
        mutate(Event_End_Time = last(Date.Time)) %>% 
        # mutate(Event_duration = as.duration(last(Date.Time) - first(Date.Time))) %>% 
        mutate(Event_duration = sum(time_diff)) %>% 
        filter(Event_duration>=60) %>% 
        # mutate(Rel_Error_Pulse = abs((HR-Pulse)/HR*100)) %>%                        ##Calculate variation of HR in Pulse
        mutate(Rel_Error_Pulse = ifelse(is.numeric(Pulse), mean(abs((HR-Pulse)/HR*100)), 100)) %>% 
        mutate(Comments = NA) %>%    ##Mark rows with absolute value of pulse-HR greater than 5% 
        ungroup() 
        
      # dataFrame$Event_duration <- as.character(dataFrame$Event_duration)  ##Conversion to character to display on Labkey
        
      ##To avoid having the grouping variables at the final table
      
      if(nrow(dataFrame>0)){
        dataFrame <- dataFrame[!duplicated(dataFrame$Event_Start_Time),] 
        
        
        dataFrame <- dataFrame %>%   
          mutate(Event_Number = 1:nrow(dataFrame) ) %>% 
          
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
      } else {
        
        # elemListName <- names(dataFrame)
        
        dataFrame <- dataFrame %>% 
          mutate(Date_Time =  NA_integer_) %>% 
          mutate(Event_Number = NA_integer_,
                 Event_Start_Time = NA_integer_,
                 Event_End_Time = NA_integer_,
                 Event_duration = NA_integer_,
                 Rel_Error_Pulse = NA_integer_,
                 Comments = "NA") %>% 
          select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
        
        
        dataFrame[1,4:8] <- 0
        dataFrame$Event_duration <- as.duration(0)
        dataFrame$df_NAME <- Name
        dataFrame$Pat_ID <- Patient_ID
        dataFrame$Date_Time <- Date_time
        dataFrame[1,9] <- as.character("No combined severe desaturation-bradycardia events in this measurement")
      }
      
    } else {
      
      dataFrame <- dataFrame %>% 
        mutate(Date_Time =  NA_integer_) %>% 
        mutate(Event_Number = NA_integer_,
               Event_Start_Time = NA_integer_,
               Event_End_Time = NA_integer_,
               Event_duration = NA_integer_,
               Rel_Error_Pulse = NA_integer_,
               Comments = "NA") %>% 
        select(df_NAME, Date_Time, Pat_ID, Event_Number, Event_Start_Time, Event_End_Time, Event_duration, Rel_Error_Pulse, Comments)
      
      
      dataFrame[1,4:8] <- 0
      dataFrame$df_NAME <- Name
      dataFrame$Pat_ID <- Patient_ID
      dataFrame$Date_Time <- Date_time      
      dataFrame[1,9] <- as.character("Elapsed time between measures greater than 25 minutes")
      
    }
    
    colnames(dataFrame) <- c('File Name', 'Date And Time', 'Patient ID',	'Event Number',	'Event Start Time',	'Event End Time', 'Event Duration',	
                             'Average Relative Error Pulse Vs HR',	'Comments')
    
    return(dataFrame)
    
    
  }
  
  
  
  
  ##################################################################################################
  #Function to select longest interval without interruptions on column
  ##################################################################################################
  
  
  interval_no_interruptions <- function(DataFrameOfMeasurements, selectVariables="HR") { #Consider range with multiple variable selected on Shiny app, not only the variable of interest
    
    DataFrameOfMeasurements <- DataFrameOfMeasurements %>% 
      mutate(Var1_interval_grp = cumsum(c(1, abs(diff(DataFrameOfMeasurements$group <- complete.cases(DataFrameOfMeasurements[,selectVariables])))))) %>%
      filter(complete.cases(DataFrameOfMeasurements[,selectVariables])) %>% 
      group_by(Var1_interval_grp) %>% 
      mutate(N = n()) %>%
      ungroup() %>% 
      filter(N == max(N)) %>% 
      select(-N, - Var1_interval_grp)
    DataFrameOfMeasurements <- as.data.frame(DataFrameOfMeasurements)  #To remove class tbl and tbl_df
    
    return(DataFrameOfMeasurements)
  }
  

  
  ##################################################################################################
  #Function to create Table displaying Sample Entropy for 90 Min measurements
  ##################################################################################################
  
  
  calculateEntropies <- function(DataFrameOfMeasurements, selectVariables="HR", VariableName = "HR", EmbeddingDim=3, NeighborhoodRadius = 0.2,...){
    #This function assumes the data frame has been already formatted and cleaned of missing values.
    #In particular, the column of the variable of interest (specified through the input parameter "VariableName") should not have any interruptions such as missing values (NAs)

    Total_Duration_of_Measurements <- last(DataFrameOfMeasurements$Date.Time) - first(DataFrameOfMeasurements$Date.Time)

    DataFrameOfMeasurements <- interval_no_interruptions(DataFrameOfMeasurements, selectVariables)

    if (is.na(DataFrameOfMeasurements[,VariableName]) | DataFrameOfMeasurements[,VariableName] == -Inf){

      stop("Data supplied are not valid!")
    }

    OutputData <- data.frame(File_Name = NA,
                             Patient_ID = NA,
                             Date_And_time = NA,
                             Entropy_of_top_part_of_time_series = NA,
                             Entropy_of_middle_part_of_time_series = NA,
                             Entropy_of_bottom_part_of_time_series = NA,
                             Entropy_of_whole_time_series = NA,
                             Effective_Initial_Time_Point = NA,
                             Effective_Final_Time_Point = NA,
                             Effective_Duration = NA,
                             Total_Duration_of_Measurements = NA)

    if (sum(names(DataFrameOfMeasurements) == VariableName) != 1){

      print(paste("Variable", VariableName, "not found in data frame", sep = " "))

    }else{

      File_Name <- DataFrameOfMeasurements[1,"df_NAME"]
      Patient_ID <- DataFrameOfMeasurements[1,"Pat_ID"]
      Date_And_time <- DataFrameOfMeasurements[1, "Date.Time"]

      #Calculate the Variable Specific Time Series

      n <- nrow(DataFrameOfMeasurements)
      # partition_index <- PartinioningTimeSeries(n)
      TimeSeries <- c(DataFrameOfMeasurements[, VariableName])

      if (any(!is.na(TimeSeries)) & ((n/2)+1) > EmbeddingDim){

      # if (sum(is.na(TimeSeries)) == 0 & ((n/2)+1) > EmbeddingDim){

        DataFrameOfMeasurements_top <-    subset(DataFrameOfMeasurements, Date.Time <= quantile(Date.Time, 0.5))
        DataFrameOfMeasurements_middle <- subset(DataFrameOfMeasurements, (Date.Time >= quantile(Date.Time, 0.25) & (Date.Time <= quantile(Date.Time, 0.75))))
        DataFrameOfMeasurements_bottom <- subset(DataFrameOfMeasurements, Date.Time >= quantile(Date.Time, 0.5))

        OutputData$File_Name = File_Name
        OutputData$Patient_ID = Patient_ID
        OutputData$Date_And_time = Date_And_time
        # OutputData$Entropy_of_top_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[1] : partition_index[2])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
        # OutputData$Entropy_of_middle_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[3] : partition_index[4])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
        # OutputData$Entropy_of_bottom_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[5] : partition_index[6])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
        # OutputData$Entropy_of_whole_time_series = SampEn_R(TimeSeries, dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
        OutputData$Entropy_of_top_part_of_time_series = SampEn_R(DataFrameOfMeasurements_top[, VariableName], dim=3, r=0.2*sd(TimeSeries))
        OutputData$Entropy_of_middle_part_of_time_series = SampEn_R(DataFrameOfMeasurements_middle[, VariableName], dim=3, r=0.2*sd(TimeSeries))
        OutputData$Entropy_of_bottom_part_of_time_series = SampEn_R(DataFrameOfMeasurements_bottom[, VariableName], dim=3, r=0.2*sd(TimeSeries))
        OutputData$Entropy_of_whole_time_series = SampEn_R(TimeSeries, dim=3, r=0.2*sd(TimeSeries))


        OutputData$Effective_Initial_Time_Point  = first(DataFrameOfMeasurements$Date.Time)
        OutputData$Effective_Final_Time_Point  =last(DataFrameOfMeasurements$Date.Time)
        # OutputData[,4:7] <- OutputData[,4:7]   ### ????????????????
        OutputData$Effective_Duration = as.duration(OutputData$Effective_Final_Time_Point - OutputData$Effective_Initial_Time_Point)
        OutputData$Total_Duration_of_Measurements = as.duration(Total_Duration_of_Measurements)


        }
    }

    return(OutputData)
  }
  
  # calculateEntropies <- function(DataFrameOfMeasurements, VariableName = "HR", EmbeddingDim=3, NeighborhoodRadius = 0.2){
  #   #This function assumes the data frame has been already formatted and cleaned of missing values.
  #   #In particular, the column of the variable of interest (specified through the input parameter "VariableName") should not have any interruptions such as missing values (NAs)
  #   
  #   Total_Duration <- last(DataFrameOfMeasurements$Date.Time) - first(DataFrameOfMeasurements$Date.Time)
  #   
  #   DataFrameOfMeasurements <- interval_no_interruptions(DataFrameOfMeasurements, VariableName = "HR")
  #   
  #   if (is.na(DataFrameOfMeasurements[,VariableName]) | DataFrameOfMeasurements[,VariableName] == -Inf){
  #     
  #     stop("Data supplied are not valid!")
  #   }
  #   
  #   OutputData <- data.frame(File_Name = NA,
  #                            Patient_ID = NA,
  #                            Date_And_time = NA,
  #                            Entropy_of_top_part_of_time_series = NA,
  #                            Entropy_of_middle_part_of_time_series = NA,
  #                            Entropy_of_bottom_part_of_time_series = NA,
  #                            Entropy_of_whole_time_series = NA,
  #                            Effective_Initial_Time_Point = NA,
  #                            Effective_Final_Time_Point = NA,
  #                            Effective_Duration = NA,
  #                            Total_Duration = NA)
  #   
  #   if (sum(names(DataFrameOfMeasurements) == VariableName) != 1){
  #     
  #     print(paste("Variable", VariableName, "not found in data frame", sep = " "))
  #     
  #   }else{
  #     
  #     File_Name <- DataFrameOfMeasurements[1,"df_NAME"]
  #     Patient_ID <- DataFrameOfMeasurements[1,"Pat_ID"]
  #     Date_And_time <- DataFrameOfMeasurements[1, "Date.Time"]
  #     
  #     #Calculate the Variable Specific Time Series
  #     
  #     n <- nrow(DataFrameOfMeasurements)
  #     partition_index <- PartinioningTimeSeries(n)
  #     TimeSeries <- c(DataFrameOfMeasurements[, VariableName])  
  #     
  #     if (sum(is.na(TimeSeries)) == 0 & ((n/2)+1) > EmbeddingDim){
  #       
  #       
  #       OutputData$File_Name = File_Name
  #       OutputData$Patient_ID = Patient_ID
  #       OutputData$Date_And_time = Date_And_time
  #       OutputData$Entropy_of_top_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[1] : partition_index[2])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
  #       OutputData$Entropy_of_middle_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[3] : partition_index[4])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
  #       OutputData$Entropy_of_bottom_part_of_time_series = SampEn_R(TimeSeries[c(partition_index[5] : partition_index[6])], dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries))
  #       OutputData$Entropy_of_whole_time_series = SampEn_R(TimeSeries, dim=EmbeddingDim, r = NeighborhoodRadius * sd(TimeSeries)) 
  #       OutputData$Effective_Initial_Time_Point  = first(DataFrameOfMeasurements$Date.Time)
  #       OutputData$Effective_Final_Time_Point  =last(DataFrameOfMeasurements$Date.Time)
  #       OutputData[,4:7] <- OutputData[,4:7]
  #       OutputData$Effective_Duration = as.duration(OutputData$Effective_Final_Time_Point - OutputData$Effective_Initial_Time_Point)
  #       OutputData$Total_Duration = as.duration(Total_Duration)
  #       
  #       
  #     }
  #   }
  #   
  #   return(OutputData)
  # }
  
  
  
  ##################################################################################################
  #Functions to upload a single or a list of dataframes
  ##################################################################################################
  
  
  #Upload function for a single dataframe
  
  upload_single <- function(outcome){
    # outcome <- outcome %>% mutate_all(as.character)  Doesn't work. Error
    session<-getSession(baseUrl = baseUrl, folderPath = folderPath);
    labkey.domain.createAndLoad(baseUrl=baseUrl, folderPath=folderPath, name=deparse(substitute(outcome)), df=outcome, domainKind="IntList", options=list(keyName = "Key"))
    
  }
  
  #Upload function for a single dataframe to different folders
  
  upload_single_folders <- function(outcome){
    folder <- gsub("Summary_", "", deparse(substitute(outcome)))
    folderPath <- paste0(folderPath, folder)
    session<-getSession(baseUrl = baseUrl, folderPath = folderPath);
    labkey.domain.createAndLoad(baseUrl=baseUrl, folderPath=folderPath, name=deparse(substitute(outcome)), df=outcome, domainKind="IntList", options=list(keyName = "Key"))
    
  }
  
  
  #Upload function for a list dataframes to different folders (same name that outcome passed)
  
  upload_list <- function(outcome){
    folder <- deparse(substitute(outcome))
    folderPath <- paste0(folderPath, folder)
    session<-getSession(baseUrl = baseUrl,folderPath = folderPath);
    lapply(seq_along(outcome), function(i) labkey.domain.createAndLoad(baseUrl=baseUrl, folderPath=folderPath,name=names(outcome[i]), df=outcome[[i]], domainKind="IntList", options=list(keyName = "Key")))
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  