library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)
library(lubridate)

## !!README!!README!!README!!
## Script used to download & filter data and save into longformat. 
### Step 1: Get scoreform data to get start/ end time of test.
### Step 2: Create empty longformat.
### Step 3: For-loop to download HR-data for each session. Also filter data based on start/ end times. If delay in start, make adjustments.
### Step 4: Save into file.

# Step 1: get scoreform with start/ end times ----
scoreform_HA <- read_excel(here("data/Scoreform_excel_HA.xlsx"), na = c("NA", "", "_", "-")) %>%
  dplyr::select(pp, nmbr_test, time_start_15, time_start_TDextra, tm_start, tm_end) %>%
  mutate(time_start_45 = time_start_15,
         time_end_45 = time_start_15 + minutes(45),                                                      # End time of 45 min block
         time_start_ch = case_when(
           is.na(tm_start) | tm_start < time_start_TDextra ~ time_end_45 + minutes(5),
           TRUE ~ time_start_TDextra),                                                                  # Start CH depends on start tm phase and TD phase
         time_start_ch = coalesce(time_start_ch, time_end_45 + minutes(5)),                             # If start_ch ends up being NA, replace by time_end_45+5 minutes
         time_end_ch = tm_end)


# Step 2: create empty longformat ----
longformat_HR <- data.frame()



# Step 3: load, subset and add each file to longformat_HR ---- 
pp_value <- c(1:25)
testname <- c("ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha7", "ha8", "ha9")

for (participant in pp_value) {
  for (test in 1:NROW(testname)){
    
    ## Empty dataframes to be sure
    averaged_HR <- NULL
    temp <- NULL
    filtered <- NULL
    timediff <- NULL
    starttime <- NULL
    times <- NULL
    tocombinetoHR <- NULL

    
    ## Create dynamic file names
    filename <- paste("/p", participant, "_", testname[test], "_hr.csv", sep = "")
    
    ## CHEST: Check if file exists & do all subsetting
    if(file.exists(paste(here("data/data_raw_foranalysis/"), "/p", participant, "/",  filename, sep = ""))) {
      
      ### Download data + get starttime watch + extract right columns
      temp <- read.csv(paste(here("data/data_raw_foranalysis/"), "/p", participant, "/", 
                             filename, sep = ""))
      
      #### Skip if HR sensor was not working and gave FALSE values.
      if (any(sapply(temp, function(col) any(col == "false", na.rm = TRUE)))){
        print(paste("p", participant, "ha", test,"Skipping operation because 'false' was found in the data."))
        averaged_HR <- data.frame(Minutes = c(1:166), HR = rep(NA, 166))
        
      } else { 
        starttime <- as_hms(temp$Start.time[1])
        temp <- temp %>%
          slice(-1, -2) %>%
          rename(time = names(.)[2],
                 HR = names(.)[3]) %>%
          mutate(time = as_hms(time), 
                 HR = as.numeric(HR)) %>%
          dplyr::select(time, HR)
        
        
        
        ### Filter only needed rows by start/ end times
        ## IS THIS STEP NECESSARY???
        times <- scoreform_HA[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test,] %>%
          mutate(time_start_45 = as_hms(substr(time_start_45, 12, 19)),
                 time_end_ch = as_hms(substr(time_end_ch, 12, 19))) %>%
          dplyr::select(pp, nmbr_test, time_start_45, time_end_ch)
        
        # 
        # filtered <- temp %>%
        #   filter(time >= times$time_start_45 & time <= times$time_end_ch)
        
        timediff <- as.numeric(times$time_start_45 - starttime)
        
        ### IF difference between start time Polar and starttime we wrote down is larger than 2 min. provide error. 
        if (timediff > 120 | timediff < -120) {
          # If time difference is too large print a statement and create a NA value column
          print(paste("pp", participant, "sessie", test, "big time diff of", timediff, "corrected for by making start minutes NA"))
          # calculate amount of empty rows to account for delay.
          emptyrows <- abs(round(timediff/60))
          averaged_HR <- data.frame(Minutes = c(1:emptyrows), HR = rep(NA, emptyrows))
          
          # create temp variable
          tocombinetoHR <- temp %>%
            mutate(Minutes = round(as.numeric(time)/60)) %>%
            group_by(Minutes) %>%
            summarise(HR = round(mean(HR, na.rm = TRUE))) %>%
            ungroup() %>%
            dplyr::select(Minutes, HR)
          
          # use temp file and empty file to create averaged_HR. Additionally, change Minutes column. 
          averaged_HR <- rbind(averaged_HR, tocombinetoHR) %>%
            mutate(Minutes = row_number() - 1)
          
        }else{
          
          ### average over 1 minute
          averaged_HR <- temp %>%
            mutate(Minutes = round(as.numeric(time)/60)) %>%
            group_by(Minutes) %>%
            summarise(HR = round(mean(HR, na.rm = TRUE))) %>%
            ungroup() %>%
            dplyr::select(Minutes, HR)
        }       
       
      }
    } else {
      
      ### If file does not exist, create an empty dataframe
      averaged_HR <- data.frame(Minutes = c(1:166), HR = rep(NA, 166))
    }
    
    ### Add pp and HA number
    averaged_HR <-  averaged_HR %>%
      mutate(pp = participant, 
             ha = test) %>%
      dplyr::select(pp, ha, Minutes, HR)
    
    
    ### Add into longformat
    longformat_HR <- rbind(longformat_HR, averaged_HR)
    
    
  }
}
 

## 
      
      
      

    
    


# TEMP STEP: change deviations from protocol ----
## Did different procol (only sitting)
longformat_HR <- longformat_HR %>%
  mutate(HR = ifelse(pp == 3 & ha == 4, is.na(HR), HR))        

## Shift HR data to 11 minutes earlier.
longformat_HR <- longformat_HR %>%
  group_by(pp, ha) %>%
  mutate(HR = ifelse(pp == 12 & ha == 1, lead(HR, 11), HR)) %>%
  ungroup()     



# Step 4: save as excel----
write.xlsx(longformat_HR,
           file = file.path(paste0(here("data_output"), 
                                   "/HA_longformat_HR.xlsx")))
