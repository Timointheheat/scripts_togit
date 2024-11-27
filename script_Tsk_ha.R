library(tidyverse)                                         
library(here)
library(dplyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(hms)

# What this script does: 
## step 1: get cleaning suggestions
## step 2: get start/ end times of test
## step 3: create empty lonformat
## step 4: cleaning loops according to predefined rules.
## step 5: exclude nonsense data (1 file)
  ## step 6: save files
  
  
  
  # Step 1: get cleaning suggestions Sheila file + get correct timestamps ----- 
cleaningRules <- read_excel(
  paste(here(), "/iButton_cleaning_marked_sections.xlsx", sep = ""),
  col_types = c("numeric", "numeric", "text", "date", "text", "text", "text", "text", "text", "text")
) %>%
  mutate(
    `Start Time` = if_else(
      str_detect(`Start Time`, ":"),
      as.POSIXct(paste(Date, `Start Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      as.POSIXct(Date, tz = "UTC") + as.difftime(as.numeric(`Start Time`), units = "days")
    ),
    `End Time` = if_else(
      str_detect(`End Time`, ":"),
      as.POSIXct(paste(Date, `End Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      as.POSIXct(Date, tz = "UTC") + as.difftime(as.numeric(`End Time`), units = "days")
    )
  )

# Step 2: get scoreform with start/ end times ----
scoreform_HA <- read_excel(here("data_raw/Scoreform_excel_HA.xlsx"), na = c("NA", "", "_", "-")) %>%
  dplyr::select(pp, nmbr_test, time_start_15, time_start_TDextra, tm_start, tm_end) %>%
  mutate(time_start_45 = time_start_15,
         time_end_45 = time_start_15 + minutes(45),                                                      # End time of 45 min block
         time_start_ch = case_when(
           is.na(tm_start) | tm_start < time_start_TDextra ~ time_end_45 + minutes(5),
           TRUE ~ time_start_TDextra),                                                                  # Start CH depends on start tm phase and TD phase
         time_start_ch = coalesce(time_start_ch, time_end_45 + minutes(5)),                             # If start_ch ends up being NA, replace by time_end_45+5 minutes
         time_end_ch = tm_end)

scoreform_HA <- scoreform_HA %>%
  mutate(
    time_start_45 = as_hms(format(time_start_45, "%H:%M:%S")),
    time_end_45 = as_hms(format(time_end_45, "%H:%M:%S")),
    time_start_ch = as_hms(format(time_start_ch, "%H:%M:%S")),
    time_end_ch = as_hms(format(time_end_ch, "%H:%M:%S"))
    
  )

  
# Step 3: create empty longformat ----
longformat_Tsk <- data.frame()



# Step 4: Major cleaning loop ---- 
pp_value <- c(1:25)
testname <- c("ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha7", "ha8", "ha9")

for (participant in pp_value) {
  for (test in 1:NROW(testname)){

    # Reset data frames
    data_frames45 <- list()
    data_framesCH <- list()
    data_frames <- list()
    
    for (ib in 1:4){
      
      
      ## Set temp variable to null
      temp <- NULL
      temp45<- NULL
      tempCH<- NULL
      datetime <- NULL
      
      ## Create dynamic file names
      filename <- paste("/p", participant, "/p", participant, "_", testname[test], "_ib", ib, ".csv", sep = "")
      
      ## Check if file name exists. If no, create NA variable.
      ## If yes download file, subset data using timestamps, do cleaning necessary  
      if(file.exists(paste(here("data_raw_foranalysis/"), filename, sep = ""))) {
        temp <- read.csv(paste(here("data_raw_foranalysis/"), filename, sep = ""), skip = 19) 
        
        temp <- temp %>%
          mutate(
            Date.Time = as.POSIXct(Date.Time, format = "%d/%m/%y %H:%M:%S", tz = "UTC"),
            time = format(Date.Time, "%H:%M:%S"),
            time = as_hms(time)) 
        
        
        ## Check if cleaning needs to be done.
        ## If not, do nothing.
        ## If yes, Check the cleaning rules to choose appropriate cleaning.
        rule <- cleaningRules %>%
          filter(Participant == participant & Session == test & `Sensor ID` == paste0("ib", ib))

        if (nrow(rule) > 0) {
          action <- rule$`action suggestion`
          error_start <- as_hms(rule$`Start Time`)
          error_end <- as_hms(rule$`End Time`)
          
          if (action[1] == "interpolate") {  # Action INTERPOLATE: Interpolate value using value before and after timepoints of error.
            
            for (i in 1:nrow(rule)) {
              start_time <- as_hms(error_start[i])
              end_time <- as_hms(error_end[i])
              
              # Skip iteration if end_time exceeds max temp$time
              if (end_time >= max(temp$time)) next
              
              start_value <- temp$Value[temp$time == start_time - 60]
              end_value <- temp$Value[temp$time == end_time + 60]
              
              interpolated_values <- approx(x = as.numeric(c(start_time - 60, end_time + 60)),
                                            y = c(start_value, end_value),
                                            xout = as.numeric(temp$time[temp$time >= start_time & temp$time <= end_time]))$y
              
              temp$Value[temp$time >= start_time & temp$time <= end_time] <- interpolated_values
            }
            
          } else if (action[1] == "cutting") {  # Action CUTTING: replace value with NA between timepoints of error
            
            for (i in 1:nrow(rule)) {
              start_time <- as_hms(error_start[i])
              end_time <- as_hms(error_end[i])
              
              # Cut until the end if end_time exceeds max temp$time
              # Else cut replace with NA from start till end time.
              if (end_time > max(temp$time)) {
                temp <- temp %>%
                  mutate(Value = ifelse(time >= start_time & time <= max(temp$time), NA, Value))
              } else {
                temp <- temp %>%
                  mutate(Value = ifelse(time >= start_time & time <= end_time, NA, Value))
              }
            }
            
          } else if (action[1] == "discard") {  # Action DISCARD: Replace entire dataset with NA
            temp <- data.frame(time = rep(NA, 150), Value = rep(NA, 150))
            
          } else if (action[1] == "leave it") {
            # Do nothing
          }
        }
        
      } else {
        print(paste("Participant", participant, "test", testname[test], "does not have a file for IB", ib))
      }
      
      
      ## Subset data based on start and end time
      time_start_45 <- as_hms(scoreform_HA$time_start_45[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test])
      time_end_45 <- as_hms(scoreform_HA$time_end_45[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test])
      time_start_ch <- as_hms(scoreform_HA$time_start_ch[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test])
      time_end_ch <- as_hms(scoreform_HA$time_end_ch[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test])
      
      # Check if temp$time contains only NA values
      if (!all(is.na(temp$time))) {
      temp45 <- temp %>%
        filter(time >= time_start_45 & time <= time_end_45) %>%
        mutate(Minutes = c(1:45)) 
      tempCH <- temp %>%
        filter(time >= time_start_ch & time <= time_end_ch)
      tempCH <- tempCH%>%
        mutate(Minutes = c(46:(NROW(tempCH)+45)))
      
      data_frames[[ib]] <- rbind(temp45, tempCH)
      
      # data_frames45[[ib]] <- temp45
      # data_framesCH[[ib]] <- tempCH
      
      } else{
        data_frames[[ib]] <- data.frame(Minutes = c(1:166), Value = rep(NA, 166), Date.Time = rep(NA, 166), time = rep(NA, 166))
        
        # data_frames45[[ib]] <- data.frame(Minutes = c(1:45), Value = rep(NA, 45))
        # data_framesCH[[ib]] <- data.frame(Minutes = c(46:166), Value = rep(NA, 121)) 
      }
      
    }

    ## Merge IB data into Tsk dataframe. 
    ## Additionally calculate average Tsk. 
    datetime <- data.frame(data_frames[1])%>%
      dplyr::select(Date.Time, time, Minutes)
    
    Tsk <- data_frames[[1]] %>%
      rename(ib_1 = Value) %>%
      left_join(data_frames[[2]] %>% rename(ib_2 = Value), by = "Minutes") %>%
      left_join(data_frames[[3]] %>% rename(ib_3 = Value), by = "Minutes") %>%
      left_join(data_frames[[4]] %>% rename(ib_4 = Value), by = "Minutes") %>%
      mutate(Tsk_average = 0.28 * ib_1 + 0.28 * ib_2 + 0.16 * ib_3 + 0.28 * ib_4,
             pp = participant,
             hst = test) %>%
      dplyr::select(pp, hst, Minutes, ib_1, ib_2, ib_3, ib_4, Tsk_average)
    
    
    Tsk <- Tsk %>%
      left_join(datetime, by = "Minutes")
    

    longformat_Tsk <- rbind(longformat_Tsk, Tsk)
  }
}


# STEP 5: Calculate average If 1 or 2 IB data are missing, then calculate unweighted mean of remaining IB
longformat_Tsk <- longformat_Tsk %>%
  rowwise() %>%
  mutate(
    Tsk_average = ifelse(
      is.na(Tsk_average) & sum(!is.na(c(ib_1, ib_2, ib_3, ib_4))) >= 2,
      mean(c(ib_1, ib_2, ib_3, ib_4), na.rm = TRUE),
      Tsk_average
    ),
  )

# longformat_Tsk_45 <- longformat_Tsk_45 %>%
#   rowwise() %>%
#   mutate(
#     Tsk_average = ifelse(
#       is.na(Tsk_average) & sum(!is.na(c(ib_1, ib_2, ib_3, ib_4))) >= 2,
#       mean(c(ib_1, ib_2, ib_3, ib_4), na.rm = TRUE),
#       Tsk_average
#     ),
#   )
# 
# longformat_Tsk_CH <- longformat_Tsk_CH %>%
#   rowwise() %>%
#   mutate(
#     Tsk_average = ifelse(
#       is.na(Tsk_average) & sum(!is.na(c(ib_1, ib_2, ib_3, ib_4))) >= 2,
#       mean(c(ib_1, ib_2, ib_3, ib_4), na.rm = TRUE),
#       Tsk_average
#     ),
#   )

# STEP 5: exclude sessions----
exclude <- data.frame(pp = 3, hst = 4) # sessions excluded because deviated from protocol.
longformat_Tsk <- longformat_Tsk %>%
  mutate(across(-c(pp, hst, Minutes), ~ if_else(pp == exclude$pp & hst == exclude$hst, NA, .)))


# Step 6: Save longformat Tsk----
write.xlsx(longformat_Tsk,
           file = file.path(paste0(here("data_output"), 
                                   "/HA_longformat_Tsk")))

