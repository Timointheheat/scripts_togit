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
## Script used to download & filter data and save into longformat. Data from new_chest_algorithm is used. 
### Step 1: Get scoreform data to get start/ end time of test.
### Step 2: Create empty longformat.
### Step 3: For-loop to download chest and hand CORE-data for each session. Also filter data based on start/ end times
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
longformat_CORE <- data.frame()
longformat_CORE_rawraw <- data.frame()


# Step 3: add each file to longformat_CORE ---- 
pp_value <- c(1:25)
                # 25)
testname <- c("ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha7", "ha8", "ha9")

for (participant in pp_value) {
  for (test in 1:NROW(testname)){
    

    ## Empty dataframes to be sure
    averaged_hand <- NULL
    averaged_chest <- NULL
    averaged_CORE <- NULL
    temp <- NULL
    temp2 <- NULL
    filtered <- NULL
    filtered2 <- NULL
    rawraw_CORE <- NULL
    
    ## Create dynamic file names
    filename_chest <- paste("/p", participant, "_", testname[test], "_core_torso_newAlgo", ".csv", sep = "")
    filename_hand <- paste("/p", participant, "_", testname[test], "_core_hand_newAlgo", ".csv", sep = "")
    
    ## CHEST: Check if file exists & do all subsetting
    if(file.exists(paste(here("data/CORE_rawdata_newAlgorithm/April - New Recalculations/"),
                         filename_chest, sep = ""))) {
      
      ### Download data + extract right columns
      temp <- read.csv(paste(here("data/CORE_rawdata_newAlgorithm/April - New Recalculations/"), 
                             filename_chest, sep = "")) %>%
        rename(Date.time = "Timestamp",
               T_core = "old_cbt",
               #T_core_new = "new_cbt",
               T_core_consumer = "consumer_cbt",
               T_skin = "skin_temp",
               HR = "heart_rate",
               consumer_q = "consumer_quality",
               old_q = "old_quality") %>%
        mutate(Date.time = sub("\\..*", "", Date.time))%>%                          # Delete unexpected ".11" etc
        mutate(date = as.POSIXct(substr(Date.time, 1, 10), format = "%Y-%m-%d"),
               time = as_hms(substr(Date.time, 12, 19))) %>%                        # Convert to useable timestamps
        mutate(T_core_chest = T_core,
               #T_core_chest_new = T_core_new,
               T_core_chest_consumer = T_core_consumer,
               T_skin_chest = T_skin,
               HR = as.numeric(HR)) %>%                                   # Convert to Tc
        dplyr::select(time, T_core_chest, T_core_chest_consumer, T_skin_chest, HR, consumer_q,  old_q)
      
      ### Filter only needed rows by start/ end times
      times <- scoreform_HA[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test,] %>%
        mutate(time_start_45 = as_hms(substr(time_start_45, 12, 19)),
               time_end_ch = as_hms(substr(time_end_ch, 12, 19))) %>%
        dplyr::select(pp, nmbr_test, time_start_45, time_end_ch)
        
      filtered <- temp %>%
        filter(time >= times$time_start_45 & time <= times$time_end_ch)
      
      tocheck_q_chest <- filtered 
      
      ### remove 0-values because they are non-physiological
      filtered <- filtered %>%
        mutate(T_core_chest = ifelse(T_core_chest == 0, NA, T_core_chest),
               T_core_chest_consumer = ifelse(T_core_chest_consumer == 0, NA, T_core_chest_consumer),
               T_skin_chest = ifelse(T_skin_chest == 0, NA, T_skin_chest))
      
      
      ### remove quality score 0-2 because data not good according to GT
      filtered <- filtered %>%
        mutate(#T_core_chest = ifelse(consumer_q < 3, NA, T_core_chest),
               T_core_chest_consumer = ifelse(consumer_q < 3 , NA, T_core_chest_consumer)
               )
      
      
      
      
      ### average over 1 minute
      averaged_chest <- filtered %>%
        mutate(timestamp = as_hms(round(as.numeric(time) / 60) * 60))%>%              # Get times towards closes minutes
        group_by(timestamp) %>%
        summarise(across(c(T_core_chest, T_core_chest_consumer, T_skin_chest, HR), mean, na.rm = TRUE)) %>%            # Calculate mean for each minute
        ungroup() %>%
        mutate(Minutes = as.numeric((timestamp - timestamp[1])/60),
               HR = round(HR)) %>%
        dplyr::select(Minutes, timestamp, T_core_chest, T_core_chest_consumer, T_skin_chest, HR) 
    } else {
      
      ### If file does not exist, create an empty dataframe
      averaged_chest <- data.frame(Minutes = c(1:166), timestamp = rep(NA, 166), T_core_chest = rep(NA, 166), 
                         T_core_chest_consumer = rep(NA, 166), T_skin_chest = rep(NA, 166), HR = rep(NA, 166))
    }
    
    ## HAND: Check if file exists & do all subsetting
    if(file.exists(paste(here("data/CORE_rawdata_newAlgorithm/April - New Recalculations/"),
                         filename_hand, sep = ""))) {
      
      ### Download data + extract right columns
      temp2 <- read.csv(paste(here("data/CORE_rawdata_newAlgorithm/April - New Recalculations/"), 
                             filename_hand, sep = "")) %>%
        rename(Date.time = "Timestamp",
               T_core = "old_cbt",
               #T_core_new = "new_cbt",
               T_core_consumer = "consumer_cbt",
               T_skin = "skin_temp",
               HR = "heart_rate",
               consumer_q = "consumer_quality",
               old_q = "old_quality") %>%
        mutate(Date.time = sub("\\..*", "", Date.time))%>%                          # Delete unexpected ".11" etc
        mutate(date = as.POSIXct(substr(Date.time, 1, 10), format = "%Y-%m-%d"),
               time = as_hms(substr(Date.time, 12, 19))) %>%                        # Convert to useable timestamps
        mutate(T_core_hand = T_core,
               #T_core_hand_new = T_core_new,
               T_core_hand_consumer = T_core_consumer,
               T_skin_hand = T_skin,
               HR = as.numeric(HR)) %>%                                   # Convert to Tc
        dplyr::select(time, T_core_hand, T_core_hand_consumer, T_skin_hand, HR,consumer_q, old_q)
      
      
      ### Filter only needed rows by start/ end times
      times2 <- scoreform_HA[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test,] %>%
        mutate(time_start_45 = as_hms(substr(time_start_45, 12, 19)),
               time_end_ch = as_hms(substr(time_end_ch, 12, 19))) %>%
        dplyr::select(pp, nmbr_test, time_start_45, time_end_ch)
      
      filtered2 <- temp2%>%
        filter(time >= times2$time_start_45 & time <= times2$time_end_ch)
      
      
      tocheck_q_hand <- filtered2
      
      
      ### remove 0-values because they are non-physiological
      filtered2 <- filtered2 %>%
        mutate(T_core_hand = ifelse(T_core_hand == 0, NA, T_core_hand),
               T_core_hand_consumer = ifelse(T_core_hand_consumer == 0, NA, T_core_hand_consumer),
               T_skin_hand = ifelse(T_skin_hand == 0, NA, T_skin_hand))
      
      
      ### remove quality score 0-2 because data not good according to GT
      filtered2 <- filtered2 %>%
        mutate(#T_core_hand = ifelse(consumer_q < 3, NA, T_core_hand),
               T_core_hand_consumer = ifelse(consumer_q < 3 , NA, T_core_hand_consumer),
        )
      
      
      ### average over 1 minute
      averaged_hand <- filtered2 %>%
        mutate(timestamp = as_hms(round(as.numeric(time) / 60) * 60))%>%              # Get times towards closes minutes
        group_by(timestamp) %>%
        summarise(across(c(T_core_hand, T_core_hand_consumer, T_skin_hand), mean, na.rm = TRUE)) %>%            # Calculate mean for each minute
        ungroup() %>%
        mutate(Minutes = as.numeric((timestamp - timestamp[1])/60)) %>%
        dplyr::select(Minutes, timestamp, T_core_hand, T_core_hand_consumer, T_skin_hand) 
    } else {
      
      ### If file does not exist, create an empty dataframe
      averaged_hand <- data.frame(Minutes = c(1:166), timestamp = rep(NA, 166), T_core_hand = rep(NA, 166), 
                                  T_core_hand_consumer = rep(NA, 166), T_skin_hand = rep(NA, 166))
      
    }
    
    ## Combine Hand and Chest
    averaged_CORE <- averaged_chest %>%
      left_join(averaged_hand, by = "Minutes") %>%
      mutate(pp = participant, 
             ha = test,
             HR_core = HR,
             timestamp = timestamp.x) %>%
      dplyr::select(pp, ha, Minutes, timestamp, T_core_chest, T_core_chest_consumer, T_skin_chest,
                    T_core_hand, T_core_hand_consumer, T_skin_hand, HR_core)
    
    # rawraw_CORE <- tocheck_q_chest %>%
    #   left_join(tocheck_q_hand, by = "time") %>%
    #   mutate(pp = participant, 
    #          ha = test,
    #          consumer_q_chest = consumer_q.x,
    #          consumer_q_hand = consumer_q.y)%>%
    # dplyr::select(pp, ha, time, consumer_q_chest, consumer_q_hand)
    
    #longformat_CORE_rawraw <- rbind(longformat_CORE_rawraw, rawraw_CORE)
    
    longformat_CORE <- rbind(longformat_CORE, averaged_CORE)
    
  }
}




# Step 4: save as excel----
write.xlsx(longformat_CORE,
           file = file.path(paste0(here("data/data_output"), 
                                   "/HA_longformat_CORE_17042025.xlsx")))
