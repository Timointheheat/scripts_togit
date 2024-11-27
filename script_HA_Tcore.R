library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)
library(lubridate)


## TEMP did loading + subsetting for 1 file.
## Make this work for all CORE files. By
### Creating similar loop as with Tsk cleaning.
### Create 1 longformat Tcore file

# Step : get scoreform with start/ end times ----
scoreform_HA <- read_excel(here("data_raw/Scoreform_excel_HA.xlsx"), na = c("NA", "", "_", "-")) %>%
  dplyr::select(pp, nmbr_test, time_start_15, time_start_TDextra, tm_start, tm_end) %>%
  mutate(time_start_45 = time_start_15,
         time_end_45 = time_start_15 + minutes(45),                                                      # End time of 45 min block
         time_start_ch = case_when(
           is.na(tm_start) | tm_start < time_start_TDextra ~ time_end_45 + minutes(5),
           TRUE ~ time_start_TDextra),                                                                  # Start CH depends on start tm phase and TD phase
         time_start_ch = coalesce(time_start_ch, time_end_45 + minutes(5)),                             # If start_ch ends up being NA, replace by time_end_45+5 minutes
         time_end_ch = tm_end)


# Step : create empty longformat ----
longformat_CORE <- data.frame()



# Step : add each file to longformat_CORE ---- 
pp_value <- c(1:25)
testname <- c("ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha7", "ha8", "ha9")

for (participant in pp_value) {
  for (test in 1:NROW(testname)){
    
    ## Create dynamic file names
    filename_chest <- paste("/p", participant, "_", testname[test], "_CORE_chest", ".csv", sep = "")
    filename_hand <- paste("/p", participant, "/p", participant, "_", testname[test], "_CORE_hand", ".csv", sep = "")
    
    ## Check if file exists
    if(file.exists(paste(here("data_raw_foranalysis/CORE_rawdata_newAlgorithm/"), filename_chest, sep = ""))) {
      
      ### Download data + extract right columns
      temp <- read.csv(paste(here("data_raw_foranalysis/CORE_rawdata_newAlgorithm/"), 
                             filename_chest, sep = "")) %>%
        rename(Date.time = "time..UTC.OFS..0100.",
               T_core = "cbt..mC.",
               T_core_new = "CBT_NEW_MODEL..mC.") %>%
        mutate(Date.time = sub("\\..*", "", Date.time))%>%                          # Delete unexpected ".11" etc
        mutate(date = as.POSIXct(substr(Date.time, 1, 10), format = "%Y-%m-%d"),
               time = as_hms(substr(Date.time, 12, 19))) %>%                        # Convert to useable timestamps
        mutate(T_core_chest = T_core/1000,
               T_core__chest_new = T_core_new/1000,) %>%                                   # Convert to Tc
        dplyr::select(time, T_core_chest, T_core__chest_new)
      
      ### Filter only needed rows by start/ end times
      times <- scoreform_HA[scoreform_HA$pp == participant & scoreform_HA$nmbr_test == test,]
      filtered <- temp %>%
        filter(time >= times$time_start_45 & time <= times$time_end_ch)
      
    
      
      }
    
      
    
  }
}



test <- read.csv(paste(here("data_cleaned"), "/p1_ha1_CORE_chest.csv", sep = "")) %>%
  rename(Date.time = "time..UTC.OFS..0100.",
         T_core = "cbt..mC.",
         T_core_new = "CBT_NEW_MODEL..mC.") %>%
  mutate(Date.time = sub("\\..*", "", Date.time))%>%                          # Delete unexpected ".11" etc
  mutate(date = as.POSIXct(substr(Date.time, 1, 10), format = "%Y-%m-%d"),
         time = as_hms(substr(Date.time, 12, 19))) %>%                        # Convert to useable timestamps
  mutate(T_core = T_core/1000,
         T_core_new = T_core_new/1000,) %>%                                   # Convert to Tc
  dplyr::select(time, T_core, T_core_new)

times <- scoreform_HA[scoreform_HA$pp == 1 & scoreform_HA$nmbr_test == 1,]

filtered <- test %>%
  filter(time >= times$time_start_45 & time <= times$time_end_ch)

temp <- filtered %>%
  mutate(minute = as_hms(round(as.numeric(time) / 60) * 60)) %>%              # Get times towards closes minutes
  group_by(minute) %>%
  summarise(across(c(T_core, T_core_new), mean, na.rm = TRUE)) %>%            # Calculate mean for each minute
  ungroup() %>%
  dplyr::select(minute, T_core, T_core_new) 
