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
## Script used to download CLEANED DATALOGGER data and save into longformat.
### Step 1: Create empty longformat.
### Step 2: For-loop to download + save data into longformat
### Step 3: Save into xlsx.

# Step 1: create empty longformat ----
longformat_DL <- data.frame()



# Step 2: add each file to longformat_DL ---- 
pp_value <- c(1:25)
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
    
    ## Create dynamic file names
    filename <- paste("/p", participant, "/p", participant, "_", testname[test], "_dl_clean", ".xlsx", sep = "")

    ## CHEST: Check if file exists & do all subsetting
    if(file.exists(paste(here("data_cleaned/"), filename, sep = ""))) {
      
      ### Download data + extract right columns
      temp <- read_xlsx(paste(here("data_cleaned/"), 
                             filename, sep = ""))
      
    } else {
      
      ### If file does not exist, create an empty dataframe
      temp <- data.frame(Minutes = c(1:166), Time = rep(NA, 166), mean_Trec = rep(NA, 166), 
                                   mean_RH_vaisalah = rep(NA, 166), mean_T_vaisalah = rep(NA, 166))
    }
    
    temp2 <- temp %>%
      mutate(pp = participant,
             ha = test) %>%
      dplyr::select(pp, ha, Minutes, Time, mean_Trec, mean_RH_vaisalah, mean_T_vaisalah)
    
    longformat_DL <- rbind(longformat_DL, temp2)
    
  }
}

# Step 3: save as excel----
write.xlsx(longformat_DL,
           file = file.path(paste0(here("data_output"), 
                                   "/HA_longformat_DL.xlsx")))
