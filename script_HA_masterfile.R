library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)

## !!README!!README!!README!!
## Script used to combine longformat files of Trec (partially cleaned), Tsk (cleaned), CORE (raw) and Polar HR (raw) data.
## Additionally, final cleaning of data is done.
### Step 1: Get datalogger data, including Trec, RH, Tdb.
### Step 2: Get ibutton data, incuding Tsk for 4 ib.
### Step 3: Get CORE data, including HR, CORE Tc and Tsk.
### Step 4: Get Polar data, including HR. !!!! NEEDS WORK!!!!
### Step 5: Combine into 1 file.
### Step 6: calculate Tbody
### Step 7: Final cleaning steps.
### Step 8: save masterfile as xlsx


# STEP 1) Get cleaned DL data -----
longformat_DL <- read_excel(paste0(here("data/data_output"), "/HA_longformat_DL.xlsx")) %>%
  dplyr::select(!Time)
  


# STEP 2) Get cleaned IB data ----
longformat_Tsk <- read_excel(paste0(here("data/data_output"), "/HA_longformat_Tsk.xlsx")) %>%
  mutate(Minutes = Minutes -1) %>%                    # Reduce by 1 minute as this starts at t = 1 instead of t = 0
  mutate(ha = hst) %>%
  dplyr::select(!c(hst, Date.Time, time))

# STEP 3) Get RAW CORE data ----
longformat_CORE <- read_excel(paste0(here("data/data_output"), "/HA_longformat_CORE_31032025.xlsx")) 





# STEP 4) Get RAW Polar data ----

longformat_Polar <- read_excel(paste0(here("data/data_output"), "/HA_longformat_HR.xlsx")) %>%
  rename(HR_polar = HR)


# STEP 5) Merge all into 1 Masterfile----
Masterfile <- longformat_CORE %>%
  full_join(longformat_Tsk, by = c("pp", "ha", "Minutes")) %>%
  full_join(longformat_DL, by = c("pp", "ha", "Minutes")) %>%
  full_join(longformat_Polar, by = c("pp", "ha", "Minutes")) %>%
  rename(Trec = mean_Trec, 
         RH = mean_RH_vaisalah, 
         Tdb = mean_T_vaisalah,
         T_core_torso = T_core_chest,
         T_core_torso_new = T_core_chest_new,
         T_skin_torso = T_skin_chest)


# STEP 6) Calculate Tbody----
Masterfile <- Masterfile %>%
  mutate(Tbody = 0.8 * Trec + 0.2 * Tsk_average,
         Tbody_core_torso = 0.8 * T_core_torso + 0.2 * T_skin_torso,
         Tbody_core_hand = 0.8 * T_core_hand + 0.2 * T_skin_hand,
         Tbody_core_torso_new = 0.8 * T_core_torso_new + 0.2 * T_skin_torso,
         Tbody_core_hand_new = 0.8 * T_core_hand_new + 0.2 * T_skin_hand)






# STEP 7) Clean data ----
## A) Exclude pp not executing protocol right or Trec data is likely measurement error (N = 3) 
## B) Use cleaning rules derived from visual inspection of all individual data. Saved in excel. 
## C) Remove non-physiological values: 
### HR < 40 during entire test & HR < 100 after cycling for 10 minutes, make NA because unlikely physiologically.
### Make CORE NA if HR is NA. Make CORE na if Tcore < 35 or > 40 degrees.
## D) CURRENTLY NOT APPLIEDExclude no-shows (N=5)

## A) exclude pp not executing protocol right
## p18_ha1_trec data lot of missing and abnormally high values (Trec > 40) exclude this session.
## p4_ha4_trec and other data, she was just sitting in chamber due to injury. Exclude this session. Later drop-out.
exclude <- data.frame(pp = c(3,16,18),
                      ha = c(4,9,1))

Masterfile_cleaned <- Masterfile %>% 
  anti_join(exclude, by = c("pp", "ha"))



## B) Individual  cleaning rules based on visual inspection
### 1) Load cleaning rules
cleaningRules <- read_excel(
  paste(here(), "/HR_CORE_TREC_cleaning_marked_sections.xlsx", sep = ""),)

### 2) cleaning loop
pp_value <- c(1:25)
testname <- c("ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha7", "ha8", "ha9")

for (rownumber in 1:NROW(cleaningRules)) {
  
  # Reset data frames
  temp <- NULL
  participant <- NULL
  session <-NULL
  sensor <- NULL
  colname <- NULL
  rule <- NULL
  start <- NULL
  end <- NULL
  
  # add new values
  participant <- cleaningRules$Participant[rownumber]
  session <- cleaningRules$Session[rownumber]
  sensor <- cleaningRules$'Sensor ID'[rownumber]
  rule <- cleaningRules$'action suggestion'[rownumber]
  
  # change name of sensor into column names
  colname <- ifelse(sensor == "CORE_torso", "T_core_torso_new",
                    ifelse(sensor == "CORE_hand", "T_core_hand_new",
                    ifelse(sensor == "HR", "HR_polar", sensor)))
  
  # exract data that needs cleaning
  #temp <- Masterfile_cleaned %>% filter(pp == participant & ha == session) %>% dplyr::select(pp, ha, Minutes, colname)

  
  if (rule == "leave it" | rule == "add it, done" | rule == "done" | rule == "added" | rule == "excluded already"  | rule == "timeshift, done"){
    # Do nothing 
  
    } else if (rule == "exclude") {
    # If data cannot be trusted, make values NA. (N = 1)
    Masterfile_cleaned <- Masterfile_cleaned %>%
      mutate(!!sym(colname) := if_else(pp == participant & ha == session, NA, .data[[colname]]))
  
    } else if (rule == "cut it") {
    # Make data NA if some small parts contain invalid data.
    start <- cleaningRules$`Start min`[rownumber]
    end <- cleaningRules$`End min`[rownumber]
    Masterfile_cleaned <- Masterfile_cleaned %>%
      mutate(!!sym(colname) := if_else(pp == participant & ha == session & Minutes >= start & Minutes <= end, 
                                       NA, .data[[colname]]))
    
    } else if (rule == "interpolate") {
      # Interpolate as a small part contains questionable data with clear correct data surrounding (N=5)
      start <- cleaningRules$`Start min`[rownumber]-1
      end <- cleaningRules$`End min`[rownumber]+1
      start_value <- Masterfile_cleaned %>% filter(pp == participant & ha == session & Minutes == start) %>% pull(!!sym(colname))
      end_value <- Masterfile_cleaned %>% filter(pp == participant & ha == session & Minutes == end) %>% pull(!!sym(colname))
      Masterfile_cleaned <- Masterfile_cleaned %>%
        mutate(!!sym(colname) := if_else(pp == participant & ha == session & Minutes > start & Minutes < end,
                                         start_value + (end_value - start_value) * (Minutes - start) / (end - start),
                                         .data[[colname]]))
    }}

  
## C) HR removal of unphysiological values
Masterfile_cleaned <- Masterfile_cleaned %>%
  mutate(HR_core = ifelse(HR_core < 40, NA, 
                          ifelse(HR_core < 100 & Minutes >= 16 & Minutes <= 46, NA, HR_core)),
         HR_polar = ifelse(HR_polar < 40, NA, 
                          ifelse(HR_polar < 100 & Minutes >= 16 & Minutes <= 46, NA, HR_polar)),
         T_core_torso = ifelse(is.na(HR_core), NA, T_core_torso),
         T_core_hand = ifelse(is.na(HR_core), NA, T_core_hand),
         T_core_torso = ifelse(T_core_torso < 35, NA, 
                                   ifelse(T_core_torso > 40, NA, T_core_torso)),
         T_core_hand = ifelse(T_core_hand < 35, NA, 
                                  ifelse(T_core_hand > 40, NA, T_core_hand)),
         T_skin_torso = ifelse(is.na(HR_core), NA, T_skin_torso),
         T_skin_hand = ifelse(is.na(HR_core), NA, T_skin_hand),
         T_core_torso_new = ifelse(is.na(HR_core), NA, T_core_torso_new),
         T_core_hand_new = ifelse(is.na(HR_core), NA, T_core_hand_new),
         T_core_torso_new = ifelse(T_core_torso_new < 35, NA, 
                          ifelse(T_core_torso_new > 40, NA, T_core_torso_new)),
         T_core_hand_new = ifelse(T_core_hand_new < 35, NA, 
                                   ifelse(T_core_hand_new > 40, NA, T_core_hand_new)),
         
  ) %>%
  anti_join(exclude, by = c("pp", "ha"))  # Exclude sessions which deviated from protocol


### D) CURRENTLY NOT APPLIED. No-show pp
#noshows <- c(5, 6, 13, 22, 24)
# Masterfile_cleaned <- Masterfile_cleaned %>%
#   filter(!pp %in% noshows) %>%  # Remove pp who did not show up at all






# STEp 8) save masterfile ----
write.xlsx(Masterfile_cleaned,
           file = file.path(paste0(here("data/data_output"), 
                                   "/HA_Masterfile_cleaned_31032025.xlsx")))
write.xlsx(Masterfile,
           file = file.path(paste0(here("data/data_output"), 
                                   "/HA_Masterfile_raw_31032025.xlsx")))











plot_ind_changes <- function(data, variable, unit) {

  # Create the plot
  ind_plot <- data %>%
    ggplot(aes_string(x = "hst", y = variable, group = "pp")) +
    geom_line(alpha = 0.2) +
    geom_point(shape = 1, size = 4, alpha = 0.3) +
    stat_summary(aes(), fun = mean, geom = "point", shape = 16, size = 4, color = "black") +
    stat_summary(aes(), fun = mean, geom = "line", color = "black", linetype = "solid", size = 1.3) +
    stat_summary(aes(), fun.data = mean_se, geom = "ribbon", alpha = 0.1, fill = "black") +
    ppt_theme() +
    labs(title = paste("changes in", variable, "across HSTs"),
         x = "HA-session",
         y = paste(variable, unit)) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 25, hjust = 0.5, vjust = 0.5),
          axis.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
          plot.title = element_text(size = 30, hjust = 0, vjust = 0.5))
  
  print(ind_plot)
}
plot_ind_changes(longformat_Tsk, "Tsk_average", "degrees")
