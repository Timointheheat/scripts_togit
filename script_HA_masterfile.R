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
longformat_DL <- read_excel(paste0(here("data_output"), "/HA_longformat_DL.xlsx")) %>%
  dplyr::select(!Time)
  


# STEP 2) Get cleaned IB data ----
longformat_Tsk <- read_excel(paste0(here("data_output"), "/HA_longformat_Tsk.xlsx")) %>%
  mutate(Minutes = Minutes -1) %>%                    # Reduce by 1 minute as this starts at t = 1 instead of t = 0
  mutate(ha = hst) %>%
  dplyr::select(!c(hst, Date.Time, time))

# STEP 3) Get RAW CORE data ----
longformat_CORE <- read_excel(paste0(here("data_output"), "/HA_longformat_CORE.xlsx")) 





# STEP 4) Get RAW Polar data ----

longformat_Polar <- read_excel(paste0(here("data_output"), "/HA_longformat_HR.xlsx")) %>%
  rename(HR_polar = HR)


# STEP 5) Merge all into 1 Masterfile----
Masterfile <- longformat_CORE %>%
  left_join(longformat_Tsk, by = c("pp","ha", "Minutes")) %>%
  left_join(longformat_DL, by = c("pp","ha", "Minutes")) %>%
  left_join(longformat_Polar, by = c("pp","ha", "Minutes")) %>%
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






# STEP 7) Further clean data ----
## Think which pp to exclude depending on RQ. 
## Exclude when protocol deviated or Trec data is likely measurement error (N = 2)
## Exclude no-shows (N=5)
## HR < 40, make NA because unlikely physiologically.
## HR < 100 after cycling for 10 minutes, make NA because unlikely physiologically.
## CORE with HR is NA, make NA because HR used as input parameter.


### Likely measurement error or deviation from first 45 minutes protocol 
## p18_ha1_trec data lot of missing and abnormally high values (Trec > 40) exclude this session.
## p4_ha4_trec and other data, she was just sitting in chamber due to injury. Exclude this session. Later drop-out.
exclude <- data.frame(pp = c(3,18),
                      ha = c(4,1))

### No-show pp
noshows <- c(5, 6, 13, 22, 24)

Masterfile <- Masterfile %>%
  mutate(HR_core = ifelse(HR_core < 40, NA, 
                          ifelse(HR_core < 100 & Minutes >= 25 & Minutes <= 45, NA, HR_core)),
         T_core_torso = ifelse(is.na(HR_core), NA, T_core_torso),
         T_core_hand = ifelse(is.na(HR_core), NA, T_core_hand),
         T_skin_torso = ifelse(is.na(HR_core), NA, T_skin_torso),
         T_skin_hand = ifelse(is.na(HR_core), NA, T_skin_hand)) %>%
  filter(!pp %in% noshows) %>%  # Remove pp who did not show up at all
  anti_join(exclude, by = c("pp", "ha"))  # Exclude sessions which deviated from protocol





# STEp 8) save masterfile ----
write.xlsx(Masterfile,
           file = file.path(paste0(here("data_output"), 
                                   "/HA_Masterfile_raw.xlsx")))


# STEP 7) Get data into right format for analyses----
## Get average values for Tsk and Trec and Tcore in rest (min 10-15) and constant exercise (min 40-45)
Means <- Masterfile %>%
  group_by(pp, ha) %>%
  summarise(
    Trec_rest_mean = mean(Trec[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tcore_torso_rest_mean = mean(T_core_torso[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tcore_hand_rest_mean = mean(T_core_hand[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Trec_ex_mean = mean(Trec[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tcore_torso_ex_mean = mean(T_core_torso[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tcore_hand_ex_mean = mean(T_core_hand[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    )


Changes_overHA <- Means %>%
  group_by(pp) %>%
  mutate(
    dTrec_rest_mean = Trec_rest_mean - first(Trec_rest_mean),
    dTcore_torso_rest_mean = Tcore_torso_rest_mean - first(Tcore_torso_rest_mean),
    dTcore_hand_rest_mean = Tcore_hand_rest_mean - first(Tcore_hand_rest_mean),
    dTrec_ex_mean = Trec_ex_mean - first(Trec_ex_mean),
    dTcore_torso_ex_mean = Tcore_torso_ex_mean - first(Tcore_torso_ex_mean),
    dTcore_hand_ex_mean = Tcore_hand_ex_mean - first(Tcore_hand_ex_mean)
  ) %>%
  filter(!ha ==1)

Filtered_Changes <- Changes_overHA %>%
  mutate(Difference = Tcore_hand_rest_mean - Tcore_torso_rest_mean)


hist(Changes_overHA)

plot(Changes_overHA$dTcore_hand_ex_mean, Changes_overHA$dTcore_torso_ex_mean)

Changes %>%
  ggplot(aes(x = as.factor(ha), y = dTcore_hand_ex_mean, group = pp)) +
  geom_point()
           
         



  average_Tsk <- Masterfile %>%
  group_by(pp,ha) %>%
  summarise(
    rest_mean = mean(Tsk_average[Minutes >= 9 & Minutes <= 14], na.rm = TRUE),
    rest_sd = sd(Tsk_average[Minutes >= 9 & Minutes <= 14], na.rm = TRUE),
    exercise_mean = mean(Tsk_average[Minutes >= 30 & Minutes <= 45], na.rm = TRUE),
    exercise_sd = sd(Tsk_average[Minutes >= 30 & Minutes <= 45], na.rm = TRUE)
  )

# TO DISPLAY -----
Masterfile_constant <- Masterfile %>%
  filter(Minutes >=0 & Minutes <= 45)

#[Masterfile_constant$pp == 23 & Masterfile_constant$ha == 2,]

Masterfile %>%   
  ggplot(aes(x = Minutes, y = Tsk_average)) + #, color = as.factor(ha))) +
  geom_point(color = "blue", alpha = 0.1) +
  geom_vline(xintercept = 15, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 45, color = "red", linetype = "dashed", size = 1) 

  
# -----  
Masterfile_constant %>%
  filter(pp == 23 & ha == 2) %>%
  dplyr::select(pp, Date.Time, Minutes, ha, Avg_HR_torso, Trec)%>%
  print(n = 50)

# Get info out of masterfile ----
Masterfile[Masterfile$Minutes >= 25 & Masterfile$Minutes <= 30,] %>%
  group_by(ha) %>%
  ggplot(aes(x = Trec, y = T_core_torso)) +
  geom_point() +  
  geom_smooth(method = "loess", se = TRUE, color = "red")

Masterfile %>%
ggplot(aes(x = Trec, y = T_core_torso )) +
  geom_point()+ 
  geom_smooth(method = "loess", se = TRUE, color = "red") 
  

cor(Masterfile$ib_3, Masterfile$T_skin_hand,use = "complete.obs")

average_Tsk <- Masterfile %>%
  group_by(pp,ha) %>%
  summarise(
    rest_mean = mean(Tsk_average[Minutes >= 9 & Minutes <= 14], na.rm = TRUE),
    rest_sd = sd(Tsk_average[Minutes >= 9 & Minutes <= 14], na.rm = TRUE),
    exercise_mean = mean(Tsk_average[Minutes >= 30 & Minutes <= 45], na.rm = TRUE),
    exercise_sd = sd(Tsk_average[Minutes >= 30 & Minutes <= 45], na.rm = TRUE)
  )

ppt_theme <- function() {
  theme_bw() +
    theme(
      text = element_text(size = 40),
      axis.line = element_line(linewidth = 3, color = "#066666"),
      panel.border = element_blank(),
      #title =  element_text(size = 40), # Adjust  title size
      axis.title = element_text(size = 35), # Adjust axis title size
      axis.text = element_text(size = 35),  # Adjust axis text size
      axis.ticks = element_line(size = 2),  # Adjust axis tick thickness
      axis.ticks.length = unit(0.5, "cm"), # Adjust tick length here
      plot.title = element_text(size = 18, margin = margin(b = 20))
    )
}

average_Tsk <- average_Tsk %>%
  mutate(ha = as.factor(hst)) 

Masterfile %>%
  filter(Minutes >= 35 & Minutes <= 40) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Trec, group = 1, color = "Trec"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Trec, group = 1, color = "Trec"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Trec, group = 1, fill = "Trec"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_core_torso, group = 2, color = "T_core_torso"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_core_torso, group = 2, color = "T_core_torso"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_core_torso, group = 2, fill = "T_core_torso"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_core_hand, group = 3, color = "T_core_hand"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_core_hand, group = 3, color = "T_core_hand"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_core_hand, group = 3, fill = "T_core_hand"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "Core temp last 5 min of exercise",
       x = "HA-session",
       y = "Core temp",
       color = "Variable",
       fill = "Variable") +
  scale_color_manual(values = c("Trec" = "orange", "T_core_torso" = "blue", "T_core_hand" = "gray")) +
  scale_fill_manual(values = c("Trec" = "orange", "T_core_torso" = "blue", "T_core_hand" = "gray"))


Masterfile %>%
  filter(Minutes >= 10 & Minutes <= 15) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Trec, group = 1, color = "Trec"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Trec, group = 1, color = "Trec"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Trec, group = 1, fill = "Trec"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_core_torso, group = 2, color = "T_core_torso"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_core_torso, group = 2, color = "T_core_torso"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_core_torso, group = 2, fill = "T_core_torso"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_core_hand, group = 3, color = "T_core_hand"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_core_hand, group = 3, color = "T_core_hand"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_core_hand, group = 3, fill = "T_core_hand"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "Core temp last 5 min of SITTING",
       x = "HA-session",
       y = "Core temp",
       color = "Variable",
       fill = "Variable") +
  scale_color_manual(values = c("Trec" = "orange", "T_core_torso" = "blue", "T_core_hand" = "gray")) +
  scale_fill_manual(values = c("Trec" = "orange", "T_core_torso" = "blue", "T_core_hand" = "gray"))

Masterfile %>%
  filter(Minutes >= 35 & Minutes <= 40) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Tsk_average, group = 1, color = "IB"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Tsk_average, group = 1, color = "IB"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Tsk_average, group = 1, fill = "IB"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_skin_torso, group = 2, color = "T_skin_torso"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_skin_torso, group = 2, color = "T_skin_torso"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_skin_torso, group = 2, fill = "T_skin_torso"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_skin_hand, group = 3, color = "T_skin_hand"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_skin_hand, group = 3, color = "T_skin_hand"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_skin_hand, group = 3, fill = "T_skin_hand"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "SKIN temp last 5 min of exercise",
       x = "HA-session",
       y = "Skin temp",
       color = "Variable",
       fill = "Variable") +
  scale_color_manual(values = c("IB" = "orange", "T_skin_torso" = "blue", "T_skin_hand" = "gray")) +
  scale_fill_manual(values = c("IB" = "orange", "T_skin_torso" = "blue", "T_skin_hand" = "gray"))


Masterfile %>%
  filter(Minutes >= 40 & Minutes <= 45) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "HR last 5 min of exercise",
       x = "HA-session",
       y = "HR")


Masterfile %>%
  filter(Minutes >= 10 & Minutes <= 15) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Avg_HR_torso, group = 1), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "HR last 5 min of sitting",
       x = "HA-session",
       y = "HR")


average_Tsk %>%
  # group_by(hst) %>%
  ggplot(aes(x = ha, y = exercise_mean, group = "pp")) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "orange") +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.1, fill = "orange") +
  stat_summary(fun = mean, geom = "line", color = "orange", linetype = "solid", size = 1) +
  labs(title = "Tsk exercise across HA-period")  
  

Masterfile %>%
  filter(Minutes >= 9 & Minutes <= 13) %>%
  ggplot(aes(x = as.factor(ha))) +
  stat_summary(aes(y = Tsk_average, group = 1, color = "IB"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = Tsk_average, group = 1, color = "IB"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Tsk_average, group = 1, fill = "IB"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_skin_torso, group = 2, color = "T_skin_torso"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_skin_torso, group = 2, color = "T_skin_torso"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_skin_torso, group = 2, fill = "T_skin_torso"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(y = T_skin_hand, group = 3, color = "T_skin_hand"), fun = mean, geom = "point", shape = 16, size = 4) +
  stat_summary(aes(y = T_skin_hand, group = 3, color = "T_skin_hand"), fun = mean, geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = T_skin_hand, group = 3, fill = "T_skin_hand"), fun.data = mean_se, geom = "ribbon", alpha = 0.1) +
  labs(title = "SKIN temp last 5 min of sitting",
       x = "HA-session",
       y = "Skin temp",
       color = "Variable",
       fill = "Variable") +
  scale_color_manual(values = c("IB" = "orange", "T_skin_torso" = "blue", "T_skin_hand" = "gray")) +
  scale_fill_manual(values = c("IB" = "orange", "T_skin_torso" = "blue", "T_skin_hand" = "gray"))

  

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
