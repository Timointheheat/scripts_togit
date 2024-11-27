library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)

# Do Step 1-3 only once. For further analyses go to step 4.
# STEP 1) Get Masterfile Pooh with cleaned Tre and HR ----
Masterfile <- read_excel(paste(here("data_cleaned"), "/Masterfile CORE + Trec.xlsx", sep = ""),
                         na = c("NA", "", "_", "-")) %>%
  mutate(session = str_remove(session, "ha"),
         ha = as.numeric(session),
         participant = str_remove(participant, "p"),
         pp = as.numeric(participant),
         Date.Time = time,
         T_skin_torso = as.numeric(Avg_T_skin_torso),
         T_core_torso = as.numeric(Avg_T_core_torso),
         T_skin_hand = as.numeric(Avg_T_skin_hand),
         T_core_hand = as.numeric(Avg_T_core_hand),
         Trec = as.numeric(mean_Trec)) %>%
  group_by(pp, ha) %>%
  # mutate(Minutes = row_number(),
  #        Minutes = as.numeric(Minutes)) %>%
  ungroup() %>%
  dplyr::select(pp, ha, Date.Time, T_skin_torso, T_core_torso, T_skin_hand, T_core_hand, Trec, Avg_HR_torso)

# STEP 2) Get cleaned IB data ----
longformat_Tsk <- read_excel(paste0(here("data_output"), "/HA_longformat_Tsk")) %>%
  mutate(ha = hst,
         Date.Time = Date.Time - 1) %>%
  dplyr::select(!hst)

# STEP 3) Merge cleaned IB data with Masterfile----
Masterfile <- longformat_Tsk %>%
  left_join(Masterfile, by = c("pp","ha", "Date.Time"))

write.xlsx(Masterfile,
           file = file.path(paste0(here("data_output"), 
                                   "/HA_Masterfile.xlsx")))



# STEP 4) Calculate Tbody----
Masterfile <- Masterfile %>%
  mutate(Tbody = 0.8 * Trec + 0.2 * Tsk_average,
         Tbody_core_torso = 0.8 * T_core_torso + 0.2 * T_skin_torso,
         Tbody_core_hand = 0.8 * T_core_hand + 0.2 * T_skin_hand)

# STEP 5) Get Masterfile including all cleaned data. ----
Masterfile <- read_excel(paste(here("data_output"), "/HA_Masterfile.xlsx", sep = ""),
                         na = c("NA", "", "_", "-"))

# STEP 6) Further clean data ----
## execute pp not doing all HA-sessions.
## Trec, I am doubting Masterfile. Have to re do.
## Tsk, Recently done. Should be good. 
## HR 
## HR < 40, make NA because unlikely physiologically.
## HR < 100 after cycling for 10 minutes, make NA because unlikely physiologically.
## CORE with HR is NA, make NA because HR used as input parameter.
pp_to_exclude <- c(2, 3, 4, 5, 6, 10, 13, 22, 24)

Masterfile <- Masterfile %>%
  mutate(Avg_HR_torso = ifelse(Avg_HR_torso < 40, NA, 
                               ifelse(Avg_HR_torso < 100 & Minutes >= 25 & Minutes <= 45, NA, Avg_HR_torso)),
         T_core_torso = ifelse(is.na(Avg_HR_torso), NA, T_core_torso),
         T_core_hand = ifelse(is.na(Avg_HR_torso), NA, T_core_hand),
         T_skin_torso = ifelse(is.na(Avg_HR_torso), NA, T_skin_torso),
         T_skin_hand = ifelse(is.na(Avg_HR_torso), NA, T_skin_hand)
  ) %>%
  filter(!pp %in% pp_to_exclude)




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
