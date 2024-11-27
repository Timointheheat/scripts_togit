library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)

# README 
## This is done with many file missing. Wait for Puck.


# STEP ) Get Masterfile----
Masterfile <- read_excel(paste0(here("data_output"), "/HA_Masterfile.xlsx")) 


# STEP ) Answer if CORE can be used to track HA-adaptations ----

## Get average values for Tsk, Trec and Tcore in rest (min 10-15) and constant exercise (min 40-45)
HA_adaptations <- Masterfile %>%
  group_by(pp, ha) %>%
  summarise(
    Trec_rest_mean = mean(Trec[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tcore_torso_rest_mean = mean(T_core_torso_new[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tsk_ib_rest_mean = mean(Tsk_average[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tsk_core_torso_rest_mean = mean(T_skin_torso[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Trec_ex_mean = mean(Trec[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tcore_torso_ex_mean = mean(T_core_torso_new[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tcore_hand_ex_mean = mean(T_core_hand_new[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tsk_ib_ex_mean = mean(Tsk_average[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tsk_core_torso_ex_mean = mean(T_skin_torso[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    )



# STEP ) Answer useful for CH-HA question---- 
## Remove rows where either Trec or Tcore_torso is NA.
Data_Q_ch <-  Masterfile %>%
  filter(!is.na(Trec) & !is.na(T_core_torso_new)) %>%
  #filter(!pp == 7) %>%
  dplyr::select(pp, ha, Trec, T_core_torso_new)


## Calculate time above 38.5
time_above_385 <- Data_Q_ch %>%
  group_by(pp, ha) %>%
  summarise(count_Trec = sum(Trec >= 38.5, na.rm = TRUE),
            count_Tcore_torso = sum(T_core_torso_new >= 38.5, na.rm = TRUE)) %>%
  mutate(diff = count_Trec - count_Tcore_torso)

## Visually inspect data
plot(time_above_385$diff)

## Is the data DIFFERENCE normally distributed?
hist(time_above_385$diff, main = "Histogram of Differences", xlab = "Differences")
qqnorm(time_above_385$diff)
qqline(time_above_385$diff, col = "red")
shapiro.test(time_above_385$diff)

## Is normally distributed execute paired-samples t-test. If not, wilcox np test.
wilcox_test_result <- wilcox.test(time_above_385$count_Trec, time_above_385$count_Tcore_torso, paired = TRUE)
print(wilcox_test_result)

## Calculate the average difference
descr <- time_above_385 %>%
  ungroup() %>%
  group_by(pp) %>%
  summarise(avg_diff = mean(diff),
            sd_diff = sd(diff))
  
