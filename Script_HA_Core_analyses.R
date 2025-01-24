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


# STEP 1) Get Masterfile----
Masterfile <- read_excel(paste0(here("data/data_output"), "/HA_Masterfile_cleaned_2412025.xlsx")) 


# STEP 2) Answer if CORE can be used to track HA-adaptations ----

## A) Get average values for Tsk, Trec and Tcore in rest (min 10-15) and constant exercise (min 40-45) ----
Means <- Masterfile %>%
  group_by(pp, ha) %>%
  summarise(
    Tc_rec_rest_mean = mean(Trec[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tsk_ib_rest_mean = mean(Tsk_average[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tc_core_torso_rest_mean = mean(T_core_torso_new[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tc_core_hand_rest_mean = mean(T_core_hand_new[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tsk_core_torso_rest_mean = mean(T_skin_torso[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tsk_core_hand_rest_mean = mean(T_skin_hand[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    Tc_rec_ex_mean = mean(Trec[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tc_core_torso_ex_mean = mean(T_core_torso_new[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tc_core_hand_ex_mean = mean(T_core_hand_new[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tsk_ib_ex_mean = mean(Tsk_average[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tsk_core_torso_ex_mean = mean(T_skin_torso[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    Tsk_core_hand_ex_mean = mean(T_skin_hand[Minutes >= 40 & Minutes <= 45], na.rm = TRUE),
    HR_rest_mean = mean(HR_polar[Minutes >= 10 & Minutes <= 15], na.rm = TRUE),
    HR_ex_mean = mean(HR_polar[Minutes >= 40 & Minutes <= 45], na.rm = TRUE))


## B) Get the change in values by substracting the first HA for each pp -----
Changes_overHA <- Means %>%
  group_by(pp) %>%
  mutate(
    dTc_rec_rest_mean = Tc_rec_rest_mean - first(Tc_rec_rest_mean),
    dTsk_ib_rest_mean = Tsk_ib_rest_mean - first(Tsk_ib_rest_mean),
    dTc_core_torso_rest_mean = Tc_core_torso_rest_mean - first(Tc_core_torso_rest_mean),
    dTc_core_hand_rest_mean = Tc_core_hand_rest_mean - first(Tc_core_hand_rest_mean),
    dTsk_core_torso_rest_mean = Tsk_core_torso_rest_mean - first(Tsk_core_torso_rest_mean),
    dTsk_core_hand_rest_mean = Tsk_core_hand_rest_mean - first(Tsk_core_hand_rest_mean),
    
    dTc_rec_ex_mean = Tc_rec_ex_mean - first(Tc_rec_ex_mean),
    dTsk_ib_ex_mean = Tsk_ib_ex_mean - first(Tsk_ib_ex_mean),
    dTc_core_torso_ex_mean = Tc_core_torso_ex_mean - first(Tc_core_torso_ex_mean),
    dTc_core_hand_ex_mean = Tc_core_hand_ex_mean - first(Tc_core_hand_ex_mean),
    dTsk_core_torso_ex_mean = Tsk_core_torso_ex_mean - first(Tsk_core_torso_ex_mean),
    dTsk_core_hand_ex_mean = Tsk_core_hand_ex_mean - first(Tsk_core_hand_ex_mean),
    
    dHR_rest_mean = HR_rest_mean - first(HR_rest_mean),
    dHR_ex_mean = HR_ex_mean - first(HR_ex_mean),
    
  )%>%
  filter(!ha ==1)


## C) create longformat for each variable ----
### Resting Core and skin
long_Tc_rest <- Means %>%
  dplyr::select(pp, ha, Tc_rec_rest_mean, Tc_core_torso_rest_mean) %>%
  pivot_longer(cols = starts_with("Tc"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "Tc_rec_rest_mean", "rec", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "rec", -0.1, 0.1))

mean_values_Tc_rest <- long_Tc_rest %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "rec", -0.1, 0.1))


long_Tsk_rest <- Means %>%
  dplyr::select(pp, ha, Tsk_ib_rest_mean, Tsk_core_torso_rest_mean) %>%
  pivot_longer(cols = starts_with("Tsk"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "Tsk_ib_rest_mean", "ib", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "ib", -0.1, 0.1))

mean_values_Tsk_rest <- long_Tsk_rest %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "ib", -0.1, 0.1))



### Excercising core and skin
long_Tc_ex <- Means %>%
  dplyr::select(pp, ha, Tc_rec_ex_mean, Tc_core_torso_ex_mean) %>%
  pivot_longer(cols = starts_with("Tc"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "Tc_rec_ex_mean", "rec", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "rec", -0.1, 0.1))

mean_values_Tc_ex <- long_Tc_ex %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "rec", -0.1, 0.1))


long_Tsk_ex <- Means %>%
  dplyr::select(pp, ha, Tsk_ib_ex_mean, Tsk_core_torso_ex_mean) %>%
  pivot_longer(cols = starts_with("Tsk"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "Tsk_ib_ex_mean", "ib", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "ib", -0.1, 0.1))

mean_values_Tsk_ex <- long_Tsk_ex %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "ib", -0.1, 0.1))



## D) Look at data -----
publication_theme <- function() {
  theme_bw() +
    theme(
      panel.grid = element_blank(), 
      #text = element_text(size = 40),
      axis.line = element_line(linewidth = 2, color = "#066666"),
      panel.border = element_blank(),
      #title =  element_text(size = 40), # Adjust  title size
      axis.title = element_text(size = 25), # Adjust axis title size
      axis.text = element_text(size = 20),  # Adjust axis text size
      axis.ticks = element_line(size = 1.2),  # Adjust axis tick thickness
      axis.ticks.length = unit(0.3, "cm"), # Adjust tick length here
      plot.title = element_text(size = 18, margin = margin(b = 20)),
      legend.text = element_text(size = 10), # Adjust legend text size
      legend.title = element_text(size = 15),  # Adjust legend title size
      legend.key.size = unit(0.7, "cm"))   # Adjust overall legend size)                            
}
colorblind_colors <- c("rec" = "#D55E00", "core" = "#0072B2")
colorblind_colors_skin <- c("ib" = "#D55E00", "core" = "#0072B2")


### Resting core and skin
long_Tc_rest %>%
  filter(!(pp == 25)) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_Tc_rest$nudge_x), alpha = 0.5, size = 2.0) +
  geom_line(data = mean_values_Tc_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tc_rest$nudge_x), size = 1.5) +
  geom_point(data = mean_values_Tc_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tc_rest$nudge_x), size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = colorblind_colors) +
  publication_theme() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "Tc in rest",
       color = "Device")


long_Tsk_rest %>%
  #filter(!(pp == 25)) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_Tsk_rest$nudge_x), alpha = 0.5, size = 2.0) +
  geom_line(data = mean_values_Tsk_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tsk_rest$nudge_x), size = 1.5) +
  geom_point(data = mean_values_Tsk_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tsk_rest$nudge_x), size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = colorblind_colors_skin) +
  publication_theme() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "Tsk in rest",
       color = "Device")



### EXERCSING core and skin
long_Tc_ex %>%
  filter(!(pp == 25)) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_Tc_ex$nudge_x), alpha = 0.5, size = 2.0) +
  geom_line(data = mean_values_Tc_ex, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tc_ex$nudge_x), size = 1.5) +
  geom_point(data = mean_values_Tc_ex, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tc_ex$nudge_x), size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = colorblind_colors) +
  publication_theme() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "Tc in exercise",
       color = "Device")

long_Tsk_ex %>%
  #filter(!(pp == 25)) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_Tsk_ex$nudge_x), alpha = 0.5, size = 2.0) +
  geom_line(data = mean_values_Tsk_ex, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tsk_ex$nudge_x), size = 1.5) +
  geom_point(data = mean_values_Tsk_ex, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_Tsk_ex$nudge_x), size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = colorblind_colors_skin) +
  publication_theme() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "Tsk in ex",
       color = "Device")





## E) create model ----
library(lmerTest)
long_Tc_rest$device <- relevel(long_Tc_rest$device, ref = "rec")
long_Tc_ex$device <- relevel(long_Tc_ex$device, ref = "rec")
model_Tc_rest <- lmer(value ~ ha * device + (1 | pp), data = long_Tc_rest)
model_Tc_ex <- lmer(value ~ ha * device + (1 | pp), data = long_Tc_ex)

summary(model_Tc_rest)
summary(model_Tc_ex)











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
  
