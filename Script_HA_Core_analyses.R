library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(psych)
library(MASS)
library(irr)
library(BlandAltmanLeh)
library(EnvStats)

# README 


# STEP 1) Get Masterfile----
Masterfile <- read_excel(paste0(here("data/data_output"), "/HA_Masterfile_cleaned_2412025.xlsx")) 


# STEP 2) Visualize data
plotvariable <- "Trec"
total_samples <- sum(!is.na(Masterfile[[plotvariable]]))
Masterfile %>%
  ggplot(aes_string(x = "Minutes", y = plotvariable)) +
  geom_point(alpha = 0.5, size = 3.0, color = "#D55E00") +
  publication_theme() +
  annotate("text", x = Inf, y = Inf, label = paste("N:", total_samples), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  labs(title = plotvariable)

long_Masterfile <- Masterfile %>%
  dplyr::select(pp, ha, Minutes, Trec, T_core_torso_new) %>%
  pivot_longer(cols = starts_with("T"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "Trec", "rec", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "rec", -0.1, 0.1))


mean_values_Tc <- long_Masterfile %>%
  group_by(Minutes, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "rec", -0.1, 0.1))


mean_values_Tc %>%
  ggplot(aes(x = Minutes, y = value, color = device)) +
  geom_point(alpha = 0.5, size = 3.0) +
  publication_theme() +
  labs(title = "Average Rec and CORE over all sessions",
       y = "Tc (C)",
       x = "Time (min)")



check <- Masterfile %>%
  filter(!!sym(plotvariable) < 36) %>%
  dplyr::select(pp, ha, Minutes, !!sym(plotvariable))


# STEP 3) Answer if CORE can be used to track HA-adaptations ----

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
    dTc_rec_rest_mean = ifelse(ha == 1, 0, Tc_rec_rest_mean - first(Tc_rec_rest_mean)),
    dTsk_ib_rest_mean = ifelse(ha == 1, 0, Tsk_ib_rest_mean - first(Tsk_ib_rest_mean)),
    dTc_core_torso_rest_mean = ifelse(ha == 1, 0, Tc_core_torso_rest_mean - first(Tc_core_torso_rest_mean)),
    dTc_core_hand_rest_mean = ifelse(ha == 1, 0, Tc_core_hand_rest_mean - first(Tc_core_hand_rest_mean)),
    dTsk_core_torso_rest_mean = ifelse(ha == 1, 0, Tsk_core_torso_rest_mean - first(Tsk_core_torso_rest_mean)),
    dTsk_core_hand_rest_mean = ifelse(ha == 1, 0, Tsk_core_hand_rest_mean - first(Tsk_core_hand_rest_mean)),
    
    dTc_rec_ex_mean = ifelse(ha == 1, 0, Tc_rec_ex_mean - first(Tc_rec_ex_mean)),
    dTsk_ib_ex_mean = ifelse(ha == 1, 0, Tsk_ib_ex_mean - first(Tsk_ib_ex_mean)),
    dTc_core_torso_ex_mean = ifelse(ha == 1, 0, Tc_core_torso_ex_mean - first(Tc_core_torso_ex_mean)),
    dTc_core_hand_ex_mean = ifelse(ha == 1, 0, Tc_core_hand_ex_mean - first(Tc_core_hand_ex_mean)),
    dTsk_core_torso_ex_mean = ifelse(ha == 1, 0, Tsk_core_torso_ex_mean - first(Tsk_core_torso_ex_mean)),
    dTsk_core_hand_ex_mean = ifelse(ha == 1, 0, Tsk_core_hand_ex_mean - first(Tsk_core_hand_ex_mean)),
    
    dHR_rest_mean = ifelse(ha == 1, 0, HR_rest_mean - first(HR_rest_mean)),
    dHR_ex_mean = ifelse(ha == 1, 0, HR_ex_mean - first(HR_ex_mean))
  )



## C) Check normality ----
check_normaliteit <- function(variable) {
  # Store results in a list
  results <- list()
  # Add summary statistics
  results$summary <- summary(variable)
  # Create plots
  par(mfrow = c(1,3))
  hist(variable, prob = TRUE)
  curve(dnorm(x, mean = mean(variable), sd = sd(variable)), add = TRUE)
  boxplot(variable)
  qqnorm(variable)
  qqline(variable)
  # Calculate skewness
  n <- length(variable)
  mean_data <- mean(variable)
  sd_data <- sd(variable)
  skewness_value <- (n * sum((variable - mean_data)^3)) / ((n - 1) * (n - 2) * sd_data^3)
  se_skewness <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
  results$skewness <- list(
    skewness = skewness_value,
    se_skewness = se_skewness,
    is_normal_skew = abs(skewness_value / se_skewness) < 2
  )
  # Calculate kurtosis
  S2 <- sum((variable - mean_data)^2)
  S4 <- sum((variable - mean_data)^4)
  V <- S2/(n-1)
  kurtosis_value <- ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * (S4 / V^2) - (3 * (n - 1)^2 / ((n - 2) * (n - 3)))
  se_kurtosis <- sqrt(24 * n * (n - 1)^2 / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
  results$kurtosis <- list(
    kurtosis = kurtosis_value,
    se_kurtosis = se_kurtosis,
    is_normal_kurt = abs(kurtosis_value / se_kurtosis) < 2
  )
  # Add statistical tests with error handling
  if (n >= 3 && n <= 5000) {
    results$shapiro <- shapiro.test(variable)
  } else {
    results$shapiro <- paste("Shapiro-Wilk test not applicable: sample size", n, 
                             "must be between 3 and 5000")
  }
  # Kolmogorov-Smirnov test
  tryCatch({
    results$ks <- ks.test(variable, "pnorm", mean = mean(variable), sd = sd(variable))
  }, error = function(e) {
    results$ks <- paste("Kolmogorov-Smirnov test failed:", e$message)
  })
  return(results)
}

plot(Changes_overHA$ha, Changes_overHA$dTc_core_hand_ex_mean)

normalitycheckdata <- Changes_overHA %>%
  # ungroup() %>%
  filter(!pp %in% c(25))%>%
  filter(!is.na(dTc_core_hand_ex_mean))%>%
  filter(ha == 3 ) %>%
  dplyr:: select(pp, ha, dTc_core_hand_ex_mean)

check_normaliteit(normalitycheckdata$dTc_core_hand_ex_mean)

## Changes_overHA normality distribution: 
### dTre: rest & ex YES (excl pp25) - ALL DATA
#### rest Y: HA2,4-9 
#### rest N: HA3
#### ex Y: HA2-9
### dTCore_torso: rest & ex YES (excl pp25) - ALL DATA
#### rest Y: HA2-9
#### ex Y: HA2-4,7
#### ex N: HA5, 6, 8, 9
### dTCore_hand: rest & ex NO (excl pp25)
#### rest Y: HA2-7
#### rest N: HA8,9
#### ex N: HA2, 5-8
#### ex N: HA3, 4, 9

## timeabove 38.5 normality distribution: 
### min diff REC-CORE:  N > 50 --> KS-test: p = 0.08. Just-about normally distributed. 

## overall Tc measured by REC and CORE: 
### very large sample size, can be assumed normal. 

Changes_overHA %>%
  filter(dTc_core_hand_ex_mean  > 0.7)


#p25 has very high values for ha1, I think these were incorrect.
## This influences all other REC & CORE results. Exclude from all REC analysis? 

#p3 has not executed entire 



## D) Get BA-analysis and ICC-scores ----
library(psych)
library(MASS)
library(irr)
library(BlandAltmanLeh)

### create a square scatterplot with a 1/1 y/x aspect ratio
### get describtives 
par(pty="s")
plot(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean, asp = 1,)
abline(0,1)

describe(Changes_overHA$dTc_rec_rest_mean)
describe(Changes_overHA$dTc_core_torso_rest_mean)


### ICC from package irr
icc <- icc(cbind(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean),
           model="twoway", type="agreement", unit="single")
icc


### BA-analysis from package BlandAltmanLeh 
bland.altman.plot(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean,
                  main="Bland Altman Plot",
                  xlab="Means", ylab="Differences", silent=FALSE)

BAresult <- bland.altman.stats(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean,
                               two = 1.96, mode = 1, conf.int = 0.95)
BAresult$lower.limit
BAresult$upper.limit


diff_dTc_ex <- Changes_overHA %>%
  mutate(diff = dTc_rec_ex_mean - dTc_core_torso_ex_mean) %>%
  dplyr::select(pp, ha, diff)


mean_diff_dTc_ex <- diff_dTc_ex %>%
  group_by(ha) %>%
  summarise(diff = mean(diff, na.rm = TRUE))

diff_dTc_ex %>%
  filter(!pp == 25) %>%
  ggplot(aes( x = as.factor(ha), y = diff)) +
  geom_point() +
  geom_point(data = mean_diff_dTc_ex, aes(y = diff),size = 4, shape = 4, stroke = 2) +
  publication_theme() +
  theme(panel.grid = element_blank()) +
  labs(x = "HA-session",
       y = "Difference in exercising dTc")





## D) create longformat for each variable ----
### Resting Core and skin
long_dTc_rest <- Changes_overHA %>%
  dplyr::select(pp, ha, dTc_rec_rest_mean, dTc_core_torso_rest_mean) %>%
  pivot_longer(cols = starts_with("dTc"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "dTc_rec_rest_mean", "rec", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "rec", -0.1, 0.1)) %>%
  ungroup() %>%                                                       # Impute using average from value above and below
  group_by(pp, device) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()

  

mean_values_dTc_rest <- long_dTc_rest %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "rec", -0.1, 0.1))


# long_Tsk_rest <- Means %>%
#   dplyr::select(pp, ha, Tsk_ib_rest_mean, Tsk_core_torso_rest_mean) %>%
#   pivot_longer(cols = starts_with("Tsk"), names_to = "device", values_to = "value") %>%
#   mutate(device = ifelse(device == "Tsk_ib_rest_mean", "ib", "core"),
#          ha = as.factor(ha),
#          device = as.factor(device),
# #          nudge_x = ifelse(device == "ib", -0.1, 0.1))%>%
# ungroup() %>%                                                       # Impute using average from value above and below
#   group_by(pp, device) %>%
#   mutate(value = na.approx(value, na.rm = FALSE)) %>%
#   ungroup()
# 
# mean_values_Tsk_rest <- long_Tsk_rest %>%
#   group_by(ha, device) %>%
#   summarise(value = mean(value, na.rm = TRUE)) %>%
#   mutate(nudge_x = ifelse(device == "ib", -0.1, 0.1))



### Excercising core and skin
long_dTc_ex <- Changes_overHA %>%
  dplyr::select(pp, ha, dTc_rec_ex_mean, dTc_core_torso_ex_mean) %>%
  pivot_longer(cols = starts_with("dTc"), names_to = "device", values_to = "value") %>%
  mutate(device = ifelse(device == "dTc_rec_ex_mean", "rec", "core"),
         ha = as.factor(ha),
         device = as.factor(device),
         nudge_x = ifelse(device == "rec", -0.1, 0.1))%>%
  ungroup() %>%                                                       # Impute using average from value above and below
  group_by(pp, device) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()

mean_values_Tc_ex <- long_dTc_ex %>%
  group_by(ha, device) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(nudge_x = ifelse(device == "rec", -0.1, 0.1))


# long_Tsk_ex <- Means %>%
#   dplyr::select(pp, ha, Tsk_ib_ex_mean, Tsk_core_torso_ex_mean) %>%
#   pivot_longer(cols = starts_with("Tsk"), names_to = "device", values_to = "value") %>%
#   mutate(device = ifelse(device == "Tsk_ib_ex_mean", "ib", "core"),
#          ha = as.factor(ha),
#          device = as.factor(device),
#          nudge_x = ifelse(device == "ib", -0.1, 0.1))%>%
# ungroup() %>%                                                       # Impute using average from value above and below
#   group_by(pp, device) %>%
#   mutate(value = na.approx(value, na.rm = FALSE)) %>%
#   ungroup()
# 
# mean_values_Tsk_ex <- long_Tsk_ex %>%
#   group_by(ha, device) %>%
#   summarise(value = mean(value, na.rm = TRUE)) %>%
#   mutate(nudge_x = ifelse(device == "ib", -0.1, 0.1))



## E) Look at data -----
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
long_dTc_rest %>%
  filter(!(pp %in% c(5, 6, 13, 24, 25))) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_dTc_rest$nudge_x), alpha = 0.5, size = 2.0) +
  geom_line(data = mean_values_dTc_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_dTc_rest$nudge_x), size = 1.5) +
  geom_point(data = mean_values_dTc_rest, aes(y = value, color = device, group = device), position = position_nudge(x = mean_values_dTc_rest$nudge_x), size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = colorblind_colors) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "dTc in rest",
       color = "Device")



# View the updated tibble
print(long_dTc_rest)




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
long_dTc_ex %>%
  filter(!(pp == 25)) %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = value, color = device), position = position_nudge(x = long_dTc_ex$nudge_x), alpha = 0.5, size = 2.0) +
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



long_dTc_ex %>%
  filter(! pp %in% c(2, 3, 4, 5, 6, 13, 24, 25)) %>%
  ggplot(aes(x = ha, y = value, group = interaction(pp, device), color = device)) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  publication_theme() +
  scale_color_manual(values = colorblind_colors)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.99, 0.99),  # Position legend inside plot
        legend.justification = c("right", "top")) + 
  labs(x = "HA-session",
       y = "dTc exercise",
       color = "Device")

## E) Execute mixed-model  ----
ncount <- long_dTc_rest %>%
  filter(!is.na(value)) %>%
  group_by(pp, device) %>%
  summarize(ha_count = n_distinct(ha)) %>%
  filter(ha_count == 9)

%>%
  ungroup() %>%
  group_by(pp) %>%
  filter(n_distinct(device) == 2)%>%
  nrow()

ncount/2



ANOVA_results <- anova_test(long_dTc_rest, 
                            dv = value, 
                            wid = pp,
                            between = device, 
                            within = ha)


## Count the amount of pp with a value for each device for all HA-sessions. 
pp_ha_device_counts <- long_dTc_ex %>%
  filter(!is.na(value)) %>%
  group_by(pp, ha) %>%
  summarize(device_count = n_distinct(device)) %>%
  filter(device_count == 2) %>%
  ungroup() %>%
  group_by(pp) %>%
  summarize(ha_count = n_distinct(ha))

%>%
  filter(ha_count >= 6) %>%
  nrow()

pp_ha_device_counts



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
  


install.packages("irr")
install.packages("BlandAltmanLeh")

library(psych)
library(MASS)
library(irr)
library(BlandAltmanLeh)

# Create a scatterplot
par(pty="s")
plot(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean, xlim=c(-1,1), ylim=c(-1,1), asp=1)
abline(0,1)

describe(Changes_overHA$dTc_rec_rest_mean)
describe(Changes_overHA$dTc_core_torso_rest_mean)

ttest <- t.test(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean, paired=TRUE)

icc <- icc(cbind(Changes_overHA$dTc_rec_rest_mean,Changes_overHA$dTc_core_torso_rest_mean ), model="twoway", type="agreement", unit="single")

use <- Masterfile%>%
  filter(Minutes %in% c(0:45)) 
bland.altman.plot(use$T_core_torso_new, use$Trec, main="Bland Altman Plot", xlab="Means", ylab="Differences", silent=FALSE)

BAresult <- bland.altman.stats(use$T_core_torso_new, use$Trec, two = 1.96, mode = 1, conf.int = 0.95)
BAresult$lower.limit
BAresult$upper.limit
