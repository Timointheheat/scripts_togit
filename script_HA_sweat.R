library(dplyr)
library(here)
library(readxl)
library(ggpubr)
library(tidyr)
library(openxlsx)
library(stringr)
library(hms)
library(lubridate)
library(tidyverse)
library(rstatix)
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
      legend.key.size = unit(0.7, "cm"))   # Adjust overall legend size
}
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

## !!README!!README!!README!!
## Script used to download and analyse sweat loss, subjective measures and hydration status
# Step 1: get scoreform with all values ----
scoreform_HA <- read_excel(here("data/Scoreform_excel_HA.xlsx"), na = c("NA", "", "_", "-")) 

# Step 2: calculate WBSL ----
## To calculate whole-body sweat loss (WBSL) of the first 45 minutes, fully equipped and clothed body mass (CBM) and towel mass (TM) was obtained.
## WBSL_during = dCBM + dTM. during = after the first 45 minutes.
## dCBM = before - after. (Body loses sweat, therefore is heavier at the start. Unless a lot of drink)
## dTM  = after - before. (Towel gains sweat, therefore is heavier at end)
## To calculate WBSL of entire session we have to account for sweat that got trapped in clothes+equipment (CM) and water that was drunk.
## WBSL_after = dCBM + dTM + dCM - dWBM. after = after session.
## dCM = after - before (sweat gets trapped, therefore heavier at end)
## dWBM = before - after (water gets drunk, therefore heavier before)
## average of 2 measurements of CBM is taken. 


#### WERID ONES P16, HA = 6, HAS LOWER TM DURING THAN BEFORE... CHECK
#### WERID ONES P3, HA = 4, HAS very low dCBM but did not follow protocol. Sit in chamber without doing anythign. EXCLUDE.
#### WEIRD ONES P7, HA = 7, lower dCLM at the end. CHECK
#### WEIRD ONES P12, HA = 7, weirdly low WB_before and therefore. dWB = 0. Replace with  average of dWB  for this pp. 
#### WEIRD ONES P18, HA = 9, weirdly low WB_before and therefore. dWB = +number.Replace with  average of dWB  for this pp.  


## fully equipped and clothed body mass (CBM), and combined clothing and equipment mass (CEM), were obtained.
## during = after the first 45 minutes. After = after entire session. 

Sweat_variables <- scoreform_HA %>%
  mutate(m_after_cl_equip = ifelse(pp == 15 & nmbr_test == 8, 1181, m_after_cl_equip)) %>%
  mutate(ha = as.factor(nmbr_test),
         CBM_before = (m_before_ppclequip_1 + m_before_ppclequip_2) / 2, 
         TM_before = m_before_t,
         WBM_before = m_before_wb,
         CLM_before = m_before_cl_equip,
         CBM_during = (m_during_ppclequip_1 + m_during_ppclequip2) / 2,
         TM_during = m_during_t,
         dCBM_45 = CBM_before - CBM_during,
         dTM_45 =  TM_during - TM_before,
         CBM_after = (m_after_ppclequip_1 + m_after_ppclequip_2) /2,
         TM_after = m_after_t,
         WBM_after = m_after_wb,
         CLM_after = m_after_cl_equip,
         dCBM_after = CBM_before - CBM_after, 
         dTM_after = TM_after - TM_before,
         dWBM_after = WBM_after - WBM_before,
         dCLM_after =  CLM_after - CLM_before,
         duration = tm_end - time_start_15) %>%
  mutate(dWBM_after = ifelse(pp == 12 & ha == 7, -1814,dWBM_after),
         dWBM_after = ifelse(pp == 18 & ha == 9, -1215,dWBM_after)) %>%
  filter(type_test == "ha") %>%
  filter(!c( pp == 3 & ha == 4)) 

  
Sweat <- Sweat_variables %>%
  mutate(WBSL_45 = dCBM_45 + dTM_45,
         WBSL_after = dCBM_after + dTM_after + dCLM_after - dWBM_after,
         WBSL_ph_after = WBSL_after/ as.numeric(duration)) %>%
  dplyr::select(pp, ha, WBSL_45, WBSL_after, WBSL_ph_after)



Sweat %>%
  ggplot(aes(x = pp, y = WBSL_ph_after )) + geom_point()


# Calculate changes across HA-sessions
Changes_Sweat <- Sweat %>%
  group_by(pp) %>%
  mutate(dWBSL_45 = ifelse(ha == 1, 0, WBSL_45 - first(WBSL_45)),
         dWBSL_after = ifelse(ha == 1, 0, WBSL_after - first(WBSL_after)),
         dWBSL_ph_after = ifelse(ha == 1, 0, WBSL_ph_after - first(WBSL_ph_after))) %>%
  dplyr::select(pp, ha, dWBSL_45, dWBSL_after, dWBSL_ph_after)



## Step 3: Calculate average and 95%CI ----
Mean_Sweat <- Sweat %>%
  group_by(ha) %>%
  summarise(
    Mean_WBSL_45 = mean(WBSL_45),
    CI_lower_45 = Mean_WBSL_45 - 1.96 * sd(WBSL_45) / sqrt(n()),
    CI_upper_45 = Mean_WBSL_45 + 1.96 * sd(WBSL_45) / sqrt(n()),
    Mean_WBSL_after = mean(WBSL_after),
    CI_lower_after = Mean_WBSL_after - 1.96 * sd(WBSL_after) / sqrt(n()),
    CI_upper_after = Mean_WBSL_after + 1.96 * sd(WBSL_after) / sqrt(n()),
    Mean_WBSL_ph_after = mean(WBSL_ph_after),
    CI_lower_ph_after = Mean_WBSL_ph_after - 1.96 * sd(WBSL_ph_after) / sqrt(n()),
    CI_upper_ph_after = Mean_WBSL_ph_after + 1.96 * sd(WBSL_ph_after) / sqrt(n())
)

Mean_Changes_Sweat <- Changes_Sweat %>%
  group_by(ha) %>%
  summarise(
    Mean_Changes_WBSL_45 = mean(dWBSL_45),
    CI_lower_45 = Mean_Changes_WBSL_45 - 1.96 * sd(dWBSL_45) / sqrt(n()),
    CI_upper_45 = Mean_Changes_WBSL_45 + 1.96 * sd(dWBSL_45) / sqrt(n()),
    Mean_Changes_WBSL_after = mean(dWBSL_after),
    CI_lower_after = Mean_Changes_WBSL_after - 1.96 * sd(dWBSL_after) / sqrt(n()),
    CI_upper_after = Mean_Changes_WBSL_after + 1.96 * sd(dWBSL_after) / sqrt(n()),
    Mean_Changes_WBSL_ph_after = mean(dWBSL_ph_after),
    CI_lower_ph_after = Mean_Changes_WBSL_ph_after - 1.96 * sd(dWBSL_ph_after) / sqrt(n()),
    CI_upper_ph_after = Mean_Changes_WBSL_ph_after + 1.96 * sd(dWBSL_ph_after) / sqrt(n())
    
  )






## Step 4: ALL DATA: Sweat average Plots  ----
Sweat %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = WBSL_45), alpha = 0.5, size = 5.0) +
  geom_point(data = Mean_Sweat, aes(y = Mean_WBSL_45), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = Mean_Sweat, aes(ymin = CI_lower_45, ymax = CI_upper_45), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "WBSL first 45 min across HA-sessions",
       x = "HA-session",
       y = "WBSL (ml)")

Sweat %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = WBSL_ph_after), alpha = 0.5, size = 5.0) +
  geom_point(data = Mean_Sweat, aes(y = Mean_WBSL_ph_after), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = Mean_Sweat, aes(ymin = CI_lower_ph_after, ymax = CI_upper_ph_after), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "WBSL/ hour of entire HA-sessions",
       x = "HA-session",
       y = "WBSL (ml/hour)")

Sweat %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = WBSL_ph_after), alpha = 0.5, size = 5.0) +
  geom_point(data = Mean_Sweat, aes(y = Mean_WBSL_ph_after), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = Mean_Sweat, aes(ymin = CI_lower_after, ymax = CI_upper_after), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "WBSL of entire HA-sessions",
       x = "HA-session",
       y = "WBSL (ml)")


Changes_Sweat %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = dWBSL_45), alpha = 0.5, size = 5.0) +
  geom_point(data = Mean_Changes_Sweat, aes(y = Mean_Changes_WBSL_45), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = Mean_Changes_Sweat, aes(ymin = CI_lower_45, ymax = CI_upper_45), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "WBSL first 45 min across HA-sessions",
       x = "HA-session",
       y = "WBSL (ml)")


## Step 5: ALL DATA: Sweat Individual lines Plots  ----


Sweat %>%
  ggplot(aes(x = ha, y = WBSL_45, group = pp, color = as.factor(pp))) +
  geom_line(size = 1.50) +
  publication_theme() +
  labs(title = "WBSL ind lines",
       x = "HA-session",
       y = "WBSL (ml)")


Changes_Sweat %>%
  ggplot(aes(x = ha, y = dWBSL_45, group = pp, color = as.factor(pp))) +
  geom_line(size = 1.50) +
  publication_theme() +
  labs(title = "dWBSL ind lines",
       x = "HA-session",
       y = "dWBSL (ml)")


## Step 6: Repeated measures ANOVA ----


#### A) General descriptives
Sweat %>%
  dplyr::select(pp, ha, WBSL_45) %>%
  group_by(ha) %>%
  get_summary_stats(WBSL_45, type = "mean_sd")

ggboxplot(Sweat, x = "ha", y = "WBSL_45", add = "point")  


### B) Identify outliers
#### P4 en P25 are identified as outliers in ha1, 2, 9. 
#### However, not extreme ones. Olso, these pp tend to sweat a lot during other sessions. 
#### Leave them in.
Sweat %>%
  group_by(ha) %>%
  identify_outliers(WBSL_45)


### C) Normality check
#### Data not always normally distributed. Use non-paramatric test.
Sweat %>%
  group_by(ha) %>%
  shapiro_test(WBSL_45)
ggqqplot(Sweat, "WBSL_45", facet.by = "ha")

check_normaliteit(Sweat$WBSL_45[Sweat$ha == 2])



### D) Repeated Measures ANOVA 
## 1) Filter only pp who did entire HA-period
Sweat_filtered <- Sweat %>%
  convert_as_factor(pp, ha) %>%
  group_by(pp) %>%
  filter(all(1:9 %in% as.integer(ha))) %>%
  ungroup()%>%
  dplyr::select(pp, ha, WBSL_45)


## 2A) Execute Parametric Repeated measures.
res.aov <- anova_test(data = Sweat_filtered, dv = WBSL_45, wid = pp, within = ha)
get_anova_table(res.aov)

## 3A) Post-hoc comparisons
pwc_para <- Sweat_filtered %>%
  pairwise_t_test(
    WBSL_45 ~ ha, paired = TRUE)
pwc_para


# 3A) Post-hoc comparisons with only HA1 + bonferoni correction
reference_ha <- "1"
pwc_filtered_para <- pwc_para %>%
  filter(group1 == reference_ha | group2 == reference_ha)
pwc_filtered_para <- pwc_filtered_para %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))



## 2B) NP Friedman test
complete_pp <- Sweat %>%
  group_by(pp) %>%
  filter(n_distinct(ha) == 9) %>%
  ungroup() %>%
  arrange(pp)

res.fried <- complete_pp %>% friedman_test(WBSL_45 ~ ha |pp)
res.fried

## 3B) Kendall's W effect size
res.kend <- complete_pp %>% friedman_effsize(WBSL_45 ~ ha |pp)

## 4B) Post-hoc 
pwc_np <- complete_pp %>%
  wilcox_test(WBSL_45 ~ ha, paired = TRUE)
reference_ha <- "1"
pwc_filtered_np <- pwc_np %>%
  filter(group1 == reference_ha | group2 == reference_ha)
pwc_filtered_np <- pwc_filtered_np %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))



## Step 7: COMPLETE PP ONLY: visualize data ----
complete_pp_Changes_Sweat <- complete_pp %>%
  group_by(pp) %>%
  mutate(dWBSL_45 = ifelse(ha == 1, 0, WBSL_45 - first(WBSL_45))) %>%
  dplyr::select(pp, ha, dWBSL_45)


complete_pp_Mean_Sweat <- complete_pp %>%
  group_by(ha) %>%
  summarise(
    Mean_WBSL_45 = mean(WBSL_45),
    CI_lower_45 = Mean_WBSL_45 - 1.96 * sd(WBSL_45) / sqrt(n()),
    CI_upper_45 = Mean_WBSL_45 + 1.96 * sd(WBSL_45) / sqrt(n()))

complete_Mean_Changes_Sweat <- complete_pp_Changes_Sweat %>%
  group_by(ha) %>%
  summarise(
    Mean_Changes_WBSL_45 = mean(dWBSL_45),
    CI_lower_45 = Mean_Changes_WBSL_45 - 1.96 * sd(dWBSL_45) / sqrt(n()),
    CI_upper_45 = Mean_Changes_WBSL_45 + 1.96 * sd(dWBSL_45) / sqrt(n()))


complete_pp %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = WBSL_45), alpha = 0.5, size = 5.0) +
  geom_point(data = complete_pp_Mean_Sweat, aes(y = Mean_WBSL_45), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = complete_pp_Mean_Sweat, aes(ymin = CI_lower_45, ymax = CI_upper_45), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp WBSL first 45 min",
       x = "HA-session",
       y = "WBSL (ml)")

complete_pp_Changes_Sweat %>%
  ggplot(aes(x = ha)) +
  geom_point(aes(y = dWBSL_45), alpha = 0.5, size = 5.0) +
  geom_point(data = complete_Mean_Changes_Sweat, aes(y = Mean_Changes_WBSL_45), size = 10, shape = 4, stroke = 4) +
  geom_errorbar(data = complete_Mean_Changes_Sweat, aes(ymin = CI_lower_45, ymax = CI_upper_45), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp dWBSL first 45 min across HA-sessions",
       x = "HA-session",
       y = "dWBSL (ml)")



## Step 8: Extract and do calculations on Comfort, sensation and RPE at the end of 45 min. ----
feelings <- scoreform_HA %>%
  mutate(pp = as.factor(pp),
         ha = as.factor(nmbr_test), 
         RPE_start = rpe_0,
         TS_start = sensation_0,
         TC_start = comfort_0,
         RPE_end = rpe_30,
         TS_end = sensation_30,
         TC_end = comfort_30) %>%
  filter(type_test == "ha") %>%
  filter(!c( pp == 3 & ha == 4))  %>%
  dplyr::select(pp, ha, RPE_end, TS_end, TC_end, RPE_start, TS_start, TC_start) %>%
  mutate(RPE_end = ifelse(pp == 8 & ha == 2, 4, RPE_end),
         TS_end = ifelse(pp == 8 & ha == 2, 4, TS_end),
         TC_end = ifelse(pp == 8 & ha == 2, 3, TC_end)) %>%
  arrange(pp)

complete_pp_feelings <- feelings %>%
  group_by(pp) %>%
  filter(n_distinct(ha) == 9) %>%
  ungroup() 

complete_pp_Mean_feeling <- complete_pp_feelings %>%
  group_by(ha) %>%
  summarise(
    Mean_RPE_end = mean(RPE_end),
    CI_lower_RPE_end = Mean_RPE_end - 1.96 * sd(RPE_end) / sqrt(n()),
    CI_upper_RPE_end = Mean_RPE_end + 1.96 * sd(RPE_end) / sqrt(n()),
    Mean_TS_end = mean(TS_end),
    CI_lower_TS_end = Mean_TS_end - 1.96 * sd(TS_end) / sqrt(n()),
    CI_upper_TS_end = Mean_TS_end + 1.96 * sd(TS_end) / sqrt(n()),
    Mean_TC_end = mean(TC_end),
    CI_lower_TC_end = Mean_TC_end - 1.96 * sd(TC_end) / sqrt(n()),
    CI_upper_TC_end = Mean_TC_end + 1.96 * sd(TC_end) / sqrt(n()),
    
    Mean_RPE_start = mean(RPE_start),
    CI_lower_RPE_start = Mean_RPE_start - 1.96 * sd(RPE_start) / sqrt(n()),
    CI_upper_RPE_start = Mean_RPE_start + 1.96 * sd(RPE_start) / sqrt(n()),
    Mean_TS_start = mean(TS_start),
    CI_lower_TS_start = Mean_TS_start - 1.96 * sd(TS_start) / sqrt(n()),
    CI_upper_TS_start = Mean_TS_start + 1.96 * sd(TS_start) / sqrt(n()),
    Mean_TC_start = mean(TC_start),
    CI_lower_TC_start = Mean_TC_start - 1.96 * sd(TC_start) / sqrt(n()),
    CI_upper_TC_start = Mean_TC_start + 1.96 * sd(TC_start) / sqrt(n()),
  )


## Step 9: VIsualize Comfort, sensation and RPE----

complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = RPE_end), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_RPE_end), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_RPE_end, ymax = CI_upper_RPE_end), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp RPE AFTER 30m cycling",
       x = "HA-session",
       y = "RPE")

complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = RPE_start), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_RPE_start), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_RPE_start, ymax = CI_upper_RPE_start), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp RPE BEFORE 30m cycling",
       x = "HA-session",
       y = "RPE")



complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = TS_end), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_TS_end), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_TS_end, ymax = CI_upper_TS_end), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp TS AFTER 30m cycling",
       x = "HA-session",
       y = "TS")

complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = TS_start), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_TS_start), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_TS_start, ymax = CI_upper_TS_start), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp TS BEFORE 30m cycling",
       x = "HA-session",
       y = "TS")

complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = TC_end), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_TC_end), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_TC_end, ymax = CI_upper_TC_end), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp TC AFTER 30m cycling",
       x = "HA-session",
       y = "TC")

complete_pp_feelings %>%
  ggplot(aes(x = ha)) +
  geom_jitter(aes(y = TC_start), alpha = 0.3, size = 5.0, width = 0.2) +
  geom_point(data = complete_pp_Mean_feeling, aes(y = Mean_TC_start), size = 10, shape = 4, stroke = 2) +
  geom_errorbar(data = complete_pp_Mean_feeling, aes(ymin = CI_lower_TC_start, ymax = CI_upper_TC_start), width = 0.2, size = 1) +
  publication_theme() +
  labs(title = "complete pp TC BEFORE 30m cycling",
       x = "HA-session",
       y = "TC")
