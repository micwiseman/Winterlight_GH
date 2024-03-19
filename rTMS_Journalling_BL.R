library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)
library(stringr)
library(readxl)
library(rlang)
library(dplyr)
library(broom)

setwd("Lab/Winterlight")

WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2023_11_03.csv")
WL <- WL[grep("^(TMS|MDD)", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMSremote_2023_11_03.csv")
WL2 <- WL2[!(WL2$participant_external_id == "TMS039" & WL2$session_label %in% c("V2", "V3", "V4")), ]
WL2 <- WL2[!(WL2$participant_external_id == "TMS039b" & WL2$session_label == "V1"), ]
WL2$participant_external_id[WL2$session_label %in% c("V2", "V3") & WL2$participant_external_id == "TMS039b"] <- "TMS039"
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(
  ifelse(
    grepl("(^CTC|C_CTC|CTB)", WL$participant_external_id), "Control",
    ifelse(grepl("^(TMS|MDD)", WL$participant_external_id), "MDD", NA)
  )
)
WL <- WL[!WL$participant_external_id == "CTC036",]

## Exclude controls with sig qids
#WL <- WL[!(WL$participant_external_id == c("CTC006", "CTC039", "CTC042", "CTC045")), ]


# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", 
                         "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043", 
                         "TMS044", "TMS045", "TMS046", "TMS048", "TMS049", "TMS050", "TMS051")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person")


# Replace values in session_label column
WL$session_label <- gsub("Baseline", "V1", WL$session_label)
WL$session_label <- gsub("Week 2", "V2", WL$session_label)
WL$session_label <- gsub("Week 6", "V3", WL$session_label)


#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet=1 )
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet=2)
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL <- merge(WL, demoALL, by = "participant_external_id", all.x=TRUE)

#merge HAMD results 
psych <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
# Merge psych dataset to WL dataset
WL <- merge(WL, psych, by = "participant_external_id", all = TRUE)

WL_Jou <- WL %>%
  filter(task_name == "journaling")
WL_Jou_BL <- WL_Jou %>%
  filter(session_label=="V1")


# # Separate by subtask
WL_feeling_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_journal_feeling.mp3")
MDD_feeling_BL <- WL_feeling_BL %>% filter(participant_group == "MDD")
Ctrl_feeling_BL <- WL_feeling_BL %>% filter(participant_group == "Control")
WL_yesterday_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_yesterday.mp3")
MDD_yesterday_BL <- WL_yesterday_BL %>% filter(participant_group == "MDD")
Ctrl_yesterday_BL <- WL_feeling_BL %>% filter(participant_group == "Control")


###### Histograms #############


speech_vars <- c("fundamental_frequency_mean", "fundamental_frequency_range", "fundamental_frequency_variance", "jitter_local", "long_pause_count_normalized","long_pause_duration", "mean_pause_duration", "medium_pause_count_normalized", "medium_pause_duration", "sentiment_arousal", "sentiment_dominance", "sentiment_valence","shimmer_local","short_pause_count_normalized", "short_pause_duration", "speech_rate", "total_duration_speech", "local_coherence_fastText_300_avg_dist","local_coherence_fastText_300_max_dist", "local_coherence_fastText_300_min_dist", "MATTR_10", "MATTR_30", "propositional_density_ratio_propositions", "constituency_avg_depth", "MLU", "sentiment_dominance", "shimmer_local","intensity_variance", "filled_pauses", "unfilled_pauses", "NP_.._PRP", "tag_PRP", "brunet","max_utt_len","TTR", "intensity_mean_db")


psych_vars <- c("Baseline_HAMD", "Baseline_HAMD_Anx", "Baseline_HAMD_Dep", "Baseline_HAMD_Insom", "Baseline_HAMD_Soma")


Ctrl_Jou_BL <- WL_Jou_BL %>% filter(participant_group =="Control")
MDD_Jou_BL <- WL_Jou_BL %>% filter(participant_group =="MDD")

## speech vars 
par(mfrow = c(1, 2))

for (var in speech_vars) {
  # Create histograms for 'MDD' group
  hist_data_MDD <- MDD_feeling_BL[[var]]
  hist(MDD_feeling_BL[[var]], main = paste("MDD -", var), xlab = var, col = "blue")
  
  # Perform normality test for 'MDD' group
  shapiro_test_MDD <- shapiro.test(hist_data_MDD)
  cat("Shapiro-Wilk Test for MDD -", var, "p-value:", shapiro_test_MDD$p.value, "\n")
  
  # Create histograms for 'Control' group
  hist_data_Control <- Ctrl_feeling_BL[[var]]
  hist(Ctrl_feeling_BL[[var]], main = paste("Control -", var), xlab = var, col = "green")
  
  # Perform normality test for 'Control' group
  shapiro_test_Control <- shapiro.test(hist_data_Control)
  cat("Shapiro-Wilk Test for Control -", var, "p-value:", shapiro_test_Control$p.value, "\n")
}

par(mfrow = c(1, 1))

## pysch vars 
for (var in psych_vars) {
  # Create histograms for 'MDD' group
  hist_data_MDD <- MDD_feeling_BL[[var]]
  hist(MDD_feeling_BL[[var]], main = paste("MDD -", var), xlab = var, col = "blue")
  
  # Perform normality test for 'MDD' group
  shapiro_test_MDD <- shapiro.test(hist_data_MDD)
  cat("Shapiro-Wilk Test for MDD -", var, "p-value:", shapiro_test_MDD$p.value, "\n")
}


### Looks at the difference in speech variables between the journaling stimulus files
# Summarizing across MDD and Control

# Create a list to store the significant variables
significant_vars <- c()

# Loop over each speech measurement of interest
for (var in speech_vars) {
  # Create the plot for the current speech measurement
  p <- ggplot(WL_Jou_BL, aes(x = stimulus_filename, y = !!sym(var))) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    stat_summary(fun = mean, geom = "point", size = 4) +
    labs(title = paste0("Comparison of ", var, " for different journalling tasks\n",
                        "T-Test Result: p = ",
                        formatC(t.test(WL_Jou_BL[[var]] ~ WL_Jou_BL$stimulus_filename)$p.value, digits = 3)),
         x = "Journalling task",
         y = "Speech measurement") +
    facet_wrap(~stimulus_filename, scales = "free_x")
  
  # Print the plot
  print(p)
  
  #Conduct a t test and check for significance
  result <- t.test(WL_Jou_BL[[var]] ~ WL_Jou_BL$stimulus_filename)
  if (result$p.value < 0.05) {
     significant_vars <- c(significant_vars, var)
   }
}

# Print the list of significant variables
if (length(significant_vars) > 0) {
  cat("The following variables are significant:\n")
  cat(paste(significant_vars, collapse = "\n"))
} else {
  cat("No variables are significant at the 0.05 level.\n")
}


### Check remote testing 
WL_feeling_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_journal_feeling.mp3")
# Create a list to store the significant variables
significant_vars <- c()
# Loop over each speech measurement of interest
for (var in speech_vars) {
  # Create the plot for the current speech measurement
  p <- ggplot(WL_Jou_BL, aes(x = testing_location, y = !!sym(var))) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    stat_summary(fun = mean, geom = "point", size = 4) +
    labs(title = paste0("Comparison of ", var, " for remote vs in-person\n",
                        "T-Test Result: p = ",
                         formatC(t.test(WL_Jou_BL[[var]] ~ WL_Jou_BL$testing_location)$p.value, digits = 3)),
         x = "Testing Location",
         y = "Speech measurement") +
    facet_wrap(~stimulus_filename, scales = "free_x")
  
  # Print the plot
  print(p)
  
  #Conduct a t test and check for significance
  result <- t.test(WL_Jou_BL[[var]] ~ WL_Jou_BL$testing_location)
  if (result$p.value < 0.05) {
    significant_vars <- c(significant_vars, var)
  }
}

# Print the list of significant variables
if (length(significant_vars) > 0) {
  cat("The following variables are significant:\n")
  cat(paste(significant_vars, collapse = "\n"))
} else {
  cat("No variables are significant at the 0.05 level.\n")
}


# Check english testing
WL_feeling_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_journal_feeling.mp3")
# Create a list to store the significant variables
significant_vars <- c()
# Loop over each speech measurement of interest
for (var in speech_vars) {
  # Conduct a t-test and check for significance
  result <- t.test(WL_Jou_BL[[var]], WL_Jou_BL$first_language_english, na.rm = TRUE)
  if (result$p.value < 0.05) {
    significant_vars <- c(significant_vars, var)
  }
}
# Print the list of significant variables
if (length(significant_vars) > 0) {
  cat("The following variables are significant:\n")
  cat(paste(significant_vars, collapse = "\n"))
} else {
  cat("No variables are significant at the 0.05 level.\n")
}



##### How are you feeling?
### Baseline Ancovas comparing MDD vs control


# ANCOVA
library(effectsize)
# Create an empty data frame to store the results
ancova_results_df <- data.frame(
  Variable = character(),
  Mean = numeric(),
  SD = numeric(),
  F_Value = numeric(),
  P_Value = numeric(),
  Eta_Squared = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable in speech_vars
for (var in speech_vars) {
  # Get summary statistics
  summary_stats <- WL_feeling_BL %>%
    group_by(participant_group) %>%
    get_summary_stats(.data[[var]], type = "common")
  
  mean_var <- summary_stats$mean
  sd_var <- summary_stats$sd
  
  # Fit the linear model
  model <- lm(paste(var, "~ age_screening + sex + age_learned_english + factor(participant_group)"), data = WL_feeling_BL)
  
  # Perform ANOVA
  ancova <- Anova(model, type = "III")
  f_value <- ancova$`F value`[5] # Assuming the first value is the one of interest
  p_value <- ancova$`Pr(>F)`[5] # Adjust index if needed
  eta_sq <-  ancova$`Sum Sq`[5]/sum(ancova$`Sum Sq`[2], ancova$`Sum Sq`[3], ancova$`Sum Sq`[4], ancova$`Sum Sq`[5])# Accessing the first row and the Eta2 column
  
  # Add a new row to the data frame
  ancova_results_df <- rbind(results_df, data.frame(
    Variable = var,
    Mean = mean_var,
    SD = sd_var,
    F_Value = f_value,
    P_Value = p_value,
    Eta_Squared = eta_sq
  ))
  
  # Create and display the plot
  bxp <- ggplot(WL_feeling_BL, aes(x = factor(participant_group, levels = c("Control", "MDD", "NA")), y = .data[[var]], fill = participant_group)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = var, x = "", y = "Score", subtitle = paste0("p = ", round(p_value, 3))) +
    theme_classic() +
    theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 20), title = element_text(size = 15)) +
    scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
    theme(legend.position = "none") +
    geom_signif(comparisons = list(c("Control", "MDD")), map_signif_level = TRUE)
  
  print(bxp)
}

# Print the results data frame
print(ancova_results_df)



### check linearity assumption for all variables: 

par(mfrow = c(2, 2))  # This will create a 2x2 grid of plots 
data <- WL_feeling_BL
for (var in speech_vars) {
  # Fit the ANCOVA model
  model <- lm(paste(var, "~ age_screening + sex + testing_location + first_language_english + factor(participant_group)"), data = data)
   # Create a plot for the model
  plot(model, main = var)
}

par(mfrow = c(2, 2))  # This will create a 2x2 grid of plots 
data <- WL_Jou_BL
for (var in speech_vars) {
  # Fit the ANCOVA model
  model <- lm(paste(var, "~ age_screening + sex + stimulus_filename+ testing_location + factor(participant_group)"), data = data)
  # Create a plot for the model
  plot(model, main = var)
}


### w/o testing location as covariate 
par(mfrow = c(2, 2))  # This will create a 2x2 grid of plots 
data <- WL_feeling_BL
included_speech_vars <- speech_vars[!speech_vars %in% c("filled_pauses", "shimmer_local", "fundamental_frequency_range")]
for (var in included_speech_vars) {
  # Fit the ANCOVA model without 'testing_location' as a covariate
  formula <- as.formula(paste(var, "~ age_screening + sex + factor(participant_group)"))
  model <- lm(formula, data = data)
  
  # Create a plot for the model
  plot(model, main = var)
}

# Create an empty data frame to store the results
totalhamd_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_total_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'hamd17_total_pre'
  beta_estimate <- coefs["hamd17_total_pre", "Estimate"]
  se <- coefs["hamd17_total_pre", "Std. Error"]
  t_stat <- coefs["hamd17_total_pre", "t value"]
  p_value <- coefs["hamd17_total_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  totalhamd_results_df <- rbind(totalhamd_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))

  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_total_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Total", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(totalhamd_results_df)


# Create an empty data frame to store the results
depresshamd_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_depression_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'hamd17_depression_pre'
  beta_estimate <- coefs["hamd17_depression_pre", "Estimate"]
  se <- coefs["hamd17_depression_pre", "Std. Error"]
  t_stat <- coefs["hamd17_depression_pre", "t value"]
  p_value <- coefs["hamd17_depression_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  depresshamd_results_df <- rbind(depresshamd_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_depression_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Depression", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(depresshamd_results_df)


# The summaries are stored in model_summaries
# You can access each summary by model_summaries[["Var1"]], model_summaries[["Var2"]], etc.



# Initialize a list to store model summaries
model_summaries <- list()

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_anxiety_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Store the summary of the model
  model_summaries[[var]] <- summary(lm_model)
  
  # Print the summary
  print(model_summaries[[var]])
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_anxiety_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Anxiety", y = "Score", title =  var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}





# Create an empty data frame to store the results
somahamd_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_somatic_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'hamd17_somatic_pre'
  beta_estimate <- coefs["hamd17_somatic_pre", "Estimate"]
  se <- coefs["hamd17_somatic_pre", "Std. Error"]
  t_stat <- coefs["hamd17_somatic_pre", "t value"]
  p_value <- coefs["hamd17_somatic_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  somahamd_results_df <- rbind(somahamd_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_somatic_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Somatic", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(somahamd_results_df)

# Create an empty data frame to store the results
insomhamd_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_insomnia_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'hamd17_insomnia_pre'
  beta_estimate <- coefs["hamd17_insomnia_pre", "Estimate"]
  se <- coefs["hamd17_insomnia_pre", "Std. Error"]
  t_stat <- coefs["hamd17_insomnia_pre", "t value"]
  p_value <- coefs["hamd17_insomnia_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  insomhamd_results_df <- rbind(insomhamd_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_insomnia_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Insomnia", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(insomhamd_results_df)

# Create an empty data frame to store the results
suicidehamd_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ hamd17_suicide_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'hamd17_suicide_pre'
  beta_estimate <- coefs["hamd17_suicide_pre", "Estimate"]
  se <- coefs["hamd17_suicide_pre", "Std. Error"]
  t_stat <- coefs["hamd17_suicide_pre", "t value"]
  p_value <- coefs["hamd17_suicide_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  suicidehamd_results_df <- rbind(suicidehamd_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "hamd17_suicide_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "HAM-D Suicide", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(suicidehamd_results_df)

# Create an empty data frame to store the results
qids_results_df <- data.frame(
  Variable = character(),
  Beta_Estimate = numeric(),
  SE = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each variable
for (var in speech_vars) {
  # Run linear regression
  formula <- as.formula(paste(var, "~ qids_total_pre + sex + age_screening + age_learned_english"))
  lm_model <- lm(formula, data = MDD_feeling_BL)
  
  # Extract coefficients
  coefs <- summary(lm_model)$coefficients
  
  # Assuming the predictor of interest is 'qids_total_pre'
  beta_estimate <- coefs["qids_total_pre", "Estimate"]
  se <- coefs["qids_total_pre", "Std. Error"]
  t_stat <- coefs["qids_total_pre", "t value"]
  p_value <- coefs["qids_total_pre", "Pr(>|t|)"]
  
  # Add a new row to the data frame
  qids_results_df <- rbind(qids_results_df, data.frame(
    Variable = var,
    Beta_Estimate = beta_estimate,
    SE = se,
    T_Statistic = t_stat,
    P_Value = p_value
  ))
  
  # Create plot
  scatter_plot <- ggplot(MDD_feeling_BL, aes_string(y = var, x = "qids_total_pre")) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    labs(x = "QIDS Total", y = "Score", title = var) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      title = element_text(size = 20),
      plot.subtitle = element_text(size = 16)
    )
  
  # Add beta and p to plot
  scatter_plot <- scatter_plot +
    labs(subtitle = paste0("\U03B2= ", round(summary(lm_model)$coefficients[2,1], 3),
                           ", p = ", signif(summary(lm_model)$coefficients[2, 4], 3)))
  
  # Display or save plot
  print(scatter_plot)
}

print(qids_results_df)



# Check assumptions
shapiro.test(resid(aov(NOUN_sentiment_arousal ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(NOUN_sentiment_arousal ~ participant_group, data = WL_feeling_BL)
Anova(aov(NOUN_sentiment_arousal ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_arousal ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_arousal ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(NOUN_sentiment_arousal, type="common")
model1<-lm(NOUN_sentiment_arousal ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = NOUN_sentiment_arousal, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Noun Sentiment Arousal by Group", x ="", y = "Arousal score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "NOUN_sentiment_arousal", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
n.sent.arou.lm <- lm(NOUN_sentiment_arousal ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
n.sent.arou.scatter <- ggplot(MDD_feeling_BL, aes(y = NOUN_sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal score", title = "Noun Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add beta to and p to plot
n.sent.arou.scatter <- n.sent.arou.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(n.sent.arou.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(n.sent.arou.lm)$coefficients[2, 4], 2)))
n.sent.arou.scatter


# NOUN_sentiment_dominance 
t.test(NOUN_sentiment_dominance ~ participant_group, data = WL_feeling_BL)
jou_nounsentdom_bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "NOUN_sentiment_dominance", 
                                   title = "Noun Sentiment Dominance by Group", 
                                   ylab = "Dominance score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_nounsentdom_bxp

jou_nounsentdom_stat.test <-  WL_feeling_BL %>% 
  t_test(NOUN_sentiment_dominance ~ participant_group) %>%
  add_significance(); jou_nounsentdom_stat.test
jou_nounsentdom_stat.test <- jou_nounsentdom_stat.test %>% add_xy_position(x = "participant_group")
jou_nounsentdom_bxp  + 
  stat_pvalue_manual(jou_nounsentdom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_nounsentdom_stat.test, description = NULL, detailed = FALSE))

# ANCOVA
shapiro.test(resid(aov(NOUN_sentiment_dominance ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(NOUN_sentiment_dominance ~ participant_group, data = WL_feeling_BL)
Anova(aov(NOUN_sentiment_dominance ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_dominance ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_dominance ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(NOUN_sentiment_dominance, type="common")
model1<-lm(NOUN_sentiment_dominance ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = NOUN_sentiment_dominance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Noun Sentiment Dominance by Group", x ="", y = "Dominance score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "NOUN_sentiment_dominance", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
n.sent.dom.lm <- lm(NOUN_sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
n.sent.dom.scatter <- ggplot(MDD_feeling_BL, aes(y = NOUN_sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance score", title = "Noun Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
n.sent.dom.scatter <- n.sent.dom.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(n.sent.dom.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(n.sent.dom.lm)$coefficients[2, 4], 2)))
n.sent.dom.scatter

# NOUN_sentiment_valence 
t.test(NOUN_sentiment_valence ~ Baseline_HAMD, data = MDD_feeling_BL)
n.sent.val.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "NOUN_sentiment_valence", 
                            title = "Noun Sentiment Valence by Group", 
                            ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); n.sent.val.bxp

n.sent.val_stat.test <-  WL_feeling_BL %>% 
  t_test(NOUN_sentiment_valence ~ participant_group) %>%
  add_significance(); n.sent.val_stat.test
n.sent.val_stat.test <- n.sent.val_stat.test %>% add_xy_position(x = "participant_group")
n.sent.val.bxp  + 
  stat_pvalue_manual(n.sent.val_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(n.sent.val_stat.test, description = NULL, detailed = FALSE))

# ANCOVA 
shapiro.test(resid(aov(NOUN_sentiment_valence ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(NOUN_sentiment_valence ~ participant_group, data = WL_feeling_BL)
Anova(aov(NOUN_sentiment_valence ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_valence ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(NOUN_sentiment_valence ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(NOUN_sentiment_valence, type="common")
model1<-lm(NOUN_sentiment_valence ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = NOUN_sentiment_valence, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Noun Sentiment Valence by Group", x ="", y = "Valence score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "NOUN_sentiment_valence", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
n.sent.val.lm <- lm(NOUN_sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
n.sent.val.scatter <- ggplot(MDD_feeling_BL, aes(y = NOUN_sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Noun Sentiment Valence") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
n.sent.val.scatter <- n.sent.val.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(n.sent.val.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(n.sent.val.lm)$coefficients[2, 4], 2)))
n.sent.val.scatter

# VERB_sentiment_arousal 
t.test(VERB_sentiment_arousal ~ participant_group, data = WL_feeling_BL)
v.sent.arou.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "VERB_sentiment_arousal", 
                             title = "Verb Sentiment Arousal by Group", 
                             ylab = "Arousal Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.arou.bxp

v.sent.arou_stat.test <-  WL_feeling_BL %>% 
  t_test(VERB_sentiment_arousal ~ participant_group) %>%
  add_significance(); v.sent.arou_stat.test
v.sent.arou_stat.test <- v.sent.arou_stat.test %>% add_xy_position(x = "participant_group")
v.sent.arou.bxp  + 
  stat_pvalue_manual(v.sent.arou_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.arou_stat.test, description = NULL, detailed = FALSE))

# ANCOVA

## Remove remote testing 
WL_feeling_BLinperson <- WL_feeling_BL %>%
  filter(testing_location == "in-person")
shapiro.test(resid(aov(VERB_sentiment_arousal ~ participant_group + age_screening+sex, data=WL_feeling_BLinperson)))
bartlett.test(VERB_sentiment_arousal ~ participant_group, data = WL_feeling_BLinperson)
Anova(aov(VERB_sentiment_arousal ~ participant_group * age_screening, data = WL_feeling_BLinperson), type = 2)
Anova(aov(VERB_sentiment_arousal ~ participant_group * sex, data = WL_feeling_BLinperson), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BLinperson %>% group_by(participant_group) %>%  get_summary_stats(VERB_sentiment_arousal, type="common")
model1<-lm(VERB_sentiment_arousal ~ age_screening + sex + factor(participant_group), data = WL_feeling_BLinperson)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[4]

bxp <- ggplot(WL_feeling_BLinperson, aes(x = participant_group, y = VERB_sentiment_arousal, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "VERB Sentiment Arousal by Group", x ="", y = "Arousal score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "VERB_sentiment_arousal", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
v.sent.arou.lm <- lm(VERB_sentiment_arousal ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
v.sent.arou.scatter <- ggplot(MDD_feeling_BL, aes(y = VERB_sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal Score", title = "Verb Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.arou.scatter <- v.sent.arou.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(v.sent.arou.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(v.sent.arou.lm)$coefficients[2, 4], 2)))
v.sent.arou.scatter

# VERB_sentiment_dominance 
t.test(VERB_sentiment_dominance ~ participant_group, data = WL_feeling_BL)
v.sent.dom.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "VERB_sentiment_dominance", 
                            title = "Verb Sentiment Dominance by Group", 
                            ylab = "Dominance Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.dom.bxp

v.sent.dom_stat.test <-  WL_feeling_BL %>% 
  t_test(VERB_sentiment_dominance ~ participant_group) %>%
  add_significance(); v.sent.dom_stat.test
v.sent.dom_stat.test <- v.sent.dom_stat.test %>% add_xy_position(x = "participant_group")
v.sent.dom.bxp  + 
  stat_pvalue_manual(v.sent.dom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.dom_stat.test, description = NULL, detailed = FALSE))


#ANCOVA 
shapiro.test(resid(aov(VERB_sentiment_dominance ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(VERB_sentiment_dominance ~ participant_group, data = WL_feeling_BL)
Anova(aov(VERB_sentiment_dominance ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(VERB_sentiment_dominance ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(VERB_sentiment_dominance ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(VERB_sentiment_dominance, type="common")
model1<-lm(VERB_sentiment_dominance ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = VERB_sentiment_dominance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "VERB Sentiment Dominance by Group", x ="", y = "Dominance score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "VERB_sentiment_dominance", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
v.sent.dom.lm <- lm(VERB_sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
v.sent.dom.scatter <- ggplot(MDD_feeling_BL, aes(y = VERB_sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance Score", title = "Verb Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.dom.scatter <- v.sent.dom.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(v.sent.dom.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(v.sent.dom.lm)$coefficients[2, 4], 2)))
v.sent.dom.scatter

# VERB_sentiment_valence 
t.test(VERB_sentiment_valence ~ participant_group, data = WL_feeling_BL)
v.sent.val.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "VERB_sentiment_dominance", 
                            title = "Verb Sentiment Valence by Group", 
                            ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.val.bxp

v.sent.val_stat.test <-  WL_feeling_BL %>% 
  t_test(VERB_sentiment_valence ~ participant_group) %>%
  add_significance(); v.sent.val_stat.test
v.sent.val_stat.test <- v.sent.val_stat.test %>% add_xy_position(x = "participant_group")
v.sent.val.bxp  + 
  stat_pvalue_manual(v.sent.val_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.val_stat.test, description = NULL, detailed = FALSE))


#ANCOVA


WL_feeling_BLinperson <- WL_feeling_BL %>%
  filter(testing_location == "in-person")
WL_feeling_BLinperson <- WL_feeling_BLinperson %>% mutate(log_data = log(VERB_sentiment_valence))

shapiro.test(resid(aov(log_data ~ participant_group + age_screening+sex, data=WL_feeling_BLinperson)))
bartlett.test(log_data ~ participant_group, data = WL_feeling_BLinperson)
Anova(aov(log_data ~ participant_group * age_screening, data = WL_feeling_BLinperson), type = 2)
Anova(aov(log_data ~ participant_group * sex, data = WL_feeling_BLinperson), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(log_data, type="common")
model1<-lm(log_data ~ age_screening + sex + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[4]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = log_data, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "VERB Sentiment Arousal by Group", x ="", y = "Arousal score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp


# Run linear regression
v.sent.val.lm <- lm(VERB_sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
v.sent.val.scatter <- ggplot(MDD_feeling_BL, aes(y = VERB_sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Verb Sentiment Valencce") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.val.scatter <- v.sent.val.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(v.sent.val.lm)$r.squared, 2),
                         ", p = ", signif(summary(v.sent.val.lm)$coefficients[2, 4], 2)))
v.sent.val.scatter

# fundamental_frequency_mean 
t.test(fundamental_frequency_mean ~ participant_group, data = WL_feeling_BL)
m.f0.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "fundamental_frequency_mean", 
                      title = "Mean F0 by Group", 
                      ylab = "Mean F0", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); m.f0.bxp

m.f0_stat.test <-  WL_feeling_BL %>% 
  t_test(fundamental_frequency_mean ~ participant_group) %>%
  add_significance(); m.f0_stat.test
m.f0_stat.test <- m.f0_stat.test %>% add_xy_position(x = "participant_group")
m.f0.bxp  + 
  stat_pvalue_manual(m.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(m.f0_stat.test, description = NULL, detailed = FALSE))


##ANCOVA

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(fundamental_frequency_mean, type="common")
model1<-lm(fundamental_frequency_mean ~ age_screening + sex + testing_location+ age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = fundamental_frequency_mean, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Mean fundamental frequency", x ="", y = "Mean F0 (Hz)",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "fundamental_frequency_mean", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
m.f0.lm <- lm(fundamental_frequency_mean ~ Baseline_HAMD + sex + age_screening + testing_location +age_learned_english, data = MDD_feeling_BL)
# Create plot
m.f0.scatter <- ggplot(MDD_feeling_BL, aes(y =fundamental_frequency_mean, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "F0 (Hz)", title = "Mean fundamental frequency") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
m.f0.scatter <- m.f0.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(m.f0.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(m.f0.lm)$coefficients[2, 4], 2)))
m.f0.scatter


# fundamental_frequency_range 
t.test(fundamental_frequency_range ~ participant_group, data = WL_feeling_BL)
r.f0.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "fundamental_frequency_range", 
                      title = "F0 Range by Group", 
                      ylab = "F0 Range", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); r.f0.bxp

r.f0_stat.test <-  WL_feeling_BL %>% 
  t_test(fundamental_frequency_range ~ participant_group) %>%
  add_significance(); r.f0_stat.test
r.f0_stat.test <- r.f0_stat.test %>% add_xy_position(x = "participant_group")
r.f0.bxp  + 
  stat_pvalue_manual(r.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(r.f0_stat.test, description = NULL, detailed = FALSE))


# Ancova
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(fundamental_frequency_range, type="common")
model1<-lm(fundamental_frequency_range ~ age_screening + sex + testing_location+ age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = fundamental_frequency_range, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Fundamental frequency range", x ="", y = "F0 range (Hz)",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

# Run linear regression
r.f0.lm <- lm(fundamental_frequency_range ~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english, data = MDD_feeling_BL)
# Create plot
r.f0.scatter <- ggplot(MDD_feeling_BL, aes(y = fundamental_frequency_range, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "F0 Range", title = "Fundamental Frequency Range") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
r.f0.scatter <- r.f0.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(r.f0.lm)$r.squared, 2),
                         ", p = ", signif(summary(r.f0.lm)$coefficients[2, 4], 2)))
r.f0.scatter

# fundamental_frequency_variance
t.test(fundamental_frequency_variance ~ participant_group, data = WL_feeling_BL)
v.f0.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "fundamental_frequency_variance", 
                      title = "F0 Variance by Group", 
                      ylab = "F0 Variance", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.f0.bxp

v.f0_stat.test <-  WL_feeling_BL %>% 
  t_test(fundamental_frequency_variance ~ participant_group) %>%
  add_significance(); v.f0_stat.test
v.f0_stat.test <- v.f0_stat.test %>% add_xy_position(x = "participant_group")
v.f0.bxp  + 
  stat_pvalue_manual(v.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.f0_stat.test, description = NULL, detailed = FALSE))

#Ancova 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(fundamental_frequency_variance, type="common")
model1<-lm(fundamental_frequency_variance ~ age_screening + sex + testing_location+ age_learned_english + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = fundamental_frequency_variance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Fundamental frequency variance", x ="", y = "F0 variance (Hz)",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

# Run linear regression
v.f0.lm <- lm(fundamental_frequency_variance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
v.f0.scatter <- ggplot(MDD_feeling_BL, aes(y = fundamental_frequency_variance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "F0 Variance", title = "Fundamental Frequency Variance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.f0.scatter <- v.f0.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(v.f0.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(v.f0.lm)$coefficients[2, 4], 2)))
v.f0.scatter


# long_pause_count_normalized
t.test(long_pause_count_normalized ~ participant_group, data = WL_feeling_BL)
longpause.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "long_pause_count_normalized", 
                           title = "Long Pause Count by Group", 
                           ylab = "Long Pause Count", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); longpause.bxp

longpause_stat.test <-  WL_feeling_BL %>% 
  t_test(long_pause_count_normalized ~ participant_group) %>%
  add_significance(); longpause_stat.test
longpause_stat.test <- longpause_stat.test %>% add_xy_position(x = "participant_group")
longpause.bxp  + 
  stat_pvalue_manual(longpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(longpause_stat.test, description = NULL, detailed = FALSE))


# ANCOVA 


# Run linear regression
longpause.lm <- lm(long_pause_count_normalized ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
longpause.scatter <- ggplot(MDD_feeling_BL, aes(y = long_pause_count_normalized, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Long Pause Count", title = "Long Pause Count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
longpause.scatter <- longpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(longpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(longpause.lm)$coefficients[2, 4], 2)))
longpause.scatter

# medium_pause_duration
t.test(medium_pause_duration ~ participant_group, data = WL_feeling_BL)
medpause.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "medium_pause_duration", 
                          title = "Medium Pause Duration by Group", 
                          ylab = "Medium Pause Duration", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); medpause.bxp

medpause_stat.test <-  WL_feeling_BL %>% 
  t_test(medium_pause_duration ~ participant_group) %>%
  add_significance(); medpause_stat.test
medpause_stat.test <- medpause_stat.test %>% add_xy_position(x = "participant_group")
medpause.bxp  + 
  stat_pvalue_manual(medpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(medpause_stat.test, description = NULL, detailed = FALSE))

# ANCOVA


shapiro.test(resid(aov(medium_pause_duration ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(medium_pause_duration ~ participant_group, data = WL_feeling_BL)
Anova(aov(medium_pause_duration ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(medium_pause_duration ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(medium_pause_duration ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(medium_pause_duration, type="common")
model1<-lm(medium_pause_duration ~ age_screening + sex + testing_location + age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = medium_pause_duration, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Total pause duration", x ="", y = "Pause duration (s)",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "medium_pause_duration", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
m.pause.dur.lm <- lm(medium_pause_duration ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
m.pause.dur.scatter <- ggplot(MDD_feeling_BL, aes(y = medium_pause_duration, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Pause duration (s)", title = "Pause duration") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
m.pause.dur.scatter <- m.pause.dur.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(m.pause.dur.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(m.pause.dur.lm)$coefficients[2, 4], 2)))
m.pause.dur.scatter
summary(m.pause.dur.lm)

# short_pause_duration
t.test(short_pause_duration ~ participant_group, data = WL_feeling_BL)
shortpause.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "short_pause_duration", 
                            title = "Short Pause Duration by Group", 
                            ylab = "Short Pause Duration", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); shortpause.bxp

shortpause_stat.test <-  WL_feeling_BL %>% 
  t_test(short_pause_duration ~ participant_group) %>%
  add_significance(); shortpause_stat.test
shortpause_stat.test <- shortpause_stat.test %>% add_xy_position(x = "participant_group")
shortpause.bxp  + 
  stat_pvalue_manual(shortpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(shortpause_stat.test, description = NULL, detailed = FALSE))


#ANCOVA

shapiro.test(resid(aov(short_pause_duration ~ participant_group + age_screening+sex, data=WL_feeling_BL)))
bartlett.test(short_pause_duration ~ participant_group, data = WL_feeling_BL)
Anova(aov(short_pause_duration ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(short_pause_duration ~ participant_group * sex, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(short_pause_duration, type="common")
model1<-lm(short_pause_duration ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = short_pause_duration, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "<1 second pause duration", x ="", y = "Seconds",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=FALSE, 
              annotations = c("*"))
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

# Run linear regression
shortpause.lm <- lm(short_pause_duration ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shortpause.scatter <- ggplot(MDD_feeling_BL, aes(y = short_pause_duration, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Pause duration (s)", title = "Short pause duration (s)") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shortpause.scatter <- shortpause.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shortpause.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(shortpause.lm)$coefficients[2, 4], 2)))
shortpause.scatter
summary(shortpause.lm)


# pause_word_ratio
t.test(pause_word_ratio ~ participant_group, data = WL_feeling_BL)
pausetoword.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "pause_word_ratio", 
                             title = "Pause to Word Ratio by Group", 
                             ylab = "Pause to Word Ratio", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); pausetoword.bxp

pausetoword_stat.test <-  WL_feeling_BL %>% 
  t_test(pause_word_ratio ~ participant_group) %>%
  add_significance(); pausetoword_stat.test
pausetoword_stat.test <- pausetoword_stat.test %>% add_xy_position(x = "participant_group")
pausetoword.bxp  + 
  stat_pvalue_manual(pausetoword_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(pausetoword_stat.test, description = NULL, detailed = FALSE))

# ANCOVA
shapiro.test(resid(aov(pause_word_ratio ~ participant_group + age_screening+sex, data=WL_feeling_BL)))
bartlett.test(pause_word_ratio ~ participant_group, data = WL_feeling_BL)
Anova(aov(pause_word_ratio ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(pause_word_ratio ~ participant_group * sex, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(pause_word_ratio, type="common")
model1<-lm(pause_word_ratio ~ age_screening + sex + testing_location+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = pause_word_ratio, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Pause to Word Ratio by Group", x ="", y = "Pause to Word Ratio",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp



# Run linear regression
pausetoword.lm <- lm(pause_word_ratio ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
pausetoword.scatter <- ggplot(MDD_feeling_BL, aes(y = pause_word_ratio, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Pause to Word Ratio", title = "Pause to Word Ratio") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
pausetoword.scatter <- pausetoword.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(pausetoword.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(pausetoword.lm)$coefficients[2, 4], 2)))
pausetoword.scatter

# unfilled_pauses
t.test(unfilled_pauses ~ participant_group, data = WL_feeling_BL)
unfilledpause.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "unfilled_pauses", 
                               title = "Unfilled Pauses by Group", 
                               ylab = "Unfilled Pauses", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); unfilledpause.bxp

unfilledpause_stat.test <-  WL_feeling_BL %>% 
  t_test(unfilled_pauses ~ participant_group) %>%
  add_significance(); unfilledpause_stat.test
unfilledpause_stat.test <- unfilledpause_stat.test %>% add_xy_position(x = "participant_group")
unfilledpause.bxp  + 
  stat_pvalue_manual(unfilledpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(unfilledpause_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
unfilledpause.lm <- lm(unfilled_pauses ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
unfilledpause.scatter <- ggplot(MDD_feeling_BL, aes(y = unfilled_pauses, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Unfilled Pauses", title = "Unfilled Pauses") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
unfilledpause.scatter <- unfilledpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(unfilledpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(unfilledpause.lm)$coefficients[2, 4], 2)))
unfilledpause.scatter

# phonation_rate
t.test(phonation_rate ~ participant_group, data = WL_feeling_BL)
phonrate.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "phonation_rate", 
                          title = "Phonation Rate by Group", 
                          ylab = "Phonation Rate", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); phonrate.bxp

phonrate_stat.test <-  WL_feeling_BL %>% 
  t_test(phonation_rate ~ participant_group) %>%
  add_significance(); phonrate_stat.test
phonrate_stat.test <- phonrate_stat.test %>% add_xy_position(x = "participant_group")
phonrate.bxp  + 
  stat_pvalue_manual(phonrate_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(phonrate_stat.test, description = NULL, detailed = FALSE))

shapiro.test(resid(aov(phonation_rate ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(phonation_rate ~ participant_group, data = WL_feeling_BL)
Anova(aov(phonation_rate ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(phonation_rate ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(phonation_rate ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

# ANCOVA
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(phonation_rate, type="common")
model1<-lm(phonation_rate ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = phonation_rate, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Phonation Rate by Group", x ="", y = "Phonation Rate",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp



# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "phonation_rate", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
phorate.lm <- lm(phonation_rate ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
phorate.scatter <- ggplot(MDD_feeling_BL, aes(y = phonation_rate, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Phonation Rate", title = "Phonation Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
phorate.scatter <- phorate.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(phorate.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(phorate.lm)$coefficients[2, 4], 2)))
phorate.scatter

# speech_rate
t.test(speech_rate ~ participant_group, data = WL_feeling_BL)
spchrate.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "speech_rate", 
                          title = "Speech Rate by Group", 
                          ylab = "Speech Rate", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); spchrate.bxp

spch.rate_stat.test <-  WL_feeling_BL %>% 
  t_test(speech_rate ~ participant_group) %>%
  add_significance(); spch.rate_stat.test
spch.rate_stat.test <- spch.rate_stat.test %>% add_xy_position(x = "participant_group")
spchrate.bxp  + 
  stat_pvalue_manual(spch.rate_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(spch.rate_stat.test, description = NULL, detailed = FALSE))

#ANCOVA

WL_feeling_BLnorm <- WL_feeling_BL %>% mutate(speech_rate = sqrt(speech_rate))


shapiro.test(resid(aov(speech_rate ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BLnorm)))
bartlett.test(speech_rate ~ participant_group, data = WL_feeling_BLnorm)
Anova(aov(speech_rate ~ participant_group * age_screening, data = WL_feeling_BLnorm), type = 2)
Anova(aov(speech_rate ~ participant_group * sex, data = WL_feeling_BLnorm), type = 2)
Anova(aov(speech_rate ~ participant_group * testing_location, data = WL_feeling_BLnorm), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(speech_rate, type="common")
model1<-lm(speech_rate ~ age_screening + sex + testing_location + age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = speech_rate, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Speech Rate", x ="", y = "Words per Minute",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "speech_rate", 
                              c("Baseline_HAMD", "sex", "age_screening", 
                                "testing_location", "age_learned_english"))
spchrate.lm <- lm(speech_rate ~ Baseline_HAMD + sex + age_screening + 
                    testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
spchrate.scatter <- ggplot(MDD_feeling_BL, aes(y = speech_rate, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Words per Minute", title = "Speech Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
spchrate.scatter <- spchrate.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(spchrate.lm)$coefficients[2,1], 2),
       ", p = ", signif(summary(spchrate.lm)$coefficients[2, 4], 2)))
spchrate.scatter
summary(spchrate.lm)

# total_speech_duration
t.test(total_duration_speech ~ participant_group, data = WL_feeling_BL)
totdur.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "total_duration_speech", 
                        title = "Speech Duration by Group", 
                        ylab = "Total Speech Duration (s)", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); totdur.bxp

totdur_stat.test <-  WL_feeling_BL %>% 
  t_test(total_duration_speech ~ participant_group) %>%
  add_significance(); totdur_stat.test
totdur_stat.test <- totdur_stat.test %>% add_xy_position(x = "participant_group")
totdur.bxp  + 
  stat_pvalue_manual(totdur_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(totdur_stat.test, description = NULL, detailed = FALSE))

# ANCOVA
shapiro.test(resid(aov(total_duration_speech ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(total_duration_speech ~ participant_group, data = WL_feeling_BL)
Anova(aov(total_duration_speech ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(total_duration_speech ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(total_duration_speech ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(total_duration_speech, type="common")
model1<-lm(total_duration_speech ~ age_screening + sex + testing_location + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = total_duration_speech, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Total Duration by Group", x ="", y = "Speech Duration",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp



# Run linear regression
totdur.lm <- lm(total_duration_speech ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
totdur.scatter <- ggplot(MDD_feeling_BL, aes(y = total_duration_speech, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Total Speech Duration (s)", title = "Speech Duration") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
 totdur.scatter <- totdur.scatter +
   labs(subtitle = paste0("\U03B2= ", round(summary(totdur.lm)$coefficients[2,1], 2),
                          ", p = ", signif(summary(totdur.lm)$coefficients[2, 4], 2)))
 totdur.scatter

# sentiment_arousal
t.test(sentiment_arousal ~ participant_group, data = WL_feeling_BL)
sentarou.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "sentiment_arousal", 
                          title = "Sentiment Arousal by Group", 
                          ylab = "Arousal Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentarou.bxp

sentarou_stat.test <-  WL_feeling_BL %>% 
  t_test(sentiment_arousal ~ participant_group) %>%
  add_significance(); sentarou_stat.test
sentarou_stat.test <- sentarou_stat.test %>% add_xy_position(x = "participant_group")
sentarou.bxp  + 
  stat_pvalue_manual(sentarou_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentarou_stat.test, description = NULL, detailed = FALSE))

# ANCOVA 
shapiro.test(resid(aov(sentiment_arousal ~ participant_group + age_screening+sex+testing_location, data=WL_feeling_BL)))
bartlett.test(sentiment_arousal ~ participant_group, data = WL_feeling_BL)
Anova(aov(sentiment_arousal ~ participant_group * age_screening, data = WL_feeling_BL), type = 2)
Anova(aov(sentiment_arousal ~ participant_group * sex, data = WL_feeling_BL), type = 2)
Anova(aov(sentiment_arousal ~ participant_group * testing_location, data = WL_feeling_BL), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  
  get_summary_stats(sentiment_arousal, type="common")
model1<-lm(sentiment_arousal ~ age_screening + sex + testing_location + 
             age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = sentiment_arousal, 
                                 fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Sentiment Arousal", x ="", y = "Arousal score",
       subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), 
        title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "sentiment_arousal", 
                              c("Baseline_HAMD", "sex", "age_screening", 
                                "testing_location", "age_learned_english"))
sentarou.lm <- lm(sentiment_arousal ~ Baseline_HAMD + sex + age_screening + 
                    testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
sentarou.scatter <- ggplot(MDD_feeling_BL, aes(y = sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal Score", title = "Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add beta coeff to and p to plot
sentarou.scatter <- sentarou.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(sentarou.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(sentarou.lm)$coefficients[2, 4], 2)))
sentarou.scatter
summary(sentarou.lm)

# sentiment_dominance
t.test(sentiment_dominance ~ participant_group, data = WL_feeling_BL)
sentdom.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "sentiment_dominance", 
                         title = "Sentiment Dominance by Group", 
                         ylab = "Dominance Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentdom.bxp

sentdom_stat.test <-  WL_feeling_BL %>% 
  t_test(sentiment_dominance ~ participant_group) %>%
  add_significance(); sentdom_stat.test
sentdom_stat.test <- sentdom_stat.test %>% add_xy_position(x = "participant_group")
sentdom.bxp  + 
  stat_pvalue_manual(sentdom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentdom_stat.test, description = NULL, detailed = FALSE))


#ANVOCA

WL_feeling_BLinperson <- WL_feeling_BL %>%
  filter(testing_location == "in-person")
shapiro.test(resid(aov(sentiment_dominance ~ participant_group + age_screening+sex, data=WL_feeling_BLinperson)))
bartlett.test(sentiment_dominance ~ participant_group, data = WL_feeling_BLinperson)
Anova(aov(sentiment_dominance ~ participant_group * age_screening, data = WL_feeling_BLinperson), type = 2)
Anova(aov(sentiment_dominance ~ participant_group * sex, data = WL_feeling_BLinperson), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(sentiment_dominance, type="common")
model1<-lm(sentiment_dominance ~ age_screening + sex + testing_location + age_learned_english + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BLinperson, aes(x = participant_group, y = sentiment_dominance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Sentiment Dominance by Group", x ="", y = "dominance score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "sentiment_dominance", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
sentdom.lm <- lm(sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
sentdom.scatter <- ggplot(MDD_feeling_BL, aes(y = sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance Score", title = "Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
sentdom.scatter <- sentdom.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(sentdom.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(sentdom.lm)$coefficients[2, 4], 2)))
sentdom.scatter

# sentiment_valence
t.test(sentiment_valence ~ participant_group, data = WL_feeling_BL)
sentval.bxp <- ggboxplot(WL_feeling_BL, x = "participant_group", y = "sentiment_valence", 
                         title = "Sentiment Valence by Group", 
                         ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentval.bxp

sentval_stat.test <-  WL_feeling_BL %>% 
  t_test(sentiment_valence ~ participant_group) %>%
  add_significance(); sentval_stat.test
sentval_stat.test <- sentval_stat.test %>% add_xy_position(x = "participant_group")
sentval.bxp  + 
  stat_pvalue_manual(sentval_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentval_stat.test, description = NULL, detailed = FALSE))


#ANCOVA: 
WL_feeling_BLinperson <- WL_feeling_BL %>%
  filter(testing_location == "in-person")
shapiro.test(resid(aov(sentiment_valence ~ participant_group + age_screening+sex, data=WL_feeling_BLinperson)))
bartlett.test(sentiment_valence ~ participant_group, data = WL_feeling_BLinperson)
Anova(aov(sentiment_valence ~ participant_group * age_screening, data = WL_feeling_BLinperson), type = 2)
Anova(aov(sentiment_valence ~ participant_group * sex, data = WL_feeling_BLinperson), type = 2)

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(sentiment_valence, type="common")
model1<-lm(sentiment_valence ~ age_screening + sex + testing_location + age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = sentiment_valence, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Sentiment valence", x ="", y = "Valence score",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp



# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "sentiment_valence", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
sent.val.lm <- lm(sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
sent.val.scatter <- ggplot(MDD_feeling_BL, aes(y = sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Sentiment Valence") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
sent.val.scatter <- sent.val.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(sent.val.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(sent.val.lm)$coefficients[2, 4], 2)))
sent.val.scatter
summary(sent.val.lm)





















# What did you do yesterday -----------------------------------------------


##### What did you do yesterday?
### Baseline t-tests comparing MDD vs control
# NOUN_sentiment_arousal 
t.test(NOUN_sentiment_arousal ~ participant_group, data = WL_yesterday_BL)
jou_nounsentarous_bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "NOUN_sentiment_arousal", 
                                   title = "Noun Sentiment Arousal by Group", 
                                   ylab = "Arousal score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_nounsentarous_bxp

jou_nounsentarous_stat.test <-  WL_yesterday_BL %>% 
  t_test(NOUN_sentiment_arousal ~ participant_group) %>%
  add_significance(); jou_nounsentarous_stat.test
jou_nounsentarous_stat.test <- jou_nounsentarous_stat.test %>% add_xy_position(x = "participant_group")
jou_nounsentarous_bxp  + 
  stat_pvalue_manual(jou_nounsentarous_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_nounsentarous_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
linear_regression_assumptions(MDD_feeling_BL, "NOUN_sentiment_arousal", c("Baseline_HAMD", "sex", "age_screening", "testing_location"))
n.sent.arou.lm <- lm(NOUN_sentiment_arousal ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
n.sent.arou.scatter <- ggplot(MDD_yesterday_BL, aes(y = NOUN_sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal score", title = "Noun Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
n.sent.arou.scatter <- n.sent.arou.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(n.sent.arou.lm)$r.squared, 2),
                         ", p = ", signif(summary(n.sent.arou.lm)$coefficients[2, 4], 2)))
n.sent.arou.scatter


# NOUN_sentiment_dominance 
t.test(NOUN_sentiment_dominance ~ participant_group, data = WL_yesterday_BL)
jou_nounsentdom_bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "NOUN_sentiment_dominance", 
                                 title = "Noun Sentiment Dominance by Group", 
                                 ylab = "Dominance score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_nounsentdom_bxp

jou_nounsentdom_stat.test <-  WL_yesterday_BL %>% 
  t_test(NOUN_sentiment_dominance ~ participant_group) %>%
  add_significance(); jou_nounsentdom_stat.test
jou_nounsentdom_stat.test <- jou_nounsentdom_stat.test %>% add_xy_position(x = "participant_group")
jou_nounsentdom_bxp  + 
  stat_pvalue_manual(jou_nounsentdom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_nounsentdom_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
n.sent.dom.lm <- lm(NOUN_sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
n.sent.dom.scatter <- ggplot(MDD_yesterday_BL, aes(y = NOUN_sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance score", title = "Noun Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
n.sent.dom.scatter <- n.sent.dom.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(n.sent.dom.lm)$r.squared, 2),
                         ", p = ", signif(summary(n.sent.dom.lm)$coefficients[2, 4], 2)))
n.sent.dom.scatter

# NOUN_sentiment_valence 
t.test(NOUN_sentiment_valence ~ participant_group, data = WL_yesterday_BL)
n.sent.val.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "NOUN_sentiment_valence", 
                            title = "Noun Sentiment Valence by Group", 
                            ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); n.sent.val.bxp

n.sent.val_stat.test <-  WL_yesterday_BL %>% 
  t_test(NOUN_sentiment_valence ~ participant_group) %>%
  add_significance(); n.sent.val_stat.test
n.sent.val_stat.test <- n.sent.val_stat.test %>% add_xy_position(x = "participant_group")
n.sent.val.bxp  + 
  stat_pvalue_manual(n.sent.val_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(n.sent.val_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
n.sent.val.lm <- lm(NOUN_sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
n.sent.val.scatter <- ggplot(MDD_yesterday_BL, aes(y = NOUN_sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Noun Sentiment Valence") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
n.sent.val.scatter <- n.sent.val.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(n.sent.val.lm)$r.squared, 2),
                         ", p = ", signif(summary(n.sent.val.lm)$coefficients[2, 4], 2)))
n.sent.val.scatter

# VERB_sentiment_arousal 
t.test(VERB_sentiment_arousal ~ participant_group, data = WL_yesterday_BL)
v.sent.arou.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "VERB_sentiment_arousal", 
                             title = "Verb Sentiment Arousal by Group", 
                             ylab = "Arousal Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.arou.bxp

v.sent.arou_stat.test <-  WL_yesterday_BL %>% 
  t_test(VERB_sentiment_arousal ~ participant_group) %>%
  add_significance(); v.sent.arou_stat.test
v.sent.arou_stat.test <- v.sent.arou_stat.test %>% add_xy_position(x = "participant_group")
v.sent.arou.bxp  + 
  stat_pvalue_manual(v.sent.arou_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.arou_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
v.sent.arou.lm <- lm(VERB_sentiment_arousal ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
v.sent.arou.scatter <- ggplot(MDD_yesterday_BL, aes(y = VERB_sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal Score", title = "Verb Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.arou.scatter <- v.sent.arou.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(v.sent.arou.lm)$r.squared, 2),
                         ", p = ", signif(summary(v.sent.arou.lm)$coefficients[2, 4], 2)))
v.sent.arou.scatter

# VERB_sentiment_dominance 
t.test(VERB_sentiment_dominance ~ participant_group, data = WL_yesterday_BL)
v.sent.dom.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "VERB_sentiment_dominance", 
                            title = "Verb Sentiment Dominance by Group", 
                            ylab = "Dominance Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.dom.bxp

v.sent.dom_stat.test <-  WL_yesterday_BL %>% 
  t_test(VERB_sentiment_dominance ~ participant_group) %>%
  add_significance(); v.sent.dom_stat.test
v.sent.dom_stat.test <- v.sent.dom_stat.test %>% add_xy_position(x = "participant_group")
v.sent.dom.bxp  + 
  stat_pvalue_manual(v.sent.dom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.dom_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
v.sent.dom.lm <- lm(VERB_sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
v.sent.dom.scatter <- ggplot(MDD_yesterday_BL, aes(y = VERB_sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance Score", title = "Verb Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.dom.scatter <- v.sent.dom.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(v.sent.dom.lm)$r.squared, 2),
                         ", p = ", signif(summary(v.sent.dom.lm)$coefficients[2, 4], 2)))
v.sent.dom.scatter

# VERB_sentiment_valence 
t.test(VERB_sentiment_valence ~ participant_group, data = WL_yesterday_BL)
v.sent.val.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "VERB_sentiment_dominance", 
                            title = "Verb Sentiment Valence by Group", 
                            ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.sent.val.bxp

v.sent.val_stat.test <-  WL_yesterday_BL %>% 
  t_test(VERB_sentiment_valence ~ participant_group) %>%
  add_significance(); v.sent.val_stat.test
v.sent.val_stat.test <- v.sent.val_stat.test %>% add_xy_position(x = "participant_group")
v.sent.val.bxp  + 
  stat_pvalue_manual(v.sent.val_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.sent.val_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
v.sent.val.lm <- lm(VERB_sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
v.sent.val.scatter <- ggplot(MDD_yesterday_BL, aes(y = VERB_sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Verb Sentiment Valencce") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.sent.val.scatter <- v.sent.val.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(v.sent.val.lm)$r.squared, 2),
                         ", p = ", signif(summary(v.sent.val.lm)$coefficients[2, 4], 2)))
v.sent.val.scatter

# fundamental_frequency_mean 
t.test(fundamental_frequency_mean ~ participant_group, data = WL_yesterday_BL)
m.f0.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "fundamental_frequency_mean", 
                      title = "Mean F0 by Group", 
                      ylab = "Mean F0", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); m.f0.bxp

m.f0_stat.test <-  WL_yesterday_BL %>% 
  t_test(fundamental_frequency_mean ~ participant_group) %>%
  add_significance(); m.f0_stat.test
m.f0_stat.test <- m.f0_stat.test %>% add_xy_position(x = "participant_group")
m.f0.bxp  + 
  stat_pvalue_manual(m.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(m.f0_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
m.fo.lm <- lm(fundamental_frequency_mean ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
m.fo.scatter <- ggplot(MDD_yesterday_BL, aes(y = fundamental_frequency_mean, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Mean F0", title = "Mean Fundamental Frequency") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
m.fo.scatter <- m.fo.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(m.fo.lm)$r.squared, 2),
                         ", p = ", signif(summary(m.fo.lm)$coefficients[2, 4], 2)))
m.fo.scatter

# fundamental_frequency_range 
t.test(fundamental_frequency_range ~ participant_group, data = WL_yesterday_BL)
r.f0.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "fundamental_frequency_range", 
                      title = "F0 Range by Group", 
                      ylab = "F0 Range", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); r.f0.bxp

r.f0_stat.test <-  WL_yesterday_BL %>% 
  t_test(fundamental_frequency_range ~ participant_group) %>%
  add_significance(); r.f0_stat.test
r.f0_stat.test <- r.f0_stat.test %>% add_xy_position(x = "participant_group")
r.f0.bxp  + 
  stat_pvalue_manual(r.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(r.f0_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
r.f0.lm <- lm(fundamental_frequency_range ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
r.f0.scatter <- ggplot(MDD_yesterday_BL, aes(y = fundamental_frequency_range, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "F0 Range", title = "Fundamental Frequency Range") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
r.f0.scatter <- r.f0.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(r.f0.lm)$r.squared, 2),
                         ", p = ", signif(summary(r.f0.lm)$coefficients[2, 4], 2)))
r.f0.scatter

# fundamental_frequency_variance
t.test(fundamental_frequency_variance ~ participant_group, data = WL_yesterday_BL)
v.f0.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "fundamental_frequency_variance", 
                      title = "F0 Variance by Group", 
                      ylab = "F0 Variance", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); v.f0.bxp

v.f0_stat.test <-  WL_yesterday_BL %>% 
  t_test(fundamental_frequency_variance ~ participant_group) %>%
  add_significance(); v.f0_stat.test
v.f0_stat.test <- v.f0_stat.test %>% add_xy_position(x = "participant_group")
v.f0.bxp  + 
  stat_pvalue_manual(v.f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(v.f0_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
v.f0.lm <- lm(fundamental_frequency_variance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
v.f0.scatter <- ggplot(MDD_yesterday_BL, aes(y = fundamental_frequency_variance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "F0 Variance", title = "Fundamental Frequency Variance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
v.f0.scatter <- v.f0.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(v.f0.lm)$r.squared, 2),
                         ", p = ", signif(summary(v.f0.lm)$coefficients[2, 4], 2)))
v.f0.scatter


# long_pause_count_normalized
t.test(long_pause_count_normalized ~ participant_group, data = WL_yesterday_BL)
longpause.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "long_pause_count_normalized", 
                           title = "Long Pause Count by Group", 
                           ylab = "Long Pause Count", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); longpause.bxp

longpause_stat.test <-  WL_yesterday_BL %>% 
  t_test(long_pause_count_normalized ~ participant_group) %>%
  add_significance(); longpause_stat.test
longpause_stat.test <- longpause_stat.test %>% add_xy_position(x = "participant_group")
longpause.bxp  + 
  stat_pvalue_manual(longpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(longpause_stat.test, description = NULL, detailed = FALSE))




# Run linear regression
longpause.lm <- lm(long_pause_count_normalized ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
longpause.scatter <- ggplot(MDD_yesterday_BL, aes(y = long_pause_count_normalized, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Long Pause Count", title = "Long Pause Count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
longpause.scatter <- longpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(longpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(longpause.lm)$coefficients[2, 4], 2)))
longpause.scatter

# medium_pause_duration
t.test(medium_pause_duration ~ participant_group, data = WL_yesterday_BL)
medpause.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "medium_pause_duration", 
                          title = "Medium Pause Duration by Group", 
                          ylab = "Medium Pause Duration", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); medpause.bxp

medpause_stat.test <-  WL_yesterday_BL %>% 
  t_test(medium_pause_duration ~ participant_group) %>%
  add_significance(); medpause_stat.test
medpause_stat.test <- medpause_stat.test %>% add_xy_position(x = "participant_group")
medpause.bxp  + 
  stat_pvalue_manual(medpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(medpause_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
MDD_yesterday_BL1 <- MDD_yesterday_BL[-1,]
medpause.lm <- lm(medium_pause_duration ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL1)
# Create plot
medpause.scatter <- ggplot(MDD_yesterday_BL1, aes(y = medium_pause_duration, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Medium Pause Duration (ms) ", title = "Medium Pause Duration") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
medpause.scatter <- medpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(medpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(medpause.lm)$coefficients[2, 4], 2)))
medpause.scatter

# short_pause_duration
t.test(short_pause_duration ~ participant_group, data = WL_yesterday_BL)
shortpause.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "short_pause_duration", 
                            title = "Short Pause Duration by Group", 
                            ylab = "Short Pause Duration", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); shortpause.bxp

shortpause_stat.test <-  WL_yesterday_BL %>% 
  t_test(short_pause_duration ~ participant_group) %>%
  add_significance(); shortpause_stat.test
shortpause_stat.test <- shortpause_stat.test %>% add_xy_position(x = "participant_group")
shortpause.bxp  + 
  stat_pvalue_manual(shortpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(shortpause_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
shortpause.lm <- lm(short_pause_duration ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
shortpause.scatter <- ggplot(MDD_yesterday_BL, aes(y = short_pause_duration, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Short Pause Duration (ms) ", title = "Short Pause Duration") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shortpause.scatter <- shortpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(shortpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(shortpause.lm)$coefficients[2, 4], 2)))
shortpause.scatter

# pause_word_ratio
t.test(pause_word_ratio ~ participant_group, data = WL_yesterday_BL)
pausetoword.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "pause_word_ratio", 
                             title = "Pause to Word Ratio by Group", 
                             ylab = "Pause to Word Ratio", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); pausetoword.bxp

pausetoword_stat.test <-  WL_yesterday_BL %>% 
  t_test(pause_word_ratio ~ participant_group) %>%
  add_significance(); pausetoword_stat.test
pausetoword_stat.test <- pausetoword_stat.test %>% add_xy_position(x = "participant_group")
pausetoword.bxp  + 
  stat_pvalue_manual(pausetoword_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(pausetoword_stat.test, description = NULL, detailed = FALSE))




# Run linear regression
pausetoword.lm <- lm(pause_word_ratio ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
pausetoword.scatter <- ggplot(MDD_yesterday_BL, aes(y = pause_word_ratio, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Pause to Word Ratio", title = "Pause to Word Ratio") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
pausetoword.scatter <- pausetoword.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(pausetoword.lm)$r.squared, 2),
                         ", p = ", signif(summary(pausetoword.lm)$coefficients[2, 4], 2)))
pausetoword.scatter

# unfilled_pauses
t.test(unfilled_pauses ~ participant_group, data = WL_yesterday_BL)
unfilledpause.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "unfilled_pauses", 
                               title = "Unfilled Pauses by Group", 
                               ylab = "Unfilled Pauses", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); unfilledpause.bxp

unfilledpause_stat.test <-  WL_yesterday_BL %>% 
  t_test(unfilled_pauses ~ participant_group) %>%
  add_significance(); unfilledpause_stat.test
unfilledpause_stat.test <- unfilledpause_stat.test %>% add_xy_position(x = "participant_group")
unfilledpause.bxp  + 
  stat_pvalue_manual(unfilledpause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(unfilledpause_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
unfilledpause.lm <- lm(unfilled_pauses ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
unfilledpause.scatter <- ggplot(MDD_yesterday_BL, aes(y = unfilled_pauses, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Unfilled Pauses", title = "Unfilled Pauses") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
unfilledpause.scatter <- unfilledpause.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(unfilledpause.lm)$r.squared, 2),
                         ", p = ", signif(summary(unfilledpause.lm)$coefficients[2, 4], 2)))
unfilledpause.scatter

# phonation_rate
t.test(phonation_rate ~ participant_group, data = WL_yesterday_BL)
phonrate.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "phonation_rate", 
                          title = "Phonation Rate by Group", 
                          ylab = "Phonation Rate", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); phonrate.bxp

phonrate_stat.test <-  WL_yesterday_BL %>% 
  t_test(phonation_rate ~ participant_group) %>%
  add_significance(); phonrate_stat.test
phonrate_stat.test <- phonrate_stat.test %>% add_xy_position(x = "participant_group")
phonrate.bxp  + 
  stat_pvalue_manual(phonrate_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(phonrate_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
phonrate.lm <- lm(phonation_rate ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
phonrate.scatter <- ggplot(MDD_yesterday_BL, aes(y = phonation_rate, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Phonation Rate", title = "Phonation Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
phonrate.scatter <- phonrate.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(phonrate.lm)$r.squared, 2),
                         ", p = ", signif(summary(phonrate.lm)$coefficients[2, 4], 2)))
phonrate.scatter

# speech_rate
t.test(speech_rate ~ participant_group, data = WL_yesterday_BL)
spchrate.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "speech_rate", 
                          title = "Speech Rate by Group", 
                          ylab = "Speech Rate", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); spchrate.bxp

spch.rate_stat.test <-  WL_yesterday_BL %>% 
  t_test(speech_rate ~ participant_group) %>%
  add_significance(); spch.rate_stat.test
spch.rate_stat.test <- spch.rate_stat.test %>% add_xy_position(x = "participant_group")
spchrate.bxp  + 
  stat_pvalue_manual(spch.rate_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(spch.rate_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
spchrate.lm <- lm(speech_rate ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
spchrate.scatter <- ggplot(MDD_yesterday_BL, aes(y = speech_rate, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Speech Rate", title = "Speech Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
spchrate.scatter <- spchrate.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(spchrate.lm)$r.squared, 2),
                         ", p = ", signif(summary(spchrate.lm)$coefficients[2, 4], 2)))
spchrate.scatter

# total_speech_duration
t.test(total_duration_speech ~ participant_group, data = WL_yesterday_BL)
totdur.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "total_duration_speech", 
                        title = "Speech Duration by Group", 
                        ylab = "Total Speech Duration (s)", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); totdur.bxp

totdur_stat.test <-  WL_yesterday_BL %>% 
  t_test(total_duration_speech ~ participant_group) %>%
  add_significance(); totdur_stat.test
totdur_stat.test <- totdur_stat.test %>% add_xy_position(x = "participant_group")
totdur.bxp  + 
  stat_pvalue_manual(totdur_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(totdur_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
totdur.lm <- lm(total_duration_speech ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
totdur.scatter <- ggplot(MDD_yesterday_BL, aes(y = total_duration_speech, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Total Speech Duration (s)", title = "Speech Duration") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
totdur.scatter <- totdur.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(totdur.lm)$r.squared, 2),
                         ", p = ", signif(summary(totdur.lm)$coefficients[2, 4], 2)))
totdur.scatter

# sentiment_arousal
t.test(sentiment_arousal ~ participant_group, data = WL_yesterday_BL)
sentarou.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "sentiment_arousal", 
                          title = "Sentiment Arousal by Group", 
                          ylab = "Arousal Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentarou.bxp

sentarou_stat.test <-  WL_yesterday_BL %>% 
  t_test(sentiment_arousal ~ participant_group) %>%
  add_significance(); sentarou_stat.test
sentarou_stat.test <- sentarou_stat.test %>% add_xy_position(x = "participant_group")
sentarou.bxp  + 
  stat_pvalue_manual(sentarou_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentarou_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
sentarou.lm <- lm(sentiment_arousal ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
sentarou.scatter <- ggplot(MDD_yesterday_BL, aes(y = sentiment_arousal, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Arousal Score", title = "Sentiment Arousal") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
sentarou.scatter <- sentarou.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(sentarou.lm)$r.squared, 2),
                         ", p = ", signif(summary(sentarou.lm)$coefficients[2, 4], 2)))
sentarou.scatter

# sentiment_dominance
t.test(sentiment_dominance ~ participant_group, data = WL_yesterday_BL)
sentdom.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "sentiment_dominance", 
                         title = "Sentiment Dominance by Group", 
                         ylab = "Dominance Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentdom.bxp

sentdom_stat.test <-  WL_yesterday_BL %>% 
  t_test(sentiment_dominance ~ participant_group) %>%
  add_significance(); sentdom_stat.test
sentdom_stat.test <- sentdom_stat.test %>% add_xy_position(x = "participant_group")
sentdom.bxp  + 
  stat_pvalue_manual(sentdom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentdom_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
sentdom.lm <- lm(sentiment_dominance ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
sentdom.scatter <- ggplot(MDD_yesterday_BL, aes(y = sentiment_dominance, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Dominance Score", title = "Sentiment Dominance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
sentdom.scatter <- sentdom.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(sentdom.lm)$r.squared, 2),
                         ", p = ", signif(summary(sentdom.lm)$coefficients[2, 4], 2)))
sentdom.scatter

# sentiment_valence
t.test(sentiment_valence ~ participant_group, data = WL_yesterday_BL)
sentval.bxp <- ggboxplot(WL_yesterday_BL, x = "participant_group", y = "sentiment_valence", 
                         title = "Sentiment Valence by Group", 
                         ylab = "Valence Score", xlab = "", add = "jitter", fill = "participant_group") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); sentval.bxp

sentval_stat.test <-  WL_yesterday_BL %>% 
  t_test(sentiment_valence ~ participant_group) %>%
  add_significance(); sentval_stat.test
sentval_stat.test <- sentval_stat.test %>% add_xy_position(x = "participant_group")
sentval.bxp  + 
  stat_pvalue_manual(sentval_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sentval_stat.test, description = NULL, detailed = FALSE))

# Run linear regression
sentval.lm <- lm(sentiment_valence ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_yesterday_BL)
# Create plot
sentval.scatter <- ggplot(MDD_yesterday_BL, aes(y = sentiment_valence, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Valence Score", title = "Sentiment Valence") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
sentval.scatter <- sentval.scatter +
  labs(subtitle = paste0("R\u00B2 = ", round(summary(sentval.lm)$r.squared, 2),
                         ", p = ", signif(summary(sentval.lm)$coefficients[2, 4], 2)))
sentval.scatter