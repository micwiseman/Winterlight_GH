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

#setwd("Lab/Winterlight")

WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS_2023FEB16.csv")
WL <- WL[grep("^TMS", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS remote_2023FEB16.csv")
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(ifelse(grepl("^CTC", WL$participant_external_id), "Control",
                                      ifelse(grepl("^TMS", WL$participant_external_id), "MDD", NA)))
WL <- WL[!grepl("^MDD|^OCD|^MFB", WL$participant_external_id), ]


# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person")




# Replace values in session_label column
WL$session_label <- gsub("Baseline", "V1", WL$session_label)
WL$session_label <- gsub("Week 2", "V2", WL$session_label)
WL$session_label <- gsub("Week 6", "V3", WL$session_label)

WL_Jou <- subsetTask(WL, "journaling")
WL_Jou_BL <- extractBaseline(WL_Jou)

#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_Demographics.xlsx")
names(demoMDD)[1] <- "participant_external_id"
WL_Jou_BL <- merge(WL_Jou_BL, demoMDD, by = "participant_external_id", all.x=TRUE)

MDD_Jou_BLq <- WL_Jou_BL %>% filter(participant_group == "MDD")


#merge QIDS and HAMD results to MDD data
psych <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
col_index1<-which(colnames(psych)=="qids_total_pre")
colnames(psych)[col_index1] <-"Baseline_QIDS"
col_index2<-which(colnames(psych) == "hamd17_total_pre")
colnames(psych)[col_index2] <-"Baseline_HAMD"
colnames(psych)[1] <- "participant_external_id"
MDD_Jou_BLq <- merge(MDD_Jou_BLq, psych[,c("participant_external_id", "Baseline_QIDS", "Baseline_HAMD")], by = "participant_external_id", all.x = TRUE)

# Separate by subtask
MDD_feeling_BLq <- subsetStimulus(MDD_Jou_BLq, "en_instruction_journal_feeling.mp3")



# List of outcome variables
outcome_variables <- c("fundamental_frequency_mean", "fundamental_frequency_variance", 
                       "fundamental_frequency_range", "hnr_ac_mean","hnr_ac_variance",
                       "hnr_ac_range","hnr_cc_mean","hnr_cc_variance", "hnr_cc_range", 
                       "jitter_ddp", "jitter_local", "jitter_local_absolute", 
                       "jitter_ppq5","jitter_rap","shimmer_apq11", "shimmer_apq3",
                       "shimmer_apq5","shimmer_dda","shimmer_local", "shimmer_local_db",
                       "medium_pause_duration", "sentiment_arousal","sentiment_valence",
                       "speech_rate", "tag_PRP")

plot_titles <- c("Mean fundamental frequency", "Fundamental frequency variance", 
                "Fundamental frequency range", "Mean HNR", "HNR variance", 
                "HNR range","Mean HNR, CC","HNR variance, CC","HNR range, CC", 
                "Average change in jitter","Local jitter","Local jitter, absolute", 
                "Relative distal jitter","Relative proximal jitter","Relative distal shimmer", 
                "Relative proximal shimmer","Relative shimmer","Average change in shimmer",
                "Local shimmer","Local shimmer, dB","Pause duration","Sentiment arousal", 
                "Sentiment valence","Speech rate", "Personal pronoun count")


# Database to store results
qids_results <- data.frame(
  outcome_variable = character(),
  beta_coefficient = numeric(),
  baseline_QIDS = numeric(),
  test_statistic = numeric(),
  standard_error = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

hamd_results <- data.frame(
  outcome_variable = character(),
  beta_coefficient = numeric(),
  baseline_HAMD = numeric(),
  test_statistic = numeric(),
  standard_error = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)



label_mapping <- list(
  frequency = "Hz",
  hnr = "dB",
  jitter = "%",
  shimmer = "%",
  duration = "Seconds",
  sentiment = "Score",
  rate = "Words per minute"
)
options(scipen = 999)

round_value <- function(value) {
  if (value < 0.01) {
    return(signif(value, digits = 1))
  } else {
    return(round(value, digits = 2))
  }
}

# Loop through outcome variables

for (i in seq_along(outcome_variables)) {
  # Extract the relevant words from the outcome variable
  outcome_var <- outcome_variables[i]
  outcome_words <- str_extract_all(outcome_var, "[A-Za-z]+")[[1]]
  for (word in outcome_words[[1]]) {
    if (word %in% names(label_mapping)) {
      y_axis_label <- label_mapping[[word]]
      break
    }
  }
#   # Create linear model
  qids_formula <- as.formula(paste(outcome_var, "~ Baseline_QIDS + sex + age_screening + testing_location + age_learned_english"))
  hamd_formula <- as.formula(paste(outcome_var, "~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english"))
  qidsmodel <- lm(qids_formula, data = MDD_feeling_BLq)
  hamdmodel <- lm(hamd_formula, data = MDD_feeling_BLq)
#   # Store results in the database
  qids_result_row <- data.frame(
    outcome_variable = outcome_var,
    beta_coefficient = formatC(summary(qidsmodel)$coefficients[2, 1], format = "f", digits = 5),
    test_statistic = formatC(coef(summary(qidsmodel))[2, 3], format = "f", digits = 3),
    standard_error = formatC(coef(summary(qidsmodel))[2, 2], format = "f", digits = 5),
    p_value = formatC(coef(summary(qidsmodel))[2, 4], format = "f", digits = 3),
    stringsAsFactors = FALSE
  )
  qids_results <- rbind(qids_results, qids_result_row)
  
  hamd_result_row <- data.frame(
    outcome_variable = outcome_var,
    beta_coefficient = formatC(summary(hamdmodel)$coefficients[2, 1], format = "f", digits = 5),
    test_statistic = formatC(coef(summary(hamdmodel))[2, 3], format = "f", digits = 3),
    standard_error = formatC(coef(summary(hamdmodel))[2, 2], format = "f", digits = 5),
    p_value = formatC(coef(summary(hamdmodel))[2, 4], format = "f", digits = 3),
    stringsAsFactors = FALSE
  )
  hamd_results <- rbind(hamd_results, hamd_result_row)
 
  
#   # Convert p_value to numeric
  qids_results$p_value <- as.numeric(qids_results$p_value)
  hamd_results$p_value <- as.numeric(hamd_results$p_value)

#   # Convert beta_coefficient to numeric
  qids_results$beta_coefficient <- as.numeric(qids_results$beta_coefficient)
  hamd_results$beta_coefficient <- as.numeric(hamd_results$beta_coefficient)


#   # Plot scatter plot of the data
  plt <- ggplot(MDD_feeling_BLq, aes(x = Baseline_HAMD, y = !!rlang::sym(outcome_var))) +
    geom_point(aes(color = "Baseline_HAMD"), show.legend = TRUE) +
    geom_smooth(method = "lm", se = TRUE, aes(fill = "Baseline_HAMD", color = "Baseline_HAMD"), show.legend = TRUE) +
    geom_point(data = MDD_feeling_BLq, aes(x = Baseline_QIDS, y = !!rlang::sym(outcome_var), color = "Baseline_QIDS"), show.legend = TRUE) +
    geom_smooth(data = MDD_feeling_BLq, aes(x = Baseline_QIDS, y = !!rlang::sym(outcome_var), fill = "Baseline_QIDS", color = "Baseline_QIDS"), method = "lm", se = TRUE, show.legend = TRUE) +
    xlab("Depression Severity") +
    ylab(y_axis_label) +
    labs(title = plot_titles[i]) +
    theme_classic() +
    scale_color_manual(name = "Depression Measure", values = c("Baseline_HAMD" = "#702963", "Baseline_QIDS" = "#0033FF"),
                       labels = c("HAMD", "QIDS")) +
    scale_fill_manual(name = "Depression Measure", values = c("Baseline_HAMD" = "#C3B1E1", "Baseline_QIDS" = "#99CCFF"),
                      labels = c("HAMD", "QIDS")) +
    labs(subtitle = paste0("HAMD: β = ", round_value(hamd_results$beta_coefficient[hamd_results$outcome_variable == outcome_var]),
                           ", p = ", round_value(hamd_results$p_value[hamd_results$outcome_variable == outcome_var]),
                           "; QIDS: β = ", round_value(qids_results$beta_coefficient[qids_results$outcome_variable == outcome_var]),
                           ", p = ", round_value(qids_results$p_value[qids_results$outcome_variable == outcome_var])))
  print(plt)
   
}
# Print the results
 print(hamd_results)
 print(qids_results)