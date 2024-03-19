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

WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS_2023FEB16.csv")
WL <- WL[grep("^TMS", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS remote_2023FEB16.csv")
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(ifelse(grepl("^CTC", WL$participant_external_id), "Control",
                                      ifelse(grepl("^TMS", WL$participant_external_id), "MDD", NA)))

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
demoCTRL <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet = 2)
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
col_index<-which(colnames(demoCTRL)=="bl_qids_tot")
colnames(demoCTRL)[col_index] <-"Baseline_QIDS"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL_Jou_BL <- merge(WL_Jou_BL, demoALL, by = "participant_external_id", all.x=TRUE)



#merge QIDS and HAMD results to MDD data
psych <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
col_index1<-which(colnames(psych)=="qids_total_pre")
colnames(psych)[col_index1] <-"Baseline_QIDS"
col_index2<-which(colnames(psych) == "hamd17_total_pre")
colnames(psych)[col_index2] <-"Baseline_HAMD"
colnames(psych)[1] <- "participant_external_id"
WL_Jou_BLq <- merge(WL_Jou_BL, psych[,c("participant_external_id", "Baseline_QIDS", "Baseline_HAMD")], by = "participant_external_id", all.x = TRUE)
# Populate empty or NA values in Baseline_QIDs with values from QIDs database
WL_Jou_BLq$Baseline_QIDS <- ifelse(is.na(WL_Jou_BLq$Baseline_QIDS.x) | WL_Jou_BLq$Baseline_QIDS.x == "", WL_Jou_BLq$Baseline_QIDS.y, WL_Jou_BLq$Baseline_QIDS.x)
# Remove the redundant Baseline_QIDs columns
WL_Jou_BLq <- subset(WL_Jou_BLq, select = -c(Baseline_QIDS.x, Baseline_QIDS.y))

# Separate by subtask
WL_feeling_BLq <- subsetStimulus(WL_Jou_BLq, "en_instruction_journal_feeling.mp3")



# List of outcome variables
outcome_variables <- c("fundamental_frequency_mean", "fundamental_frequency_variance", 
                       "fundamental_frequency_range", "hnr_ac_mean", "hnr_ac_variance",
                       "hnr_ac_range", "hnr_cc_mean", "hnr_cc_variance", "hnr_cc_range", 
                       "jitter_ddp", "jitter_local", "jitter_local_absolute", "jitter_ppq5",
                       "jitter_rap","shimmer_apq11", "shimmer_apq3","shimmer_apq5",
                       "shimmer_dda","shimmer_local", "shimmer_local_db","long_pause_count_normalized",
                       "short_pause_count_normalized","medium_pause_duration", "sentiment_arousal",
                       "sentiment_valence","speech_rate", "tag_VBG", "tag_RB","pos_DET", 
                       "morph_number_plur", "morph_aspect_prog", "mfcc_var_31", "mfcc_var_3",
                       "mfcc_var_19", "mfcc_mean_40", "intensity_mean_sones", "intensity_median",
                       "imageability","graph_num_nodes", "age_of_acquisition","articulation_rate",
                       "VP_.._VBG_NP", "VP_.._VBD_SBAR", "S_.._NP_ADVP_VP", "PP_.._IN_S",
                       "NP_.._NN_NN", "NP_.._DT_NN","NP_.._DT","NP_.._CD_NN", "tag_participant_clarity" )  # Add your desired outcome variables here

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
options(scipen = 999)
# Loop through outcome variables
for (outcome_var in outcome_variables) {
  # Create linear model
  qids_formula <- as.formula(paste(outcome_var, "~ Baseline_QIDS + sex + age_screening + testing_location + age_learned_english"))
  hamd_formula <- as.formula(paste(outcome_var, "~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english"))
  qidsmodel <- lm(qids_formula, data = WL_feeling_BLq)
  hamdmodel <-lm(hamd_formula, data = WL_feeling_BLq)
  # Store results in the database
  qids_result_row <- data.frame(
    outcome_variable = outcome_var,
    beta_coefficient = formatC(summary(qidsmodel)$coefficients[2, 1], format = "f", digits = 3),
    test_statistic = formatC(coef(summary(qidsmodel))[2, 3], format = "f", digits = 3),
    standard_error = formatC(coef(summary(qidsmodel))[2, 2], format = "f", digits = 3),
    p_value = formatC(coef(summary(qidsmodel))[2, 4], format = "f", digits = 3),
    stringsAsFactors = FALSE
  )
  qids_results <- rbind(qids_results, qids_result_row)
  
  hamd_result_row <- data.frame(
    outcome_variable = outcome_var,
    beta_coefficient = formatC(summary(hamdmodel)$coefficients[2, 1], format = "f", digits = 3),
    test_statistic = formatC(coef(summary(hamdmodel))[2, 3], format = "f", digits = 3),
    standard_error = formatC(coef(summary(hamdmodel))[2, 2], format = "f", digits = 3),
    p_value = formatC(coef(summary(hamdmodel))[2, 4], format = "f", digits = 3),
    stringsAsFactors = FALSE
  )
  hamd_results <- rbind(hamd_results, hamd_result_row)
  
  # Plot scatter plot of the data
  plt <- ggplot(WL_feeling_BLq, aes(x = Baseline_HAMD, y = !!rlang::sym(outcome_var))) +
    geom_point(color = "#9F2B68") +
    geom_smooth(method = "lm", color = "#702963", fill = "#C3B1E1") +
    geom_point(data = WL_feeling_BLq, aes(x = Baseline_QIDS, y = !!rlang::sym(outcome_var)),
               color = "#0033FF") +
    geom_smooth(data = WL_feeling_BLq, aes(x = Baseline_QIDS, y = !!rlang::sym(outcome_var)),
                method = "lm", color = "#0033FF", fill = "#99CCFF") +
    xlab("Depression Severity") +
    ylab("Outcome") +
    labs(title = outcome_var) +
    theme_classic()
  
  # Print the plot
  print(plt)
}

# Print the results
print(hamd_results)
print(qids_results)

linear_regression_assumptions(MDD_feeling_BLq, "short_pause_count_normalized", c("Baseline_QIDS", "sex", "age_screening", "testing_location","age_learned_english"))
model <- lm("fundamental_frequency_variance" ~ Baseline_QIDS + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BLq)
scatter <- ggplot(MDD_feeling_BLq, aes(y = "fundamental_frequency_variance", x = Baseline_QIDS)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "QIDS", y = "Variance", title = "F0") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
scatter <- scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(model)$coefficients[2,1], 4),
                         ", p = ", signif(summary(model)$coefficients[2, 4], 2)))
scatter
summary(model)

