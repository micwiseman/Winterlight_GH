library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)
library(stringr)

# Read Winterlight task data for rTMS patients and HAMD data 
MDD <- readMDDdata("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2022JULY20.csv")
HAMD <- extractHAMD("~/Lab/Winterlight/rTMS-HAMD.csv")
# Extract baseline HAMD and winterlight data and merge into one df 
HAMD_bl <- HAMD %>%
  select('participant_external_id', 'baseline_HAMD')
MDD <- left_join(HAMD_bl, MDD, by = "participant_external_id")
colnames(MDD)[2] <- "baseline_HAMD"

MDD$sample_datetime_completed_utc <- as.character(MDD$sample_datetime_completed_utc)

MDD_bl <- extractBaseline(MDD)
#Subset baseline data by task
MDD_bl_read<- subsetTask(MDD_bl,"paragraph_reading")
MDD_bl_jou1 <- subsetTask(MDD_bl, "picture_description")
MDD_bl_pic1 <- subsetStimulus(MDD_bl,"01_WinterLight_Family_in_the_kitchen_web.png")
MDD_bl_pic2 <- subsetStimulus(MDD_bl,"02_WinterLight_Living_room_web.png")
MDD_bl_pho<- subsetTask(MDD_bl,"phonemic_fluency")
MDD_bl_sem<- subsetTask(MDD_bl,"semantic_fluency")
MDD_bl_pos <- subsetTask(MDD_bl, "positive_fluency")
MDD_bl_jou <- subsetTask(MDD_bl, "journaling")
MDD_bl_jou1 <- subsetStimulus(MDD_bl,"en_instruction_journal_feeling.mp3")
MDD_bl_jou2 <- subsetStimulus(MDD_bl,"en_instruction_yesterday.mp3")
MDD_bl_recall <- subsetTask(MDD_bl, "paragraph_recall")

MDD_bl_pos <- MDD_bl_pos %>%
  filter(participant_external_id != "TMS004")

# Read in control data 
control <- read_csv("~/Lab/Winterlight/WINTERLIGHT_InternalPsychSession2_4MAR2022.csv")
# Subset control data by task 
control_read<- subsetTask(control,"paragraph_reading")
control_pic <- subsetTask(control, "picture_description")
control_pic1 <- subsetStimulus(control,"01_WinterLight_Family_in_the_kitchen_web.png")
control_pic2 <- subsetStimulus(control,"02_WinterLight_Living_room_web.png")
control_pho<- subsetTask(control,"phonemic_fluency")
control_sem<- subsetTask(control,"semantic_fluency")
control_pos <- subsetTask(control, "positive_fluency")
control_jou <- subsetTask(control, "journaling")
control_jou1 <- subsetStimulus(control,"en_instruction_journal_feeling.mp3")
control_jou2 <- subsetStimulus(control,"en_instruction_yesterday.mp3")
control_recall <- subsetTask(control, "paragraph_recall")



speech_vars <- c("NOUN_sentiment_arousal", "NOUN_sentiment_dominance", "NOUN_sentiment_valence",
                "VERB_sentiment_arousal", "VERB_sentiment_dominance", "VERB_sentiment_valence", 
                 "articulation_rate", "fundamental_frequency_mean", "fundamental_frequency_range",
                 "fundamental_frequency_variance", "hnr_ac_max", "hnr_ac_mean", "hnr_ac_median", "hnr_ac_min", 
                 "hnr_ac_range", "hnr_ac_variance", "hnr_cc_max", "hnr_cc_mean", "hnr_cc_median", "hnr_cc_min", 
                 "hnr_cc_range", "hnr_cc_variance", "jitter_ddp", "jitter_local", "jitter_local_absolute", 
                 "jitter_ppq5", "jitter_rap", "long_pause_count_normalized", "long_pause_duration", 
                 "mean_pause_duration", "medium_pause_count_normalized", "medium_pause_duration", 
                 "pause_word_ratio", "phonation_rate", "sentiment_arousal", "sentiment_dominance", "sentiment_valence", 
                 "shimmer_apq11", "shimmer_apq3", "shimmer_apq5", "shimmer_dda", "shimmer_local", "shimmer_local_db", 
                 "short_pause_duration", "speech_rate", "total_duration_speech", "unfilled_pause_duration", "unfilled_pauses")
                 

# Loop through variables for combined picture tasks 
pic_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_jou1[[vars]])) < 2 || sum(!is.na(control_pic[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  pic_ttest_result <- t.test(MDD_bl_jou1[[vars]], control_pic[[vars]], na.rm = TRUE)
  if (pic_ttest_result$p.value < 0.05) {
    pic_t.test_sig <- rbind(pic_t.test_sig, data.frame(col_name = vars, statistic = pic_ttest_result$statistic, p.value = pic_ttest_result$p.value))
  }
}


# Loop through variables for picture 1 
pic1_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_pic1[[vars]])) < 2 || sum(!is.na(control_pic1[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  pic1_ttest_result <- t.test(MDD_bl_pic1[[vars]], control_pic1[[vars]], na.rm = TRUE)
  if (pic1_ttest_result$p.value < 0.05) {
    pic1_t.test_sig <- rbind(pic1_t.test_sig, data.frame(col_name = vars, statistic = pic1_ttest_result$statistic, p.value = pic1_ttest_result$p.value))
  }
}


# Loop through variables for  picture 2 
pic2_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_pic2[[vars]])) < 2 || sum(!is.na(control_pic2[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  pic2_ttest_result <- t.test(MDD_bl_pic2[[vars]], control_pic2[[vars]], na.rm = TRUE)
  if (pic2_ttest_result$p.value < 0.05) {
    pic2_t.test_sig <- rbind(pic2_t.test_sig, data.frame(col_name = vars, statistic = pic2_ttest_result$statistic, p.value = pic2_ttest_result$p.value))
  }
}

# Loop through variables for combined journaling
jou_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_jou[[vars]])) < 2 || sum(!is.na(control_jou[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  jou_ttest_result <- t.test(MDD_bl_jou[[vars]], control_jou[[vars]], na.rm = TRUE)
  if (jou_ttest_result$p.value < 0.05) {
    jou_t.test_sig <- rbind(jou_t.test_sig, data.frame(col_name = vars, statistic = jou_ttest_result$statistic, p.value = jou_ttest_result$p.value))
  }
}

# Loop through variables for journaling 1
jou1_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_jou1[[vars]])) < 2 || sum(!is.na(control_jou1[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  jou1_ttest_result <- t.test(MDD_bl_jou1[[vars]], control_jou1[[vars]], na.rm = TRUE)
  if (jou1_ttest_result$p.value < 0.05) {
    jou1_t.test_sig <- rbind(jou1_t.test_sig, data.frame(col_name = vars, statistic = jou1_ttest_result$statistic, p.value = jou1_ttest_result$p.value))
  }
}

# Loop through variables for phonemic fluency
pho_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_pho[[vars]])) < 2 || sum(!is.na(control_pho[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  pho_ttest_result <- t.test(MDD_bl_pho[[vars]], control_pho[[vars]], na.rm = TRUE)
  if (pho_ttest_result$p.value < 0.05) {
    pho_t.test_sig <- rbind(pho_t.test_sig, data.frame(col_name = vars, statistic = pho_ttest_result$statistic, p.value = pho_ttest_result$p.value))
  }
}

# Loop through variables for semantic fluency
sem_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_sem[[vars]])) < 2 || sum(!is.na(control_sem[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  sem_ttest_result <- t.test(MDD_bl_sem[[vars]], control_sem[[vars]], na.rm = TRUE)
  if (sem_ttest_result$p.value < 0.05) {
    sem_t.test_sig <- rbind(sem_t.test_sig, data.frame(col_name = vars, statistic = sem_ttest_result$statistic, p.value = sem_ttest_result$p.value))
  }
}

# Loop through variables for paragraph reading
read_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_read[[vars]])) < 2 || sum(!is.na(control_read[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  read_ttest_result <- t.test(MDD_bl_read[[vars]], control_read[[vars]], na.rm = TRUE)
  if (read_ttest_result$p.value < 0.05) {
    read_t.test_sig <- rbind(read_t.test_sig, data.frame(col_name = vars, statistic = read_ttest_result$statistic, p.value = read_ttest_result$p.value))
  }
}

# Loop through variables for paragraph recall
rec_t.test_sig <- data.frame(col_name = character(), statistic = numeric(), p.value = numeric())
for (vars in speech_vars) {
  if (sum(!is.na(MDD_bl_recall[[vars]])) < 2 || sum(!is.na(control_recall[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because one of the groups has less than 2 observations"))
    next
  }
  recall_ttest_result <- t.test(MDD_bl_recall[[vars]], control_recall[[vars]], na.rm = TRUE)
  if (recall_ttest_result$p.value < 0.05) {
    recall_t.test_sig <- rbind(recall_t.test_sig, data.frame(col_name = vars, statistic = recall_ttest_result$statistic, p.value = recall_ttest_result$p.value))
  }
}



# Loop through all variables for combined pic tasks and perform Linear regression
# create an empty data frame to store the results
sem_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou1[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou1$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_jou1[[vars]] ~ baseline_HAMD, data = MDD_bl_jou1)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    sem_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}


# Loop through all variables pic1 and perform Linear regression
# create an empty data frame to store the results
pic1_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pic1[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pic1$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_pic1[[vars]] ~ baseline_HAMD, data = MDD_bl_pic1)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    pic1_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}

# Loop through all variables for pic 2 and perform Linear regression
# create an empty data frame to store the results
pic2_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pic2[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pic2$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_pic2[[vars]] ~ baseline_HAMD, data = MDD_bl_pic2)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    pic2_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}


# Loop through all variables for combined journal tasks and perform Linear regression
# create an empty data frame to store the results
jou_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_jou[[vars]] ~ baseline_HAMD, data = MDD_bl_jou)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    jou_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}

# Loop through all variables for journal 1 and perform Linear regression
# create an empty data frame to store the results
jou1_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou1[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou1$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_jou1[[vars]] ~ baseline_HAMD, data = MDD_bl_jou1)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    jou1_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}

# Loop through all variables for journal 2 and perform Linear regression
# create an empty data frame to store the results
jou2_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou2[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_jou2$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_jou2[[vars]] ~ baseline_HAMD, data = MDD_bl_jou2)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    jou2_results_lm <- rbind(results_df, data.frame(variable = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}


# Loop through all variables for positive fluency tasks and perform Linear regression
# create an empty data frame to store the results
pos_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pos[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pos$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_pos[[vars]] ~ baseline_HAMD, data = MDD_bl_pos)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    pos_results_lm <- rbind(results_df, data.frame(predictor = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}

# Loop through all variables for  semantic fluency  and perform Linear regression
# create an empty data frame to store the results
sem_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_sem[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_sem$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_sem[[vars]] ~ baseline_HAMD, data = MDD_bl_sem)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    sem_results_lm <- rbind(results_df, data.frame(predictor = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}

# Loop through all variables for  phonemic fluency  and perform Linear regression
# create an empty data frame to store the results
pho_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pho[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_pho$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_pho[[vars]] ~ baseline_HAMD, data = MDD_bl_pho)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    pho_results_lm <- rbind(results_df, data.frame(predictor = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}



# Loop through all variables for paragraph reading and perform Linear regression
# create an empty data frame to store the results
read_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_read[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_read$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_read[[vars]] ~ baseline_HAMD, data = MDD_bl_read)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    read_results_lm <- rbind(results_df, data.frame(predictor = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}


# Loop through all variables for paragraph fluency and perform Linear regression
# create an empty data frame to store the results
recall_results_lm <- data.frame(vars = character(), coefficient = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
# loop through the variables in the MDD_bl_pic data frame
for (vars in speech_vars) {
  # check if the response variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_recall[[vars]])) < 2) {
    message(paste("Skipping variable", vars, "because the response variable has less than 2 non-NA observations"))
    next
  }
  # check if the predictor variable has at least 2 non-NA observations
  if (sum(!is.na(MDD_bl_recall$baseline_HAMD)) < 2) {
    message(paste("Skipping variable", vars, "because the predictor variable has less than 2 non-NA observations"))
    next
  }
  # fit a linear regression model
  model <- lm(MDD_bl_recall[[vars]] ~ baseline_HAMD, data = MDD_bl_recall)
  # extract the p-value of the predictor
  p.value <- summary(model)$coef[2, 4]
  # store the results if the p-value is significant
  if (p.value < 0.05) {
    recall_results_lm <- rbind(results_df, data.frame(predictor = vars, coefficient = summary(model)$coef[2, 1], p.value = p.value, stringsAsFactors = FALSE))
  }
}







