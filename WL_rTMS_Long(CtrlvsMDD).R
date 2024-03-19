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


WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS_2023FEB16.csv")
WL <- WL[grep("^TMS", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS remote_2023FEB16.csv")
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$Group <- factor(ifelse(grepl("^CTC", WL$participant_external_id), "Control",
                          ifelse(grepl("^TMS", WL$participant_external_id), "MDD", NA)))


# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person")


# Replace values in session_label column

WL <- WL[!(WL$session_label %in% c("V4", "V5", "V6", "V7", "V8")), ]
WL$session_label <- gsub("Baseline|V1", "1", WL$session_label)
WL$session_label <- gsub("Week 2|V2", "2", WL$session_label)
WL$session_label <- gsub("Week 6|V3", "3", WL$session_label)
WL$session_label <-as.numeric(WL$session_label)

# Convert to datetime, remove extra tasks and sort by date
WL$sample_datetime_completed_utc <-as.Date(WL$sample_datetime_completed_utc, origin="1970-01-01")
WL<- WL[!(WL$sample_id >= 85848 & WL$sample_id <= 85862), ]  


WL_Jou <- subsetTask(WL, "journaling")
#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_Demographics.xlsx")
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet = 2)
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL_Jou <- merge(WL_Jou, demoALL, by = "participant_external_id", all.x=TRUE)

# Separate by subtask
WL_feeling <- subsetStimulus(WL_Jou, "en_instruction_journal_feeling.mp3")
# Take out duplicates 
WL_feeling <- WL_feeling[!(WL_feeling$participant_external_id == "TMS031" & WL_feeling$session_label == "V2"), ]
WL_feeling <- WL_feeling[!(WL_feeling$participant_external_id == "TMS032" & WL_feeling$session_label == "V2"), ]


# Calculate days between visits for each participant
WL_feeling$sample_datetime_completed_utc <- as.POSIXct(WL_feeling$sample_datetime_completed_utc)
# sort the data frame by ID and date
WL_feeling <- WL_feeling[order(WL_feeling$participant_external_id, WL_feeling$sample_datetime_completed_utc),]
# create a new column called "time_diff" to store the time differences in days
WL_feeling$time_diff <- NA
# loop through each unique ID
for (id in unique(WL_feeling$participant_external_id)) {
  # get the subset of the data frame for this ID
  subset_df <- WL_feeling[WL_feeling$participant_external_id == id,]
  # calculate the time differences in days
  time_diff <- as.numeric(diff(subset_df$sample_datetime_completed_utc), units = "days")
  # assign the time differences to the appropriate rows in the "time_diff" column
  WL_feeling$time_diff[which(WL_feeling$participant_external_id == id)[-1]] <- time_diff
}

time_diffs<-data.frame(participant_external_id = WL_feeling$participant_external_id, session_label = WL_feeling$session_label, time_diff = WL_feeling$time_diff)


# Loop over each participant ID
for (id in unique(WL_feeling$participant_external_id)) {
  # Subset the data frame for the current participant
  participant_df <- WL_feeling[WL_feeling$participant_external_id == id, ]
  # Find the row index for V2 session
  v2_row <- which(participant_df$session_label == "2")
  # If there is no V2 session, skip to the next participant ID
  if (length(v2_row) == 0) {
    next
  }
  # Check if time difference is greater than 20 for V2 sessions
  if (any(participant_df$time_diff[v2_row] > 20)) {
    # If condition is true, change session label to V3 for V2 session
    participant_df$session_label[v2_row] <- "3"
  }
  # Update the original data frame with the modified values
  WL_feeling[WL_feeling$participant_external_id == id, ] <- participant_df
}

#check time diffs again
time_diffs<-data.frame(participant_external_id = WL_feeling$participant_external_id, session_label = WL_feeling$session_label, time_diff = WL_feeling$time_diff)

# Count TMS participants at each visit
tms_count_1 <- length(unique(grep("^TMS", WL_feeling$participant_external_id[WL_feeling$session_label == "1"], value = TRUE)))
tms_count_2 <- length(unique(grep("^TMS", WL_feeling$participant_external_id[WL_feeling$session_label == "2"], value = TRUE)))
tms_count_3 <- length(unique(grep("^TMS", WL_feeling$participant_external_id[WL_feeling$session_label == "3"], value = TRUE)))

# Print count of TMS participants with different session labels
cat("Number of TMS patients with visit 1: ", tms_count_1, "\n")
cat("Number of TMS patients with visit 2: ", tms_count_2, "\n")
cat("Number of TMS patients with visit 3: ", tms_count_3, "\n")

# Count control participants at each visit
ctrl_count_1 <- length(unique(grep("^CTC", WL_feeling$participant_external_id[WL_feeling$session_label == "1"], value = TRUE)))
ctrl_count_2 <- length(unique(grep("^CTC", WL_feeling$participant_external_id[WL_feeling$session_label == "2"], value = TRUE)))
ctrl_count_3 <- length(unique(grep("^CTC", WL_feeling$participant_external_id[WL_feeling$session_label == "3"], value = TRUE)))

# Print count of controls with different session labels
cat("Number of controls with visit 1: ", ctrl_count_1, "\n")
cat("Number of controls  with visit 2: ", ctrl_count_2, "\n")
cat("Number of controls patients with visit 3: ", ctrl_count_3, "\n")

# function that data frame and outcome variable as inputs 
library(lme4)
library(lmerTest)
library(sjPlot)
library(car)

# Define the function for the analysis
my_lmer <- function(data, outcome_var, y.axis, plot_title) {
  # Create the formula string
  data$session_label <- as.numeric(data$session_label)
  formula_str <- paste(outcome_var, "~ session_label * Group + 
                       age_screening + sex + age_learned_english + 
                       (1| participant_external_id)")
  # Fit the linear mixed-effects model
  model <- lmer(formula_str, data = data)
  print(summary(model))
  
  # Plot
  plot <- plot_model(model, type = "pred", terms = c("session_label", "Group"),
                     show.ci = TRUE, show.data = TRUE, 
                     colors = c("blue", "#FF76F2")) +
    xlab("Visit number") +
    ylab(y.axis) +
    scale_x_continuous(breaks = c(1, 2, 3)) +  
    theme(strip.text.x = element_text(size = 35),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.line = element_line()) +
    guides(color = guide_legend(override.aes = list(fill = c("blue", "#FF76F2"))))+
    labs(title=plot_title)
  
  return(plot)
}

#sentiment arousal 
my_lmer(WL_feeling, "sentiment_arousal", "Arousal score", "Sentiment arousal")
#sentiment dominance
my_lmer(WL_feeling, "sentiment_dominance" , "Dominance score", "Sentiment dominance")
#sentiment valence
my_lmer(WL_feeling, "sentiment_valence", "Valence score", "Sentiment valence")
#speech_rate
my_lmer(WL_feeling, "speech_rate", "Words per minute", "Speech rate")
#fundamental frequency range
my_lmer(WL_feeling, "fundamental_frequency_range", "F0 range (Hz)", "Fundamental frequency, range")
#fundamental frequency variance
my_lmer(WL_feeling, "fundamental_frequency_variance", "F0 variance (Hz)", "Fundamental frequency, variance")
#fundamental frequency mean
my_lmer(WL_feeling, "fundamental_frequency_mean", "Mean F0 (Hz)", "Mean fundamental frequency")
#medium pause duration 
my_lmer(WL_feeling, "medium_pause_duration", "Pause duration (s)", "Pause duration")
#HNR ac mean 
my_lmer(WL_feeling, "hnr_ac_mean", "HNR (dB)", "Mean HNR")
#HNR ac variance 
my_lmer(WL_feeling, "hnr_ac_variance", "HNR (dB)", "HNR, variance")
#Avg change in jitter 
my_lmer(WL_feeling, "jitter_ddp", "%", "Average change in jitter")
#Local jitter
my_lmer(WL_feeling, "jitter_local", "%", "Local jitter")
# Relative distal jitter
my_lmer(WL_feeling, "jitter_ppq5", "%", "Relative distal jitter")
# Relative proximal jitter 
my_lmer(WL_feeling, "jitter_rap", "%", "Relative proximal jitter")
# Relative distal shimmer
my_lmer(WL_feeling, "shimmer_apq11", "%", "Relative distal shimmer")
# Relative proximal shimmer 
my_lmer(WL_feeling, "shimmer_apq3", "%", "Relative proximal shimmer")
# Avg change in shimmer 
my_lmer(WL_feeling, "shimmer_dda", "%", "Average change in shimmer")
# local shimmer 
my_lmer(WL_feeling, "shimmer_local", "%", "Local shimmer")
# pronoun count 
my_lmer(WL_feeling, "tag_PRP", "Raw count", "Personal pronoun use")








