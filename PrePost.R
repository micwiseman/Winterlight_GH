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

# Convert to datetime, remove extra tasks and sort by date
WL$sample_datetime_completed_utc <-as.Date(WL$sample_datetime_completed_utc, origin="1970-01-01")
WL<- WL[!(WL$sample_id >= 85848 & WL$sample_id <= 85862), ]  


WL_Jou <- subsetTask(WL, "journaling")
#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_Demographics.xlsx")
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/WL_Control_Demographics.xlsx")
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL_Jou<- merge(WL_Jou, demoALL, by = "participant_external_id", all.x=TRUE)
MDD_Jou <- WL_Jou %>% filter(participant_group == "MDD")

MDD_Jou_BL <- WL_Jou_BL %>% filter(participant_group == "MDD")
#merge HAMD results to MDD data
HAMD <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
col_index<-which(colnames(HAMD)=="hamd17_total_pre")
colnames(HAMD)[col_index] <-"Baseline_HAMD"
colnames(HAMD)[1] <- "participant_external_id"
MDD_Jou_BL <- merge(MDD_Jou_BL, HAMD[,c("participant_external_id", "Baseline_HAMD", "hamd17_total_wk4")], by = "participant_external_id", all.x = TRUE)
MDD_Jou <- merge(MDD_Jou, HAMD[,c("participant_external_id", "Baseline_HAMD", "hamd17_total_wk4")], by = "participant_external_id", all.x = TRUE)
Ctrl_Jou_BL <- WL_Jou_BL %>% filter(participant_group == "Control")

# Separate by subtask
WL_feeling <- subsetStimulus(WL_Jou, "en_instruction_journal_feeling.mp3")
# Take out duplicates 
WL_feeling <- WL_feeling[!(WL_feeling$participant_external_id == "CTC025" & WL_feeling$session_label == "V2"), ]
WL_feeling <- WL_feeling[!(WL_feeling$participant_external_id == "TMS031" & WL_feeling$session_label == "V2"), ]
WL_feeling <- WL_feeling[!(WL_feeling$participant_external_id == "TMS032" & WL_feeling$session_label == "V2"), ]


MDD_feeling <- WL_feeling %>% filter(participant_group == "MDD")
MDD_feeling <- merge(MDD_feeling, HAMD[,c("participant_external_id", "Baseline_HAMD", "hamd17_total_wk4")], by = "participant_external_id", all.x = TRUE)
Ctrl_feeling <- WL_feeling %>% filter(participant_group == "Control")


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
  v2_row <- which(participant_df$session_label == "V2")
  # If there is no V2 session, skip to the next participant ID
  if (length(v2_row) == 0) {
    next
  }
  # Check if time difference is greater than 20 for V2 sessions
  if (participant_df$time_diff[v2_row] > 20) {
    # If condition is true, change session label to V3 for V2 session
    participant_df$session_label[v2_row] <- "V3"
  }
  # Update the original data frame with the modified values
  WL_feeling[WL_feeling$participant_external_id == id, ] <- participant_df
}

#check time diffs again
time_diffs<-data.frame(participant_external_id = WL_feeling$participant_external_id, session_label = WL_feeling$session_label, time_diff = WL_feeling$time_diff)

# Create data frame that only has data for participants with all three assessment points 
long_feeling<-data.frame()
for (id in unique(WL_feeling$participant_external_id)) {
  #subset data frame for current ID
  id_df <- WL_feeling[WL_feeling$participant_external_id == id,]
  # check if all V1, V2, V3 session labels are present
  if ("V1" %in% id_df$session_label & 
      "V2" %in% id_df$session_label & 
      "V3" %in% id_df$session_label) {
    # if all session labels are present, add to filtered dataframe
    long_feeling <- rbind(long_feeling, id_df)
  }
}

# Count controls and patients in the ANOVA data frame 
ctc_count <- length(unique(grep("^CTC", long_feeling$participant_external_id, value = TRUE)))
# print count of "CTC" IDs
cat("Number of controls: ", ctc_count, "\n")
# count unique IDs that start with "TMS"
tms_count <- length(unique(grep("^TMS", long_feeling$participant_external_id, value = TRUE)))
# print count of "TMS" IDs
cat("Number of TMS patients: ", tms_count, "\n")


# Create data frame that only has data for participants with 2 assessment points 
twovisits_feeling<-data.frame()
for (id in unique(WL_feeling$participant_external_id)) {
  #subset data frame for current ID
  id_df <- WL_feeling[WL_feeling$participant_external_id == id,]
  # check if all V1, V2, V3 session labels are present
  if ("V1" %in% id_df$session_label & 
      "V2" %in% id_df$session_label) {
    # if all session labels are present, add to filtered dataframe
    twovisits_feeling <- rbind(twovisits_feeling, id_df)
  }
}

# Count controls and patients in the ANOVA data frame 
ctc_count <- length(unique(grep("^CTC", twovisits_feeling$participant_external_id, value = TRUE)))
# print count of "CTC" IDs
cat("Number of controls: ", ctc_count, "\n")
# count unique IDs that start with "TMS"
tms_count <- length(unique(grep("^TMS", twovisits_feeling$participant_external_id, value = TRUE)))
# print count of "TMS" IDs
cat("Number of TMS patients: ", tms_count, "\n")




### Linear Mixed Models  ##### 
# Load the necessary libraries

# function that data frame and outcome variabe as inputs 
library(lme4)
library(car)
library(emmeans)

my_lmer <- function(data, outcome_var){
  model <- lmer(get(outcome_var) ~ session_label*participant_group + age_screening + 
                  sex + testing_location + first_language_english +(1|participant_external_id), data = data)
  # Check the model assumptions
  # Check normality of residuals
  qqnorm(resid(model))
  qqline(resid(model)) # relatively straight line = normal 
  # Check homogeneity of variance
  plot(model, which = 1) # points evenly distributed around 0 = contstant variance 
  # Check for outliers
  plot(model, which = 5) 
  # Check for influential observations
  influenceIndexPlot(model)
  # Check for multicollinearity
  vif(model)
  
  # Plot the data and fitted model
  plot(ggplot(data, aes(x = session_label, y = get(outcome_var), group = participant_group, color = participant_group)) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_point(alpha=0.3) +
    labs(title = paste0(outcome_var, " by time point"), 
         x = "Assessment Number", y = "Outcome", color = "Group") +
    scale_color_manual(values = c("Control" = "blue", "MDD" = "red"), labels = c("Control", "MDD")) +
    theme_classic())
  # Print the model summary
  summary(model)
}



#sentiment arousal 
my_lmer(long_feeling, "sentiment_arousal")
#sentiment dominance
my_lmer(long_feeling, "sentiment_dominance")
#sentiment valence
my_lmer(long_feeling, "sentiment_valence")
#speech_rate
my_lmer(long_feeling, "speech_rate")
#fundamental frequency range
my_lmer(long_feeling, "fundamental_frequency_range")
#fundamental frequency variance
my_lmer(long_feeling, "fundamental_frequency_variance")
#fundamental frequency mean
my_lmer(long_feeling, "fundamental_frequency_mean")
#long pause count 
my_lmer(long_feeling, "long_pause_count_normalized")
#long pause duration 
my_lmer(long_feeling, "long_pause_duration")
#medium_pause_count_normalized 
my_lmer(long_feeling, "medium_pause_count_normalized")
#medium pause duration 
my_lmer(long_feeling, "medium_pause_duration")
# short pause duration 
my_lmer(long_feeling, "short_pause_duration")
#pause_word_ratio
my_lmer(long_feeling, "pause_word_ratio")
#phonation rate 
my_lmer(long_feeling, "phonation_rate")
# total duration speech 
my_lmer(long_feeling, "total_duration_speech")




MDD_feeling_BL <- extractBaseline(MDD_feeling)
Four_weeks <- MDD_feeling %>% filter(session_label == "V3")
Baseline <- MDD_feeling_BL[MDD_feeling_BL$participant_external_id %in% MDD_feeling_post$participant_external_id,]
Baseline$participant_external_id
MDD_feeling_post$participant_external_id



t.test(Baseline$sentiment_arousal, Four_weeks$sentiment_arousal)

# Extract the column you want to plot from each data frame
preArous <- Baseline$sentiment_arousal
postArous <- Four_weeks$sentiment_arousal
# Combine the columns into a single data frame
combinedArous <- data.frame(group = c(rep("MDD_feeling_BL", length(preArous)), 
                                      rep("Four_weeks", length(postArous))), 
                            value= c(preArous, postArous))
# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, paired = T, data = combinedArous)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedArous, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post Sentiment Arousal", x = "", y = "Arousal score", fill = "", subtitle = paste("p = ", round(pval, 3), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("MDD_feeling_BL", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)

# Extract the column you want to plot from each data frame
preVal <- Baseline$sentiment_valence
postVal <- Four_weeks$sentiment_valence
# Combine the columns into a single data frame
combinedVal <- data.frame(group = c(rep("MDD_feeling_BL", length(preVal)), 
                                      rep("Four_weeks", length(postVal))), 
                            value= c(preVal, postVal))
# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, paired = T, data = combinedVal)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedVal, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post Sentiment Valence", x = "", y = "valence score", fill = "", subtitle = paste("p = ", round(pval, 3), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("MDD_feeling_BL", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)

t.test(Baseline$speech_rate, Four_weeks$speech_rate)

# Extract the column you want to plot from each data frame
prespeechrate <- Baseline$speech_rate
postspeechrate <- Four_weeks$speech_rate
# Combine the columns into a single data frame
combinedspeechrate <- data.frame(group = c(rep("Baseline", length(prespeechrate)), 
                                      rep("Four_weeks", length(postspeechrate))), 
                            value= c(prespeechrate, postspeechrate))
# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, data = combinedspeechrate)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedspeechrate, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post Speech Rate", x = "", y = "Speech Rate", fill = "", subtitle = paste("p = ", round(pval, 3), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("Baseline", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)

t.test(Baseline$fundamental_frequency_mean, Four_weeks$fundamental_frequency_mean)

# Extract the column you want to plot from each data frame
pref0 <- Baseline$fundamental_frequency_mean
postf0 <- Four_weeks$fundamental_frequency_mean
# Combine the columns into a single data frame
combinedf0 <- data.frame(group = c(rep("Baseline", length(pref0)), 
                                      rep("Four_weeks", length(postf0))), 
                            value= c(pref0, postf0))
# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, data = combinedf0)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedf0, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post f0", x = "", y = "f0", fill = "", subtitle = paste("p = ", round(pval, 3), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("Baseline", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)



t.test(Baseline$medium_pause_duration, Four_weeks$medium_pause_duration)

# Extract the column you want to plot from each data frame
premedpause <- Baseline$medium_pause_duration
postmedpause <- Four_weeks$medium_pause_duration
# Combine the columns into a single data frame
combinedmedpause <- data.frame(group = c(rep("Baseline", length(premedpause)), 
                                   rep("Four_weeks", length(postmedpause))), 
                         value= c(premedpause, postmedpause))
# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, data = combinedmedpause)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedmedpause, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post Pause Duration", x = "", y = "Medium Pause Duration", fill = "", subtitle = paste("p = ", round(pval, 3), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("Baseline", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)




t.test(Baseline$Baseline_HAMD, Four_weeks$Baseline_HAMD)


# Extract the column you want to plot from each data frame
preHAMD <- Baseline$Baseline_HAMD
postHAMD <- Four_weeks[[ncol(Four_weeks)]]
# Combine the columns into a single data frame
combinedHAMD <- data.frame(group = c(rep("Baseline", length(preHAMD)), 
                                         rep("Four_weeks", length(postHAMD))), 
                               value= c(preHAMD, postHAMD))
combinedHAMD <- combinedHAMD[-c(16, 7),]

# Create the box plot
# Perform t-test
ttest_result <- t.test(value ~ group, data = combinedHAMD)

# Get p-value and significance level
pval <- ttest_result$p.value
if (pval < 0.05) {
  significance <- "*"
} else {
  significance <- ""
}

# Create plot
ggplot(combinedHAMD, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, size = 3, width = 0.2) + 
  labs(title = "Pre Post HAMD", x = "", y = "HAMD", fill = "", subtitle = paste("p = ", round(pval, 5), significance)) + 
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16), 
        legend.position = "none", title = element_text(size = 20)) + 
  theme_classic() +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  guides(fill = FALSE) +
  geom_signif(comparisons = list(c("Baseline", "Four_weeks")), 
              map_signif_level = ifelse(pval < 0.05, TRUE, FALSE), 
              annotations = c("*"),
              textsize = 5, vjust = -0.5, bracketHeight = 0.05)

to