library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)

MDD <- read_csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2022JULY20.csv")

# Removing OCD and MDD controls 
MDD <- MDD %>%
  filter(participant_external_id != "OCD108" & participant_external_id != "OCD109" & participant_external_id != "OCD110" & 
           participant_external_id != "OCD111" & participant_external_id != "OCD112" & participant_external_id != "MDD103" &
           participant_external_id != "MDD104")

# Filtering for baseline 
MDD_bl <- MDD %>%
  filter(session_label == "Baseline")


# subset by task #######
MDD_bl_read<- MDD_bl %>%
  filter(task_name == "paragraph_reading")
MDD_bl_pic1 <- MDD_bl %>%
  filter(stimulus_filename == "01_WinterLight_Family_in_the_kitchen_web.png")
MDD_bl_pic2 <- MDD_bl %>%
  filter(stimulus_filename == "02_WinterLight_Living_room_web.png")
MDD_bl_pho<- MDD_bl %>%
  filter(task_name == "phonemic_fluency")
MDD_bl_sem<- MDD_bl %>%
  filter(task_name == "semantic_fluency")
MDD_bl_pos <- MDD_bl %>%
  filter(task_name == "positive_fluency")
MDD_bl_jou1 <- MDD_bl %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3")
MDD_bl_jou2 <- MDD_bl %>%
  filter(stimulus_filename == "en_instruction_yesterday.mp3")
MDD_bl_recall <- MDD_bl %>%
  filter(task_name == "paragraph_recall")

# Removing null values 
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

MDD_bl_read <- MDD_bl_read  %>% select(where(not_all_na))
MDD_bl_pic1 <- MDD_bl_pic1  %>% select(where(not_all_na))
MDD_bl_pic2 <- MDD_bl_pic2  %>% select(where(not_all_na))
MDD_bl_pho <- MDD_bl_pho  %>% select(where(not_any_na))
MDD_bl_sem <- MDD_bl_sem  %>% select(where(not_any_na))

# Exclude NA 
MDD_bl_pos <- MDD_bl_pos %>%
  filter(participant_external_id != "TMS004")

MDD_bl_pos <- MDD_bl_pos  %>% select(where(not_any_na))
MDD_bl_jou1 <- MDD_bl_jou1  %>% select(where(not_any_na))
MDD_bl_jou2 <- MDD_bl_jou2  %>% select(where(not_any_na))
MDD_bl_recall <- MDD_bl_recall  %>% select(where(not_all_na))

# para read ########
cor_read <- cor_test(MDD_bl_read[sapply(MDD_bl_read, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_read_p <- cor_read %>%
  filter(p <"0.05")

# pic 1 ########
cor_pic1 <- cor_test(MDD_bl_pic1[sapply(MDD_bl_pic1, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_pic1_p <- cor_pic1 %>%
  filter(p <"0.05")

# pic 2 ########
cor_pic2 <- cor_test(MDD_bl_pic2[sapply(MDD_bl_pic2, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_pic2_p <- cor_pic2 %>%
  filter(p <"0.05")

# phonemic fluency ########
cor_pho <- cor_test(MDD_bl_pho[sapply(MDD_bl_pho, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_pho_p <- cor_pho %>%
  filter(p <"0.05")

# semantic fluency ########
cor_sem <- cor_test(MDD_bl_sem[sapply(MDD_bl_sem, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_sem_p <- cor_sem %>%
  filter(p <"0.05")

# positive fluency ########
cor_pos <- cor_test(MDD_bl_pos[sapply(MDD_bl_pos, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_pos_p <- cor_pos %>%
  filter(p <"0.05")

# journaling 1 ########
cor_jou1 <- cor_test(MDD_bl_jou1[sapply(MDD_bl_jou1, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_jou1_p <- cor_jou1 %>%
  filter(p <"0.05")

# journaling 2 ########
cor_jou2 <- cor_test(MDD_bl_jou2[sapply(MDD_bl_jou2, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_jou2_p <- cor_jou2 %>%
  filter(p <"0.05")

# recall ########
cor_rec <- cor_test(MDD_bl_recall[sapply(MDD_bl_recall, is.numeric)], vars = "baseline_HAMD", method = "pearson")
cor_rec_p <- cor_rec %>%
  filter(p <"0.05")

