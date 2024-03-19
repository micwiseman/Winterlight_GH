library(tidyverse)
library(dplyr)
library(stats)

# Extracts the columns that were significantly correlated with HAMD at baseline 
# Using results from exploratory Pearson correlations
MDD_bl_jou1_sig <-MDD_bl_jou1[,c("participant_external_id", cor_jou1$var2)]
MDD_bl_jou2_sig <-MDD_bl_jou2[,c("participant_external_id", cor_jou2$var2)]
MDD_bl_pic1_sig <-MDD_bl_pic1[,c("participant_external_id", cor_pic1$var2)]
MDD_bl_pic2_sig <-MDD_bl_pic2[,c("participant_external_id", cor_pic2$var2)]
MDD_bl_pho_sig <-MDD_bl_pho[,c("participant_external_id", cor_pho$var2)]
MDD_bl_pos_sig <-MDD_bl_pos[,c("participant_external_id", cor_pos$var2)]
MDD_bl_read_sig <-MDD_bl_read[,c("participant_external_id", cor_read$var2)]
MDD_bl_recall_sig <-MDD_bl_recall[,c("participant_external_id", cor_rec$var2)]
MDD_bl_sem_sig <-MDD_bl_sem[,c("participant_external_id", cor_jou1$sem)]

# Read in control data 
control <- read_csv("~/Lab/Winterlight/WINTERLIGHT_InternalPsychSession2_4MAR2022.csv")

## Performs t-tests comparing control data and baseline MDD data for the speech variables 
## that were sig correlated with HAMD in exploratory Pearson correlations

# Paragraph reading 
control_read<- subsetTask(control,"paragraph_reading")
read_t.test_sig<-baseline_t.test(MDD_bl_read_sig, control_read)

# Pic 1 - family in kitchen 
control_pic1 <- subsetStimulus(control,"01_WinterLight_Family_in_the_kitchen_web.png")
pic1_t.test_sig<-baseline_t.test(MDD_bl_pic1_sig, control_pic1)

# Pic 2 - living room
control_pic2 <- subsetStimulus(control,"02_WinterLight_Living_room_web.png")
pic2_t.test_sig<-baseline_t.test(MDD_bl_pic2_sig, control_pic2)

# Phonemic fluency 
control_pho<- subsetTask(control,"phonemic_fluency")
pho_t.test_sig<-baseline_t.test(MDD_bl_pho_sig, control_pho)

# Semantic fluency
control_sem<- subsetTask(control,"semantic_fluency")
sem_t.test_sig<-baseline_t.test(MDD_bl_sem_sig, control_sem)

# Positive fluency 
control_pos <- subsetTask(control, "positive_fluency")
pos_t.test_sig<-baseline_t.test(MDD_bl_pos_sig, control_pos)

# Journal 1 - how are you feeling?
control_jou1 <- subsetStimulus(control,"en_instruction_journal_feeling.mp3")
jou1_t.test_sig<-baseline_t.test(MDD_bl_jou1_sig, control_jou1)

# Paragraph recall
control_recall <- subsetTask(control, "paragraph_recall")
recall_t.test_sig<-baseline_t.test(MDD_bl_recall_sig, control_recall)


