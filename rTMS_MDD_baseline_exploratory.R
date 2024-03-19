library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)
library(dplyr)

# Read Winterlight task data for rTMS patients and HAMD data 
MDD <- readMDDdata("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2022JULY20.csv")
HAMD <- extractHAMD("~/Lab/Winterlight/rTMS-HAMD.csv")
# Extract baseline HAMD and winterlight data and merge into one df 
MDD_bl <- extractBaseline(MDD)
HAMD_bl <- HAMD %>%
  select('participant_external_id', 'baseline_HAMD')
MDD_bl <- left_join(HAMD_bl, MDD_bl, by = "participant_external_id")
colnames(MDD_bl)[2] <- "baseline_HAMD"

#Subset baseline data by task
MDD_bl_read<- subsetTask(MDD_bl,"paragraph_reading")
MDD_bl_pic1 <- subsetStimulus(MDD_bl,"01_WinterLight_Family_in_the_kitchen_web.png")
MDD_bl_pic2 <- subsetStimulus(MDD_bl,"02_WinterLight_Living_room_web.png")
MDD_bl_pho<- subsetTask(MDD_bl,"phonemic_fluency")
MDD_bl_sem<- subsetTask(MDD_bl,"semantic_fluency")
MDD_bl_pos <- subsetTask(MDD_bl, "positive_fluency")
MDD_bl_jou1 <- subsetStimulus(MDD_bl,"en_instruction_journal_feeling.mp3")
MDD_bl_jou2 <- subsetStimulus(MDD_bl,"en_instruction_yesterday.mp3")
MDD_bl_recall <- subsetTask(MDD_bl, "paragraph_recall")

#Remove null values 
MDD_bl_read <- MDD_bl_read  %>% select(where(not_all_na))
MDD_bl_pic1 <- MDD_bl_pic1  %>% select(where(not_all_na))
MDD_bl_pic2 <- MDD_bl_pic2  %>% select(where(not_all_na))
MDD_bl_pho <- MDD_bl_pho  %>% select(where(not_any_na))
MDD_bl_sem <- MDD_bl_sem  %>% select(where(not_any_na))

# Remove TMS004 positive fluency scores - null 
MDD_bl_pos <- MDD_bl_pos %>%
  filter(participant_external_id != "TMS004")

# Remove null values cntd. 
MDD_bl_pos <- MDD_bl_pos  %>% select(where(not_any_na))
MDD_bl_jou1 <- MDD_bl_jou1  %>% select(where(not_any_na))
MDD_bl_jou2 <- MDD_bl_jou2  %>% select(where(not_any_na))
MDD_bl_recall <- MDD_bl_recall  %>% select(where(not_all_na))

# para read ########
cor_read <- explrCorr(MDD_bl_read)
# pic 1 ########
cor_pic1 <- explrCorr(MDD_bl_pic1)
# pic 2 ########
cor_pic2 <- explrCorr(MDD_bl_pic2)
# phonemic fluency ########
cor_pho <- explrCorr(MDD_bl_pho)
# semantic fluency ########
cor_sem <- explrCorr(MDD_bl_sem)
# positive fluency ########
cor_pos <- explrCorr(MDD_bl_pos)
# journaling 1 ########
cor_jou1 <- explrCorr(MDD_bl_jou1)
# journaling 2 ########
cor_jou2 <- explrCorr(MDD_bl_jou2)
# recall ########
cor_rec <- explrCorr(MDD_bl_recall)


## Explore speech variables that are most reliably associated with HAMD scores 

# Common variables that are sig across journaling tasks 
common_jour <- commonVar(list(cor_jou1, cor_jou2)) 
# Common variables that are sig across pic reading tasks 
common_pic <- commonVar(list(cor_pic1, cor_pic2))
# Common variables that are sig across fluency tasks 
common_flu <- commonVar(list(cor_pho, cor_pos, cor_sem))
# Common variables that are sig in tasks that emphasize content (journaling, picture description, pos fluency)
common_contnt <- commonVar(list(cor_jou1, cor_jou2, cor_pos, cor_pic1, cor_pic2))
# Common variables that are sig in tasks that (maybe) de-emphasize content 
common_acc <- commonVar(list(cor_pho, cor_read, cor_sem))
# Common variables that are sig across all tasks 
common_all <- commonVar(list(cor_jou1, cor_jou2, cor_pho, cor_pic1, cor_pic2, cor_pos, cor_read, cor_rec, cor_sem))

# Ranks most frequent sig variables in journaling tasks 
freq_jour <- freqVars(list(cor_jou1, cor_jou2))
# Ranks most frequent sig variables in pic description tasks 
freq_pic <- freqVars(list(cor_pic1, cor_pic2))
# Ranks most frequent sig variables in fluency tasks  
freq_flu <- freqVars(list(cor_pho, cor_pos, cor_sem))
# Ranks most frequent sig variables in tasks that empahsize content (journaling, picture description, pos fluency)
freq_contnt <- freqVars(list(cor_jou1, cor_jou2, cor_pos, cor_pic1, cor_pic2))
# Ranks most frequent variables that are sig in tasks that (maybe) de-emphasize content
freq_acc <- freqVars(list(cor_pho, cor_read, cor_sem))
# Ranks most freq sig variables across all tasks 
freq_all <- freqVars(list(cor_jou1, cor_jou2, cor_pho, cor_pic1, cor_pic2, cor_pos, cor_read, cor_rec, cor_sem))

