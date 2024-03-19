library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)

MDD <- read_csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2022JULY20.csv")
control <- read_csv("~/Lab/Winterlight/WINTERLIGHT_InternalPsychSession2_4MAR2022.csv")

MDD <- MDD %>%
  filter(participant_external_id != "OCD108" & participant_external_id != "OCD109" & participant_external_id != "OCD110" & 
           participant_external_id != "OCD111" & participant_external_id != "OCD112" & participant_external_id != "MDD103" &
           participant_external_id != "MDD104")

MDD$sample_datetime_completed_utc <- as.character(MDD$sample_datetime_completed_utc)


# HAMD scores ######################

library(readxl)
hamd <- read_excel("~/Lab/Winterlight/TMS-Clinical-Data-with-IDs-July29-MW.xlsx")
hamd_bl <- hamd %>%
  filter(timepoint == "baseline")
hamd_wide <- hamd_bl %>%
  select(c(participant_external_id, protocol, BL_Date, BL_hamd, Post_Date, Post_hamd, no_trt_from_bl_post))

MDD <- merge(MDD, hamd_wide)

MDD$Post_hamd <- as.numeric(MDD$Post_hamd)
MDD$BL_hamd <- as.numeric(MDD$BL_hamd)

MDD <- MDD %>%
  rowwise() %>%
  mutate(hamd_change = Post_hamd - BL_hamd)




MDD_bl <- MDD %>%
  filter(session_label == "Baseline")

rTMS <- bind_rows(MDD, control, .id = "cohort")

rTMS$cohort <- ifelse(rTMS$cohort == "1", "MDD", "Control")

rTMS_bl <- rTMS %>%
  filter(session_label != "Week 2" & session_label != "Week 4" & session_label != "Week 6")

table(rTMS_bl$participant_external_id)
# n = 12 controls, n = 10 rTMS baseline

rTMS_bl <- rTMS_bl %>%
  rename("interjections" = "INTJ_->_UH")


# HAMD scores ######################

library(readxl)
hamd <- read_excel("~/Desktop/Winterlight_rTMS/rtms_mdd_clinical.xlsx")

hamd <- hamd %>%
  rename("participant_external_id" = "id")

hamd_long <- read_excel("~/Desktop/Winterlight_rTMS/rtms_mdd_clinical_long.xlsx")

MDD <- merge(MDD, hamd_long)

# jouranling ############
MDD_jou1 <- MDD  %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3") 

MDD_jou1_nounval <- MDD_jou1 %>%
  select(participant_external_id, session_label, NOUN_sentiment_valence)
MDD_jou1_nounval_bl <- MDD_jou1_nounval %>%
  filter(session_label == "Baseline")
MDD_jou1_nounval_bl <-MDD_jou1_nounval_bl %>%
  rename("NOUN_sentiment_valence_bl" = "NOUN_sentiment_valence")
MDD_jou1_nounval_bl <-MDD_jou1_nounval_bl %>%
  select(c(participant_external_id, NOUN_sentiment_valence_bl))
MDD_jou1_nounval_wk4 <- MDD_jou1_nounval %>%
  filter(session_label == "Week 4")
MDD_jou1_nounval_wk4 <-MDD_jou1_nounval_wk4 %>%
  rename("NOUN_sentiment_valence_wk4" = "NOUN_sentiment_valence")
MDD_jou1_nounval_wk4 <-MDD_jou1_nounval_wk4 %>%
  select(c(participant_external_id, NOUN_sentiment_valence_wk4))

MDD_jou1 <- merge(MDD_jou1, MDD_jou1_nounval_bl)
MDD_jou1 <- merge(MDD_jou1, MDD_jou1_nounval_wk4)

MDD_jou1 <- MDD_jou1 %>%
  rowwise() %>%
  mutate(NOUN_sentiment_valence_change = MDD_jou1_nounval_wk4 - MDD_jou1_nounval_bl)


# Picture description task #######
rTMS_bl_pic <- rTMS_bl %>%
  filter(task_name == "picture_description")
rTMS_bl_pic1 <- rTMS_bl_pic %>%
  filter(stimulus_filename == "01_WinterLight_Family_in_the_kitchen_web.png")
rTMS_bl_pic2 <- rTMS_bl_pic %>%
  filter(stimulus_filename == "02_WinterLight_Living_room_web.png")

rTMS_bl_pic2_MDD <- rTMS_bl_pic2 %>%
  filter(cohort == "MDD")

## sentiment valence ##########
pic_noun_sent <- t.test(NOUN_sentiment_valence ~ cohort, data = rTMS_bl_pic); pic_noun_sent
pic_verb_sent <- t.test(VERB_sentiment_valence ~ cohort, data = rTMS_bl_pic); pic_verb_sent

pic1_noun_sent <- t.test(NOUN_sentiment_valence ~ cohort, data = rTMS_bl_pic1); pic1_noun_sent

pic1_noun_sent_bxp <- ggboxplot(rTMS_bl_pic1, x = "cohort", y = "NOUN_sentiment_valence", 
                 title = "Picture 1, noun sentiment valence", 
                 ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=16)) +
  scale_fill_discrete_sequential("Hawaii") +
  theme(legend.position = "none"); pic1_noun_sent_bxp

pic1_noun_sent_stat.test <-  rTMS_bl_pic1 %>% 
  t_test(NOUN_sentiment_valence ~ cohort) %>%
  add_significance(); pic1_noun_sent_stat.test 
pic1_noun_sent_stat.test  <- pic1_noun_sent_stat.test  %>% add_xy_position(x = "cohort")
pic1_noun_sent_bxp  + 
  stat_pvalue_manual(pic1_noun_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(pic1_noun_sent_stat.test, description = NULL, detailed = FALSE))

summary(lm(NOUN_sentiment_valence ~ baseline_HAMD, data = rTMS_bl_pic1))
ggplot(rTMS_bl_pic1, aes(y=NOUN_sentiment_valence, x=baseline_HAMD)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Valence score") + labs(title = "Noun sentiment valence") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 


pic2_noun_sent_bxp <- ggboxplot(rTMS_bl_pic2, x = "cohort", y = "NOUN_sentiment_valence", 
                                title = "Picture description task 2, noun sentiment valence", 
                                ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); pic2_noun_sent_bxp
pic2_noun_sent_stat.test <-  rTMS_bl_pic2 %>% 
  t_test(NOUN_sentiment_valence ~ cohort) %>%
  add_significance(); pic2_noun_sent_stat.test 
pic2_noun_sent_stat.test  <- pic2_noun_sent_stat.test  %>% add_xy_position(x = "cohort")
pic2_noun_sent_bxp  + 
  stat_pvalue_manual(pic2_noun_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(pic2_noun_sent_stat.test, description = NULL, detailed = FALSE))

pic_noun_sent_bxp <- ggboxplot(rTMS_bl_pic, x = "cohort", y = "NOUN_sentiment_valence", 
                                title = "Picture description tasks 1 & 2, noun sentiment valence", 
                                ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); pic_noun_sent_bxp
pic_noun_sent_stat.test <-  rTMS_bl_pic %>% 
  t_test(NOUN_sentiment_valence ~ cohort) %>%
  add_significance(); pic_noun_sent_stat.test 
pic_noun_sent_stat.test  <- pic_noun_sent_stat.test  %>% add_xy_position(x = "cohort")
pic_noun_sent_bxp  + 
  stat_pvalue_manual(pic_noun_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(pic_noun_sent_stat.test, description = NULL, detailed = FALSE))

## speech rate ############
pic_rate <- t.test(speech_rate ~ cohort, data = rTMS_bl_pic); pic_rate # n.s.

## speech frequency ##########
pic_freq <- t.test(fundamental_frequency_mean ~ cohort, data = rTMS_bl_pic); pic_freq # p=0.09

## pic units ############
pic1_units <- t.test(info_units_bool_count ~ cohort, data = rTMS_bl_pic1); pic1_units
pic2_units <- t.test(info_units_bool_count ~ cohort, data = rTMS_bl_pic2); pic2_units # sig, MDD reported more units 


# para recall  #######
rTMS_bl_recall <- rTMS_bl %>%
  filter(task_name == "paragraph_recall")

## sentiment valence ##############
t.test(NOUN_sentiment_valence ~ cohort, data = rTMS_bl_recall) # valence near significant
t.test(NOUN_sentiment_dominance ~ cohort, data = rTMS_bl_recall) # n.s. 

## speech frequency ###########
t.test(fundamental_frequency_mean ~ cohort, data = rTMS_bl_recall) # frequency near significant

## speech rate ################
t.test(speech_rate  ~ cohort, data = rTMS_bl_recall) # n.s.

## units recalled ###############
t.test(paragraph_recall_bool_count_details  ~ cohort, data = rTMS_bl_recall)
recall_units_bxp <- ggboxplot(rTMS_bl_recall, x = "cohort", y = "paragraph_recall_bool_count_details", 
                               title = "Paragraph recall task, distinct details recalled", 
                               ylab = "Distinct details recalled", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); recall_units_bxp

recall_units_stat.test <-  rTMS_bl_recall %>% 
  t_test(paragraph_recall_bool_count_details ~ cohort) %>%
  add_significance(); recall_units_stat.test
recall_units_stat.test <- recall_units_stat.test %>% add_xy_position(x = "cohort")
recall_units_bxp  + 
  stat_pvalue_manual(recall_units_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(recall_units_stat.test, description = NULL, detailed = FALSE))

t.test(paragraph_recall_count_details  ~ cohort, data = rTMS_bl_recall)
raw_recall_units_bxp <- ggboxplot(rTMS_bl_recall, x = "cohort", y = "paragraph_recall_count_details", 
                              title = "Paragraph recall task, raw number of details recalled", 
                              ylab = "Raw number of details recalled", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); raw_recall_units_bxp
raw_recall_units_stat.test <-  rTMS_bl_recall %>% 
  t_test(paragraph_recall_count_details ~ cohort) %>%
  add_significance(); raw_recall_units_stat.test
raw_recall_units_stat.test <- raw_recall_units_stat.test %>% add_xy_position(x = "cohort")
raw_recall_units_bxp  + 
  stat_pvalue_manual(raw_recall_units_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(raw_recall_units_stat.test, description = NULL, detailed = FALSE))


# positive fluency #######
rTMS_bl_sem <- rTMS_bl %>%
  filter(task_name == "semantic_fluency")

## speech rate ############
t.test(speech_rate ~ cohort, data = rTMS_bl_sem)


## speech frequency ############
t.test(fundamental_frequency_mean ~ cohort, data = rTMS_bl_sem)

sem_freq_bxp <- ggboxplot(rTMS_bl_sem, x = "cohort", y = "fundamental_frequency_mean", 
                                  title = "Semantic fluency task, mean speech frequency", 
                                  ylab = "Mean fundamental frequency, Hz", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); sem_freq_bxp
sem_freq_stat.test <-  rTMS_bl_sem %>% 
  t_test(fundamental_frequency_mean ~ cohort) %>%
  add_significance(); sem_freq_stat.test
sem_freq_stat.test <- sem_freq_stat.test %>% add_xy_position(x = "cohort")
sem_freq_bxp  + 
  stat_pvalue_manual(sem_freq_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(sem_freq_stat.test, description = NULL, detailed = FALSE))


# journaling #############
rTMS_bl_jou1 <- rTMS_bl %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3") 

rTMS_bl_jou_MDD <- rTMS_bl_jou1 %>%
  filter(cohort == "MDD")

## speech frequency ###########
jou_freq_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "fundamental_frequency_mean", 
                          title = "Journaling task, mean speech frequency", 
                          ylab = "Mean fundamental frequency, Hz", xlab = "", add = "jitter", fill = "cohort") +
  theme(legend.position = "none"); jou_freq_bxp
jou_freq_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(fundamental_frequency_mean ~ cohort) %>%
  add_significance(); jou_freq_stat.test
jou_freq_stat.test <- jou_freq_stat.test %>% add_xy_position(x = "cohort")
jou_freq_bxp  + 
  stat_pvalue_manual(jou_freq_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_freq_stat.test, description = NULL, detailed = FALSE))

## sentiment valence #############
t.test(sentiment_valence ~ cohort, data = rTMS_bl_jou1)
jou_sent_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "sentiment_valence", 
                          title = "Journaling, sentiment valence", 
                          ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_bxp

jou_sent_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(sentiment_valence ~ cohort) %>%
  add_significance(); jou_sent_stat.test
jou_sent_stat.test <- jou_sent_stat.test %>% add_xy_position(x = "cohort")
jou_sent_bxp  + 
  stat_pvalue_manual(jou_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$sentiment_valence, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=sentiment_valence, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Valence score") + labs(title = "Journaling, sentiment valence") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

## sentiment dominance #############
t.test(sentiment_dominance ~ cohort, data = rTMS_bl_jou1)
jou_sent_dom_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "sentiment_dominance", 
                          title = "Journaling, sentiment dominance", 
                          ylab = "Dominance score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_dom_bxp
jou_sent_dom_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(sentiment_dominance ~ cohort) %>%
  add_significance(); jou_sent_dom_stat.test
jou_sent_dom_stat.test <- jou_sent_dom_stat.test %>% add_xy_position(x = "cohort")
jou_sent_dom_bxp  + 
  stat_pvalue_manual(jou_sent_dom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_dom_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$sentiment_dominance, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## sentiment arousal #############
t.test(sentiment_arousal ~ cohort, data = rTMS_bl_jou1)
jou_sent_ar_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "sentiment_arousal", 
                              title = "Journaling, sentiment arousal", 
                              ylab = "Arousal score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_dom_bxp
jou_sent_ar_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(sentiment_arousal ~ cohort) %>%
  add_significance(); jou_sent_ar_stat.test
jou_sent_ar_stat.test <- jou_sent_ar_stat.test %>% add_xy_position(x = "cohort")
jou_sent_ar_bxp  + 
  stat_pvalue_manual(jou_sent_ar_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_ar_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$sentiment_arousal, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=sentiment_arousal, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Arousal score") + labs(title = "Journaling, sentiment arousal") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

## min utt length #############
t.test(min_utt_len ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$min_utt_len, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## max utt length #############
t.test(max_utt_len ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$max_utt_len, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## long pause  #############
t.test(long_pause_count_normalized ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$long_pause_count_normalized, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## med pause #############
t.test(medium_pause_count_normalized ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$medium_pause_count_normalized, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## pause length #############
# this one is good
t.test(mean_pause_duration ~ cohort, data = rTMS_bl_jou1)
jou_pause_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "mean_pause_duration", 
                              title = "Journaling, pause duration", 
                              ylab = "Mean pause duration", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_pause_bxp
jou_pause_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(mean_pause_duration ~ cohort) %>%
  add_significance(); jou_pause_stat.test
jou_pause_stat.test <- jou_pause_stat.test %>% add_xy_position(x = "cohort")
jou_pause_bxp  + 
  stat_pvalue_manual(jou_pause_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_pause_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$mean_pause_duration, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=mean_pause_duration, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Mean pause duration") + labs(title = "Journaling, pause duration") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 


## long pause length #############
t.test(long_pause_duration ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$long_pause_duration, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## med pause length #############
t.test(medium_pause_duration ~ cohort, data = rTMS_bl_jou1)
cor.test(rTMS_bl_jou_MDD$medium_pause_duration, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

## intensity variance #############
t.test(intensity_variance~ cohort, data = rTMS_bl_jou1)
jou_iv_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "intensity_variance", 
                           title = "Journaling task, intensity variance", 
                           ylab = "Intensity variance", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size=16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16), title = element_text(size=16)) +
  theme(legend.position = "none"); jou_iv_bxp
jou_iv_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(intensity_variance ~ cohort) %>%
  add_significance(); jou_iv_stat.test
jou_iv_stat.test <- jou_iv_stat.test %>% add_xy_position(x = "cohort")
jou_iv_bxp  + 
  stat_pvalue_manual(jou_iv_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_iv_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$intensity_variance, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))


## intensity variance #############
t.test(intensity_variance~ cohort, data = rTMS_bl_jou1)
jou_ie_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "intensity_mean_energy", 
                        title = "Journaling task, intensity", 
                        ylab = "Intensity", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size=16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16), title = element_text(size=16)) +
  theme(legend.position = "none"); jou_ie_bxp
jou_ie_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(intensity_mean_energy ~ cohort) %>%
  add_significance(); jou_ie_stat.test
jou_ie_stat.test <- jou_ie_stat.test %>% add_xy_position(x = "cohort")
jou_ie_bxp  + 
  stat_pvalue_manual(jou_ie_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_ie_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$intensity_variance, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=intensity_mean_energy, x=baseline_HAMD)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Intensity") + labs(title = "Journaling task, intensity") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size=16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16), title = element_text(size=16)) 



## phonation rate #############
t.test(phonation_rate ~ cohort, data = rTMS_bl_jou1)
jou_pr_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "phonation_rate", 
                        title = "Journaling, phonation rate", 
                        ylab = "Phonation rate", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_pr_bxp
jou_pr_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(phonation_rate ~ cohort) %>%
  add_significance(); jou_pr_stat.test
jou_pr_stat.test <- jou_pr_stat.test %>% add_xy_position(x = "cohort")
jou_pr_bxp  + 
  stat_pvalue_manual(jou_pr_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_pr_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$phonation_rate, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=phonation_rate, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Phonation rate") + labs(title = "Journaling, phonation rate") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 


## speech rate #############
t.test(speech_rate ~ cohort, data = rTMS_bl_jou1)
jou_sr_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "speech_rate", 
                        title = "Journaling, speech rate", 
                        ylab = "Speech rate", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sr_bxp
jou_sr_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(speech_rate ~ cohort) %>%
  add_significance(); jou_sr_stat.test
jou_sr_stat.test <- jou_sr_stat.test %>% add_xy_position(x = "cohort")
jou_sr_bxp  + 
  stat_pvalue_manual(jou_sr_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sr_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$speech_rate, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=speech_rate, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Speech rate") + labs(title = "Journaling, speech rate") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 


## frequency #########
t.test(fundamental_frequency_mean ~ cohort, data = rTMS_bl_jou1) 
jou_f0_bxp <- ggboxplot(rTMS_bl_jou1, x = "cohort", y = "fundamental_frequency_mean", 
                        title = "Journaling, F0", 
                        ylab = "F0, Hz", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_f0_bxp
jou_f0_stat.test <-  rTMS_bl_jou1 %>% 
  t_test(fundamental_frequency_mean ~ cohort) %>%
  add_significance(); jou_f0_stat.test
jou_f0_stat.test <- jou_f0_stat.test %>% add_xy_position(x = "cohort")
jou_f0_bxp  + 
  stat_pvalue_manual(jou_f0_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_f0_stat.test, description = NULL, detailed = FALSE))

cor.test(rTMS_bl_jou_MDD$fundamental_frequency_mean, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))
ggplot(rTMS_bl_jou_MDD, aes(y=fundamental_frequency_mean, x=baseline_HAMD)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "F0, Hz") + labs(title = "Journaling, F0") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 


freq_model <- lm(fundamental_frequency_mean  ~  baseline_HAMD+ sex, data = rTMS_bl_jou_MDD)

cor.test(rTMS_bl_jou_MDD$baseline_HAMD,rTMS_bl_jou_MDD$fundamental_frequency_mean, type = c("pearson"))
summary(lm(baseline_HAMD ~ fundamental_frequency_mean + sex, data = rTMS_bl_jou_MDD))
summary(freq_model)
library(effects)
summary(freq_model)
predict(freq_model)
predtable <- effect(term = "fundamental_frequency_mean", mod = freq_model) %>% as_tibble()
rTMS_bl_jou_MDD %>% 
  ggplot(mapping = aes(x = fundamental_frequency_mean, y = baseline_HAMD)) +
  geom_point(alpha = 0.5) +
  geom_line(data = predtable, mapping = aes(x = fundamental_frequency_mean, y = fit), color = "red") +
  geom_ribbon(data = predtable, mapping = aes(x = fundamental_frequency_mean, y = fit, ymin = lower, ymax = upper), fill = "blue", alpha = 0.25) +
  labs(x = "fundamental_frequency_mean (grand mean centered)", y = "HAMD")

ggplot(rTMS_bl_jou_MDD, aes(y=fundamental_frequency_mean, x=baseline_HAMD)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Frequency") + labs(title = "Journaling task, phonation rate") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size=16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16), title = element_text(size=16)) 


# phonemic fluency ############ 

# positive fluency ############# 
rTMS_bl_pos <- rTMS_bl %>%
  filter(task_name == "positive_fluency")
summary(lm(baseline_HAMD ~ long_pause_count_normalized, data = subset(rTMS_bl_pos, cohort == "MDD")))

ggplot(data = subset(rTMS_bl_pos, cohort == "MDD"), aes(y=mean_pause_duration, x=baseline_HAMD)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Phonation rate") + labs(title = "Journaling task, phonation rate") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size=16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16), title = element_text(size=16)) 



# power analysis ###########
library(effsize)

t.test(sentiment_dominance ~ cohort, data = rTMS_bl_jou1)
cohen.d(rTMS_bl_jou1$sentiment_dominance, rTMS_bl_jou1$cohort, pooled=TRUE, paired=FALSE, na.rm=FALSE, 
        hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)

t.test(long_pause_duration ~ cohort, data = rTMS_bl_jou1)
cohen.d(rTMS_bl_jou1$sentiment_arousal, rTMS_bl_jou1$cohort, pooled=TRUE, paired=FALSE, na.rm=FALSE, 
        hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)


t.test(mean_pause_duration ~ cohort, data = rTMS_bl_jou1)
cohen.d(rTMS_bl_jou1$mean_pause_duration, rTMS_bl_jou1$cohort, pooled=TRUE, paired=FALSE, na.rm=FALSE, 
        hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)



cor.test(rTMS_bl_jou_MDD$sentiment_dominance, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

summary(lm(sentiment_dominance ~ baseline_HAMD, data = rTMS_bl_jou1))





cor.test(rTMS_bl_jou_MDD$mean_pause_duration, rTMS_bl_jou_MDD$baseline_HAMD, type = c("pearson"))

