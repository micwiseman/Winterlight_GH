library(tidyverse) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(colorspace)

# MDD vs Control at baseline ##############################################################################

MDD <- read_csv("~/Desktop/Winterlight_rTMS/WINTERLIGHT_Sunnybrook_rTMS_2022JULY20.csv")
control <- read_csv("~/Desktop/Winterlight_rTMS/WINTERLIGHT_InternalPsychSession2_4MAR2022.csv")

MDD <- MDD %>%
  filter(participant_external_id != "OCD108" & participant_external_id != "OCD109" & participant_external_id != "OCD110" & 
           participant_external_id != "OCD111" & participant_external_id != "OCD112" & participant_external_id != "MDD103" &
           participant_external_id != "MDD104")

MDD$sample_datetime_completed_utc <- as.character(MDD$sample_datetime_completed_utc)

MDD_bl <- MDD %>%
filter(session_label == "Baseline")

rTMS_control <- bind_rows(MDD_bl, control, .id = "cohort")

rTMS_control$cohort <- ifelse(rTMS_control$cohort == "1", "MDD", "Control")

table(rTMS_control$participant_external_id)
# n = 12 controls, n = 21 rTMS baseline


## journaling task #############################################################################

rTMS_control_jou1 <- rTMS_control %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3") 


## sentiment valence ####################
# no outliers removed
t.test(sentiment_valence ~ cohort, data = subset(rTMS_control_jou1))
jou_sent_bxp <- ggboxplot(data = subset(rTMS_control_jou1), x = "cohort", y = "sentiment_valence", 
                          title = "Journaling, sentiment valence", 
                          ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  theme(legend.position = "none"); jou_sent_bxp

jou_sent_stat.test <-  rTMS_control_jou1 %>% 
  t_test(sentiment_valence ~ cohort) %>%
  add_significance(); jou_sent_stat.test
jou_sent_stat.test <- jou_sent_stat.test %>% add_xy_position(x = "cohort")
jou_sent_bxp  + 
  stat_pvalue_manual(jou_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_stat.test, description = NULL, detailed = FALSE))


# outliers removed 
t.test(sentiment_valence ~ cohort, data = subset(rTMS_control_jou1, sentiment_valence > 5 & sentiment_valence < 7))
jou_sent_bxp <- ggboxplot(data = subset(rTMS_control_jou1, sentiment_valence > 5 & sentiment_valence < 7), x = "cohort", y = "sentiment_valence", 
                          title = "Journaling, sentiment valence", 
                          ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  theme(legend.position = "none"); jou_sent_bxp
ggplot_build(jou_sent_bxp)

jou_sent_stat.test <-  rTMS_control_jou1 %>% 
  filter(sentiment_valence > 5 & sentiment_valence < 7) %>%
  t_test(sentiment_valence ~ cohort) %>%
  add_significance(); jou_sent_stat.test
jou_sent_stat.test <- jou_sent_stat.test %>% add_xy_position(x = "cohort")
jou_sent_bxp  + 
  stat_pvalue_manual(jou_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_stat.test, description = NULL, detailed = FALSE))

## sentiment dominance ################################
t.test(sentiment_dominance ~ cohort, data = rTMS_control_jou1)
jou_sent_dom_bxp <- ggboxplot(rTMS_control_jou1, x = "cohort", y = "sentiment_dominance", 
                              title = "Journaling, sentiment dominance", 
                              ylab = "Dominance score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_dom_bxp
jou_sent_dom_stat.test <-  rTMS_control_jou1 %>% 
  t_test(sentiment_dominance ~ cohort) %>%
  add_significance(); jou_sent_dom_stat.test
jou_sent_dom_stat.test <- jou_sent_dom_stat.test %>% add_xy_position(x = "cohort")
jou_sent_dom_bxp  + 
  stat_pvalue_manual(jou_sent_dom_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_dom_stat.test, description = NULL, detailed = FALSE))

## sentiment arousal ################################
t.test(sentiment_arousal ~ cohort, data = rTMS_control_jou1)
jou_sent_dom_bxp <- ggboxplot(rTMS_control_jou1, x = "cohort", y = "sentiment_arousal", 
                              title = "Journaling, sentiment dominance", 
                              ylab = "Arousal score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_dom_bxp
# not significant but pattern of MDD having higher arousal 

## sentiment arousal ################################
rTMS_control_jou1$sentiment_valence <- as.numeric(rTMS_control_jou1$sentiment_valence)
rTMS_control_jou1$sentiment_dominance <- as.numeric(rTMS_control_jou1$sentiment_dominance)
rTMS_control_jou1$sentiment_arousal <- as.numeric(rTMS_control_jou1$sentiment_arousal)

rTMS_control_jou1$val_dom_ar <- (rTMS_control_jou1$sentiment_valence + rTMS_control_jou1$sentiment_dominance +
                                     rTMS_control_jou1$sentiment_arousal)/3

t.test(val_dom_ar ~ cohort, data = rTMS_control_jou1)

jou_comp_bxp <- ggboxplot(data = subset(rTMS_control_jou1), x = "cohort", y = "val_dom_ar", 
                          title = "Journaling, composite", 
                          ylab = "score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  theme(legend.position = "none"); jou_comp_bxp

jou_comp_stat.test <-  rTMS_control_jou1 %>% 
  t_test(val_dom_ar ~ cohort) %>%
  add_significance(); jou_comp_stat.test
jou_comp_stat.test <- jou_comp_stat.test %>% add_xy_position(x = "cohort")
jou_comp_bxp  + 
  stat_pvalue_manual(jou_comp_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_comp_stat.test, description = NULL, detailed = FALSE))

## pauses ####################
t.test(mean_pause_duration ~ cohort, data = rTMS_control_jou1) # significant
t.test(medium_pause_duration ~ cohort, data = rTMS_control_jou1) # significant

## phonation rate ####################
t.test(phonation_rate ~ cohort, data = rTMS_control_jou1) # significant


jou_sent_bxp <- ggboxplot(rTMS_control_jou1, x = "cohort", y = "sentiment_valence", 
                          title = "Journaling, sentiment valence", 
                          ylab = "Valence score", xlab = "", add = "jitter", fill = "cohort") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none"); jou_sent_bxp


jou_sent_stat.test <-  rTMS_control_jou1 %>% 
  t_test(sentiment_valence ~ cohort) %>%
  add_significance(); jou_sent_stat.test
jou_sent_stat.test <- jou_sent_stat.test %>% add_xy_position(x = "cohort")
jou_sent_bxp  + 
  stat_pvalue_manual(jou_sent_stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(jou_sent_stat.test, description = NULL, detailed = FALSE))


# adding HAMD scores ########################################################################################

library(readxl)
hamd <- read_excel("~/Desktop/Winterlight_rTMS/TMS-Clinical-Data-wide.xlsx")
hamd <- hamd %>%
  rename("participant_external_id" = "Participant_ID")

MDD <- merge(MDD, hamd, by = "participant_external_id")

write.csv(MDD, "~/Desktop/Winterlight_rTMS/rTMS_MDD_Winterlight_data_Aug2022.csv")

MDD_baseline_date <- MDD %>%
  filter(session_label == "Baseline" & task_name == "semantic_fluency") %>%
  select(c(participant_external_id, sample_datetime_completed_utc))
MDD_baseline_date <- MDD_baseline_date %>%
  rename("baseline_date" = "sample_datetime_completed_utc")

MDD <- merge(MDD, MDD_baseline_date)

library(lubridate)
MDD$baseline_date <- ymd_hms(MDD$baseline_date)
MDD$sample_datetime_completed_utc <- ymd_hms(MDD$sample_datetime_completed_utc)

MDD$baseline_date <- as.Date(MDD$baseline_date)   
MDD$sample_datetime_completed_utc <- as.Date(MDD$sample_datetime_completed_utc)

MDD <- MDD %>%
  rowwise() %>%
  mutate(time_since_baseline = sample_datetime_completed_utc - baseline_date)

table(MDD$time_since_baseline)

MDD_sessions <- MDD %>%
  select(c(participant_external_id, session_label, time_since_baseline))

MDD$session_in_weeks <- ifelse(MDD$time_since_baseline == 0, "0",
                               ifelse(MDD$time_since_baseline < 20, "2", 
                                      ifelse(MDD$time_since_baseline >=20 & MDD$time_since_baseline <= 31, "4", "6")))

MDD_baseline <- MDD %>%
  filter(session_in_weeks == "0")

MDD_baseline$sentiment_arousal_R <- MDD_baseline$sentiment_arousal*(-1)
MDD_baseline$val_dom_ar <- (MDD_baseline$sentiment_valence + MDD_baseline$sentiment_dominance +
                              MDD_baseline$sentiment_arousal)/3

MDD_baseline$val_dom_ar2 <- (MDD_baseline$sentiment_valence + MDD_baseline$sentiment_dominance +
                              MDD_baseline$sentiment_arousal_R)/3


MDD_baseline$val_dom <- (MDD_baseline$sentiment_valence + MDD_baseline$sentiment_dominance)/2


# HAMD vs speech at baseline ########################################################################################

## journaling task  ##################################################################################################
MDD_bl_jou1 <- MDD_baseline %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3") 

MDD_bl_jou2 <- MDD_baseline %>%
  filter(stimulus_filename == "en_instruction_yesterday.mp3") 

MDD_bl_jou <- MDD_baseline %>%
  filter(task_name == "journaling") 


### sentiment valence ################################################################################################
MDD_bl_jou1_outlier <-MDD_bl_jou1 %>% filter(sentiment_valence > 4)
cor.test(MDD_bl_jou1_outlier$sentiment_valence, MDD_bl_jou1_outlier$BL_hamd, type = c("pearson"))
plot <- ggplot(MDD_bl_jou1_outlier, aes(y=sentiment_valence, x=BL_hamd)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Valence score") + labs(title = "Journaling, sentiment valence") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 
ggplot_build(plot)


### sentiment dominance ################################################################################################
cor.test(MDD_bl_jou1$sentiment_dominance, MDD_bl_jou1$BL_hamd, type = c("pearson"))
ggplot(MDD_bl_jou1, aes(y=sentiment_dominance, x=BL_hamd)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Dominance score") + labs(title = "Journaling, sentiment dominance") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

### sentiment arousal ################################################################################################
cor.test(MDD_bl_jou1$sentiment_arousal, MDD_bl_jou1$BL_hamd, type = c("pearson"))
ggplot(MDD_bl_jou1, aes(y=sentiment_arousal, x=BL_hamd)) + 
  geom_point(color = "#9F2B68") + geom_smooth(method=lm, color = "#702963", fill = "#C3B1E1") + labs(x = "HAM-D") + 
  labs(y = "Arousal score") + labs(title = "Journaling, sentiment arousal") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

### composites ##########################################################################################################
cor.test(MDD_bl_jou1$val_dom_ar2, MDD_bl_jou1$BL_hamd, type = c("pearson"))
ggplot(MDD_bl_jou1, aes(y=val_dom_ar2, x=BL_hamd)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Composite score") + labs(title = "Journaling, valence/dominance/arousal composite") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

cor.test(MDD_bl_jou1$val_dom, MDD_bl_jou1$BL_hamd, type = c("pearson"))
ggplot(MDD_bl_jou1, aes(y=val_dom, x=BL_hamd)) + 
  geom_point() + geom_smooth(method=lm) + labs(x = "HAM-D") + 
  labs(y = "Composite score") + labs(title = "Journaling, valence/dominance composite") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) 

### pauses #########################################################################################################
cor.test(MDD_bl_jou1$mean_pause_duration, MDD_bl_jou1$BL_hamd, type = c("pearson")) # near sig
cor.test(MDD_bl_jou1$medium_pause_duration, MDD_bl_jou1$BL_hamd, type = c("pearson")) # sig

### phonation rate ################################################################################################
cor.test(MDD_bl_jou1$phonation_rate, MDD_bl_jou1$BL_hamd, type = c("pearson")) # sig 


## positive fluency task  ##################################################################################################
MDD_bl_pos <- MDD_baseline %>%
  filter(task_name == "positive_fluency") 

cor.test(MDD_bl_pos$mean_pause_duration, MDD_bl_pos$BL_hamd, type = c("pearson")) #trend?

cor.test(MDD_bl_pos$sentiment_dominance, MDD_bl_pos$BL_hamd, type = c("pearson"))#trend?

cor.test(MDD_bl_pos$sentiment_arousal, MDD_bl_pos$BL_hamd, type = c("pearson")) # trend?

cor.test(MDD_bl_pos$speech_rate, MDD_bl_pos$BL_hamd, type = c("pearson")) # trend?

cor.test(MDD_bl_pos$phonation_rate, MDD_bl_pos$BL_hamd, type = c("pearson")) # trend?

cor.test(MDD_bl_pos$fundamental_frequency_mean, MDD_bl_pos$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pos$NOUN_sentiment_valence, MDD_bl_pos$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pos$VERB_sentiment_dominance, MDD_bl_pos$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pos$max_utt_len, MDD_bl_pos$BL_hamd, type = c("pearson"))  # near sig

cor.test(MDD_bl_pos$long_pause_duration, MDD_bl_pos$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pos$min_utt_len, MDD_bl_pos$BL_hamd, type = c("pearson"))


## picture description task  ##################################################################################################
MDD_bl_pic1 <- MDD_baseline %>%
  filter(stimulus_filename == "01_WinterLight_Family_in_the_kitchen_web.png")

cor.test(MDD_bl_pic1$mean_pause_duration, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$sentiment_arousal, MDD_bl_pic1$BL_hamd, type = c("pearson"))

cor.test(MDD_bl_pic1$speech_rate, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$phonation_rate, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$fundamental_frequency_mean, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$NOUN_sentiment_valence, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$VERB_sentiment_dominance, MDD_bl_pic1$BL_hamd, type = c("pearson")) 

cor.test(MDD_bl_pic1$max_utt_len, MDD_bl_pic1$BL_hamd, type = c("pearson")) # sig

cor.test(MDD_bl_pic1$long_pause_duration, MDD_bl_pic1$BL_hamd, type = c("pearson"))

cor.test(MDD_bl_pic1$min_utt_len, MDD_bl_pic1$BL_hamd, type = c("pearson")) 




# Longitudinal ####################################################################
MDD_scores <- MDD %>%
  select(participant_external_id, session_in_weeks, task_name, stimulus_filename, sentiment_valence, sentiment_dominance, sentiment_arousal, 
         phonation_rate, mean_pause_duration, medium_pause_duration)

MDD_scores_bl <- MDD_scores %>%
  filter(session_in_weeks == "0")
MDD_scores_bl <- MDD_scores_bl %>%
  rename("sentiment_valence_bl" = "sentiment_valence", "sentiment_dominance_bl" = "sentiment_dominance",
         "sentiment_arousal_bl" = "sentiment_arousal", "phonation_rate_bl" = "phonation_rate", 
         "mean_pause_duration_bl" = "mean_pause_duration", "medium_pause_duration_bl" = "medium_pause_duration" )
MDD_scores_bl <- MDD_scores_bl %>%
  select(-c(session_in_weeks))
MDD <- merge(MDD, MDD_scores_bl, all = TRUE)

MDD <- MDD %>%
  rowwise() %>%
  mutate("sentiment_valence_change" = sentiment_valence - sentiment_valence_bl) %>%
  mutate("sentiment_dominance_change" = sentiment_dominance - sentiment_dominance_bl) %>%
  mutate("sentiment_arousal_change" = sentiment_arousal - sentiment_arousal_bl) %>%
  mutate("phonation_rate_change" = phonation_rate - phonation_rate_bl) %>%
  mutate("mean_pause_duration_change" = mean_pause_duration - mean_pause_duration_bl) %>%
  mutate("medium_pause_duration_change" = medium_pause_duration - medium_pause_duration_bl) 

MDD_lv <- MDD %>%
  group_by(participant_external_id) %>%
  slice_max(session_in_weeks) %>%
  filter(session_in_weeks != "0" & session_in_weeks != "2" )

MDD_lv_6wk <- MDD_lv %>%
  filter(session_in_weeks == "6")

MDD_lv_6wk <- MDD_lv_6wk %>%
  rowwise() %>%
  mutate("hamd_change" = `6_wk_hamd` - `BL_hamd`)

MDD_lv_4wk <- MDD_lv %>%
  filter(session_in_weeks == "4")

MDD_lv_4wk <- MDD_lv_4wk %>%
  rowwise() %>%
  mutate("hamd_change" = `4_wk_hamd` - `BL_hamd`)

MDD_lv <- rbind(MDD_lv_6wk, MDD_lv_4wk)

## journaling task  #########################################################################
MDD_lv <- as.data.frame(MDD_lv)
MDD_lv_jou1 <- MDD_lv %>%
  filter(stimulus_filename == "en_instruction_journal_feeling.mp3") 

MDD_lv_jou1_10 <- MDD_lv_jou1  %>%
  filter(participant_external_id == "TMS001" | participant_external_id == "TMS002" | participant_external_id == "TMS003"|
           participant_external_id == "TMS004" | participant_external_id == "TMS005" | participant_external_id == "TMS006"| 
           participant_external_id == "TMS007" | participant_external_id == "TMS008" | participant_external_id == "TMS009"| 
           participant_external_id == "TMS010")
MDD_lv_jou1_10_res <- MDD_lv_jou1_10 %>%
  filter(Tx_response == "0")
  
t.test(MDD_lv_jou1_10_res$sentiment_valence_bl, MDD_lv_jou1_10_res$sentiment_valence, paired=TRUE)

MDD_lv_jou1_10_res <- MDD_lv_jou1_10_res %>%
  select(participant_external_id, sentiment_valence_bl, sentiment_valence)

MDD_lv_jou1_10_res_long <- MDD_lv_jou1_10_res %>%
  pivot_longer(cols = sentiment_valence_bl:sentiment_valence, names_to = "Time", values_to = "sentiment_valence")

MDD_lv_jou1_10_res_long$Time <- ifelse(MDD_lv_jou1_10_res_long$Time == "sentiment_valence_bl", "Pre rTMS", "Post rTMS")

jou_bxp <- ggboxplot(MDD_lv_jou1_10_res_long, x = "Time", y = "sentiment_valence", 
                          title = "Journaling, sentiment valence", 
                          ylab = "Valence score", xlab = "", add = "jitter", fill = "Time") +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=20)) +
  theme(legend.position = "none"); jou_bxp

 
cor.test(MDD_lv_jou1$sentiment_dominance_change, MDD_lv_jou1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_jou1$sentiment_valence_change, MDD_lv_jou1$hamd_change, type = c("pearson")) # sig 
cor.test(MDD_lv_jou1$sentiment_arousal_change, MDD_lv_jou1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_jou1$phonation_rate_change, MDD_lv_jou1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_jou1$mean_pause_duration_change, MDD_lv_jou1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_jou1$medium_pause_duration_change, MDD_lv_jou1$hamd_change, type = c("pearson"))

MDD_lv_jou1_responders
t.test(MDD_lv_jou1$sentiment_valence, MDD_lv_jou1$sentiment_valence_bl, paired=TRUE) # sig 



summary(lm(sentiment_valence ~ hamd_change + sentiment_valence_bl, data = MDD_lv_jou1))
summary(lm(hamd_change ~ sentiment_valence + sentiment_valence_bl, data = MDD_lv_jou1))

t.test(sentiment_valence_change ~ Tx_response, data = MDD_lv_jou1)
t.test(sentiment_dominance_change ~ Tx_response, data = MDD_lv_jou1)
t.test(sentiment_arousal_change ~ Tx_response, data = MDD_lv_jou1)
t.test(phonation_rate_change ~ Tx_response, data = MDD_lv_jou1)
t.test(mean_pause_duration_change ~ Tx_response, data = MDD_lv_jou1)
t.test(medium_pause_duration_change ~ Tx_response, data = MDD_lv_jou1)


# Picture description task #######
MDD_lv_pic1 <- MDD_lv %>%  
  filter(stimulus_filename == "01_WinterLight_Family_in_the_kitchen_web.png")

cor.test(MDD_lv_pic1$sentiment_dominance_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$sentiment_valence_change, MDD_lv_pic1$hamd_change, type = c("pearson")) 
cor.test(MDD_lv_pic1$sentiment_arousal_change, MDD_lv_pic1$hamd_change, type = c("pearson"))# sig 
cor.test(MDD_lv_pic1$phonation_rate_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$mean_pause_duration_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$medium_pause_duration_change, MDD_lv_pic1$hamd_change, type = c("pearson"))

summary(lm(sentiment_valence ~ hamd_change + sentiment_valence_bl, data = MDD_lv_pic1)) # sig but what does this mean
summary(lm(hamd_change ~ sentiment_valence + sentiment_valence_bl, data = MDD_lv_pic1))

t.test(sentiment_valence_change ~ Tx_response, data = MDD_lv_pic1)
t.test(sentiment_dominance_change ~ Tx_response, data = MDD_lv_pic1)
t.test(sentiment_arousal_change ~ Tx_response, data = MDD_lv_pic1)
t.test(phonation_rate_change ~ Tx_response, data = MDD_lv_pic1)
t.test(mean_pause_duration_change ~ Tx_response, data = MDD_lv_pic1)
t.test(medium_pause_duration_change ~ Tx_response, data = MDD_lv_pic1)


MDD_lv_pic1 <- MDD_lv %>%  
  filter(stimulus_filename == "01_WinterLight_Family_in_the_kitchen_web.png")

cor.test(MDD_lv_pic1$sentiment_dominance_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$sentiment_valence_change, MDD_lv_pic1$hamd_change, type = c("pearson")) 
cor.test(MDD_lv_pic1$sentiment_arousal_change, MDD_lv_pic1$hamd_change, type = c("pearson"))# sig 
cor.test(MDD_lv_pic1$phonation_rate_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$mean_pause_duration_change, MDD_lv_pic1$hamd_change, type = c("pearson"))
cor.test(MDD_lv_pic1$medium_pause_duration_change, MDD_lv_pic1$hamd_change, type = c("pearson"))

summary(lm(sentiment_valence ~ hamd_change + sentiment_valence_bl, data = MDD_lv_pic1)) # sig but what does this mean
summary(lm(hamd_change ~ sentiment_valence + sentiment_valence_bl, data = MDD_lv_pic1))

t.test(sentiment_valence_change ~ Tx_response, data = MDD_lv_pic1)
t.test(sentiment_dominance_change ~ Tx_response, data = MDD_lv_pic1)
t.test(sentiment_arousal_change ~ Tx_response, data = MDD_lv_pic1)
t.test(phonation_rate_change ~ Tx_response, data = MDD_lv_pic1)
t.test(mean_pause_duration_change ~ Tx_response, data = MDD_lv_pic1)
t.test(medium_pause_duration_change ~ Tx_response, data = MDD_lv_pic1)

