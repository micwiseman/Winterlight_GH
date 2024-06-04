## Run Winterlight_Baseline_DataClean.r first 


# Subset to look at journaling (feeling) and baseline scores
WL_full_jou <-subset_by_task(WL_demo_psych, "journaling")
WL_full_jou_bl<-subset_by_visit(WL_full_jou, "V1")
WL_jou_bl_feeling <- WL_full_jou_bl[WL_full_jou_bl$stimulus_filename=="en_instruction_journal_feeling.mp3",]


# Scale each speech variable and calculate their row means ----------------



WL_jou_bl_feeling$scaled_f0_mean <- scale(WL_jou_bl_feeling$fundamental_frequency_mean)
WL_jou_bl_feeling$scaled_f0_var <- scale(WL_jou_bl_feeling$fundamental_frequency_variance)
WL_jou_bl_feeling$scaled_intensity <- scale(WL_jou_bl_feeling$intensity_mean_db)
WL_jou_bl_feeling$scaled_pause <- -(scale(WL_jou_bl_feeling$medium_pause_duration))
WL_jou_bl_feeling$scaled_speech_rate <- scale(WL_jou_bl_feeling$speech_rate)
WL_jou_bl_feeling$scaled_sent_arou <- -(scale(WL_jou_bl_feeling$sentiment_arousal))
WL_jou_bl_feeling$scaled_sent_val <- scale(WL_jou_bl_feeling$sentiment_valence)
WL_jou_bl_feeling$scaled_sent_dom <- scale(WL_jou_bl_feeling$sentiment_dominance)

WL_jou_bl_feeling$speech_composite <- rowMeans(subset(WL_jou_bl_feeling, select = c(scaled_f0_mean, 
                                          scaled_f0_var, scaled_intensity, scaled_pause,
                                          scaled_speech_rate, scaled_sent_arou, scaled_sent_val,
                                          scaled_sent_dom)), na.rm=TRUE)


# Ancova comparing composite score across groups -------------------------


library(car)
library(ggpubr)


filtered_data <- WL_jou_bl_feeling[!is.na(WL_jou_bl_feeling$participant_group), ]
ancova_model <- lm(speech_composite ~ participant_group + age_screening + sex + age_learned_english + testing_location,
               data = filtered_data)
ancova <- Anova(ancova_model, type = "III")
p_value <- ancova$`Pr(>F)`[2]
  

# Determine p-value label
p_label <- ifelse(is.na(p_value), "p = NA", paste("p =", round(p_value, 5)))

# Determine significance level
signif_level <- ifelse(p_value < 0.001, '***', 
                       ifelse(p_value < 0.01, '**', 
                              ifelse(p_value < 0.05, '*', 'ns')))

# Determine upper limit for y-axis
upper_limit <- max(filtered_data$speech_composite, na.rm = TRUE) * 1.1

# Create the plot
bxp <- ggplot(filtered_data, aes(x = participant_group, y = speech_composite, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Speech Composite Scores by Participant Group", x = "", y = "Score", subtitle = p_label) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 30), axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), axis.text.y = element_text(size = 30), title = element_text(size = 15)) +
  scale_fill_manual(values = c("Control" = "#CCFFFF", "MDD" = "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), annotations = signif_level, map_signif_level = FALSE, textsize = 6) +
  scale_y_continuous(limits = c(NA, upper_limit))

# Print the plot
print(bxp)


  
  

# Linear regression comparing composite score to HAMD in patients  --------

  
  
WL_jou_bl_MDD <- WL_jou_bl_feeling %>% filter(participant_group == "MDD")

filtered_data <- WL_jou_bl_MDD %>%
  filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(hamd17_total_pre))
  
lm_model <- lm(hamd17_total_pre ~ speech_composite + sex + age_screening + age_learned_english + testing_location, data = filtered_data)
  
model_summary <- summary(lm_model)

  
  # Create and save plots
plot <- ggplot(filtered_data, aes(x = speech_composite, y = hamd17_total_pre)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Speech Composite x HAMD", x = "Speech Composite", y = "HAMD") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        title = element_text(size = 18))

print(plot)

