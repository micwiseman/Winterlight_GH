
library(readxl)
library(dplyr)
library(readr)
library(tidyr)

## Subset by task (journalling) and by visit (baseline)

# Subset to look at journaling and baseline scores
WL_full_jou <-subset_by_task(WL_demo_psych, "journaling")
WL_full_jou_bl<-subset_by_visit(WL_full_jou, "V1")

## Check missing data 

# Categorizing variables into Clinical, Demographic, and Speech variables
demographic_variables <- c('sex', 'age_screening', 'participant_group', 'age_learned_english', 'testing_location','years_education',"first_language_english")
speech_variables <- c('fundamental_frequency_mean','fundamental_frequency_variance', 'intensity_mean_db','medium_pause_duration',
                    'speech_rate', 'sentiment_dominance', 'sentiment_valence',
                    'sentiment_arousal')

# Creating subsets of data based on these categories
clinical_data <- WL_full_jou_bl[, c("participant_external_id", "hamd17_total_pre", "qids_total_pre")]
demographic_data <- WL_full_jou_bl[, c("participant_external_id", demographic_variables)]
speech_data <- WL_full_jou_bl[, c("participant_external_id", speech_variables)]

speech_missing <- analyze_missing_data(speech_data, "participant_external_id")
write.csv(speech_missing, "speech_missing.csv")


# For clinical data
clinical_missing <- analyze_missing_data(clinical_data, "participant_external_id")
write.csv(clinical_missing, "clinical_missing.csv")
## need hamds for TMS035, TMS052, TMS054, TMS053, TMS055, TMS056, TMS058, TMS059, TMS057, MFB14

# For demographic data
demographic_missing <- analyze_missing_data(demographic_data, "participant_external_id")
write.csv(demographic_missing, "demographic_missing.csv")


## Remove participants with missing speech data (processing errors)

WL_full_jou_bl <- WL_full_jou_bl[WL_full_jou_bl$participant_external_id != "TMS055" &
                                 WL_full_jou_bl$participant_external_id != "TMS053" &
                                 WL_full_jou_bl$participant_external_id != "MFB14", ]


#Linear models in pateints 

library(ggplot2)
library(lmtest)

WL_full_jou_bl$sex <- as.factor(WL_full_jou_bl$sex)
WL_full_jou_bl$testing_location <- as.factor(WL_full_jou_bl$testing_location)
WL_jou_bl_MDD <- WL_full_jou_bl %>% filter(participant_group == "MDD")
WL_jou_bl_MDD_feeling <- WL_jou_bl_MDD %>% filter(stimulus_filename == "en_instruction_journal_feeling.mp3")


results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {
    # Filter out observations where sex is NA
    filtered_data <- WL_jou_bl_MDD_feeling %>%
        filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(hamd17_total_pre))
    
    # Linear regression without interaction term
    formula <- as.formula(paste("hamd17_total_pre ~", s, "+ sex + age_screening + age_learned_english"))
    model <- lm(formula, data = filtered_data)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "hamd17_total_pre", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(filtered_data, aes_string(x = s, y = "hamd17_total_pre")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "hamd17_total_pre"),
             x = s, y = "hamd17_total_pre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))


    plot_name <- paste(s, "hamd17_total_pre_plot", ".png", sep = "")
    ggsave(plot_name, plot, width = 6, height = 4, bg = "white")
}

# Print and save results
print(plots)
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


## Anvocas by group 
library(car)
library(ggpubr)

covariates <- c("sex", "age_learned_english", "age_screening")

# Initialize lists and data frames
plots <- list()
results_ancova <- list()
# Extend the dataframe to include mean and SD for each group
ancova_stats_df <- data.frame(Variable = character(),
                              F_Value = numeric(),
                              Pr_F = numeric(),
                              Df = numeric(),
                              Partial_Eta_Squared = numeric(),
                              Mean_MDD = numeric(),
                              SD_MDD = numeric(),
                              Mean_Control = numeric(),
                              SD_Control = numeric(),
                              stringsAsFactors = FALSE)

# Filter your data
WL_jou_bl_feeling <- WL_full_jou_bl %>% filter(stimulus_filename == "en_instruction_journal_feeling.mp3")

p_values_ancovas <- c()


# Loop through each speech variable
for (s in speech_variables) {
    filtered_data <- WL_jou_bl_feeling[!is.na(WL_jou_bl_feeling$participant_group), ]
    
    formula <- as.formula(paste(s, "~ participant_group +", paste(covariates, collapse = " + ")))
    model <- lm(formula, data = filtered_data)
    
    ancova <- Anova(model, type = "III")
    results_ancova[[s]] <- ancova
    
    p_value <- ancova$`Pr(>F)`[2]
    f_value <- ancova$`F`[2]
    df1 <- ancova$`Df`[1]
    df2 <- ancova$`Df`[6]
    ss_effect <- ancova$`Sum Sq`[2]
    ss_error <- ancova$`Sum Sq`[6]
    partial_eta_squared <- ss_effect / (ss_effect + ss_error)
    
    # Calculate mean and SD for each group
    group_stats <- filtered_data %>%
                   group_by(participant_group) %>%
                   summarise(Mean = mean(get(s), na.rm = TRUE), 
                             SD = sd(get(s), na.rm = TRUE)) %>%
                   arrange(participant_group) # Ensure groups are in the same order
    
    # Append the calculated statistics
    ancova_stats_df <- rbind(ancova_stats_df, data.frame(Variable = s, 
                                                         F_Value = f_value, 
                                                         Pr_F = p_value, 
                                                         Df = paste(df1, df2, sep = "/"),
                                                         Partial_Eta_Squared = partial_eta_squared,
                                                         Mean_MDD = ifelse(any(group_stats$participant_group == "MDD"), group_stats$Mean[group_stats$participant_group == "MDD"], NA),
                                                         SD_MDD = ifelse(any(group_stats$participant_group == "MDD"), group_stats$SD[group_stats$participant_group == "MDD"], NA),
                                                         Mean_Control = ifelse(any(group_stats$participant_group == "Control"), group_stats$Mean[group_stats$participant_group == "Control"], NA),
                                                         SD_Control = ifelse(any(group_stats$participant_group == "Control"), group_stats$SD[group_stats$participant_group == "Control"], NA)))
    
    p_label <- ifelse(is.na(p_value), "p = NA", paste("p =", round(p_value, 5)))

    signif_level <- ifelse(p_value < 0.001, '***', 
                  ifelse(p_value < 0.01, '**', 
                  ifelse(p_value < 0.05, '*', 'ns')))

    
    plot_name <- paste(s, "ANCOVA_plot", sep = "_")
  # Create and display the plot
 upper_limit <- max(filtered_data[[s]], na.rm = TRUE) * 1.1 # Adjust the multiplier as needed

# Change levels of participant_group from "MDD" to "TRD"
filtered_data$participant_group <- factor(filtered_data$participant_group, levels = c("Control", "MDD"))
levels(filtered_data$participant_group)[levels(filtered_data$participant_group) == "MDD"] <- "TRD"

# Now create the plot
bxp <- ggplot(filtered_data, aes(x = participant_group, y = .data[[s]], fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = s, x = "", y = "Score", subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 30), axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), axis.text.y = element_text(size = 30), title = element_text(size = 15)) +
  scale_fill_manual(values = c("Control" = "#CCFFFF", "TRD" = "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "TRD")), annotations = signif_level, map_signif_level = FALSE, textsize = 6) +
  scale_y_continuous(limits = c(NA, upper_limit))

print(bxp)




    p_values_ancovas <- c(p_values_ancovas, p_value)
}

# Print the ANCOVA statistics dataframe and p-values vector
print(ancova_stats_df)
print(p_values_ancovas)

# Print results and display plots
print(results_ancova)
lapply(names(plots), function(x) print(plots[[x]]))

# Save the summary statistics dataframe
write.csv(ancova_stats_df, "SOBP_ANCOVA_summary_statistics.csv", row.names = FALSE)


## BH FDR Correction 
p_values_ancovas <- ancova_stats_df$Pr_F
adjusted_p_values <- p.adjust(p_values_ancovas, method="BH")
# Display the original and adjusted p-values
data.frame(Original_P_Value = p_values_ancovas, Adjusted_P_Value = adjusted_p_values)


#### Demographic info 

library(dplyr)

# Calculate mean and SD of age for each participant group
age_stats <- WL_jou_bl_feeling %>%
  group_by(participant_group) %>%
  summarise(
    mean_age = mean(age_screening, na.rm = TRUE),
    sd_age = sd(age_screening, na.rm = TRUE)
  )

# Calculate percentage of M and F in each participant group
sex_distribution <- WL_jou_bl_feeling %>%
  group_by(participant_group, sex) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = count / sum(count) * 100
  )

# Adjusting previous code for unique IDs and NA participants
# Count unique IDs for each combination of sex and participant group
unique_counts <- WL_jou_bl_feeling %>%
  group_by(participant_group, sex) %>%
  summarise(unique_ids = n_distinct(participant_external_id), .groups = 'drop')

# Filter rows where both sex and participant_group are NA
na_participants <- WL_jou_bl_feeling %>%
  filter(is.na(sex) | is.na(participant_group)) %>%
  distinct(participant_external_id)


# Adjusted code to classify and count IDs by prefix
unique_counts <- WL_jou_bl_feeling %>%
  mutate(prefix = case_when(
    substr(participant_external_id, 1, 3) == "TMS" ~ "TMS",
    substr(participant_external_id, 1, 3) == "MDD" ~ "MDD",
    substr(participant_external_id, 1, 3) == "MFB" ~ "MFB",
    TRUE ~ "Other"  # Handles cases that do not match the above prefixes
  )) %>%
  group_by(participant_group, sex, prefix) %>%
  summarise(unique_ids = n_distinct(participant_external_id), .groups = 'drop')


# Print the results
print(age_stats)
print(sex_distribution)
print(unique_counts)
print(na_participants)

# HAMD (patients)

mean(WL_jou_bl_feeling$hamd17_total_pre, na.rm = TRUE)
sd(WL_jou_bl_feeling$hamd17_total_pre, na.rm = TRUE)

WL_jou_bl_feeling_ctls <-WL_jou_bl_feeling %>% filter(participant_group == "Control")
mean(WL_jou_bl_feeling_ctls$qids_total_pre, na.rm = TRUE)
sd(WL_jou_bl_feeling_ctls$qids_total_pre, na.rm = TRUE)



