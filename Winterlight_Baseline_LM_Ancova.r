
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


#Linear models in patients 

library(ggplot2)
library(lmtest)
library(sjPlot)


speech_by_hamd <- function(data, speech_variable, axis_labels = list(x = "Dominance Score", y = "HAMD")) {
  # Convert necessary columns to factors
  data$sex <- as.factor(data$sex)
  data$testing_location <- as.factor(data$testing_location)
  
  # Filter the data for MDD group and specific stimulus filename
  filtered_data <- data %>%
    filter(participant_group == "MDD", 
           stimulus_filename == "en_instruction_journal_feeling.mp3",
           !is.na(sex), 
           !is.na(!!sym(speech_variable)), 
           !is.na(hamd17_total_pre))
  
  # Linear regression without interaction term
  formula <- as.formula(paste("hamd17_total_pre ~", speech_variable, "+ sex + age_screening + age_learned_english"))
  model <- lm(formula, data = filtered_data)
  
  # Model summary
  model_summary <- summary(model)
  
  # Extract the coefficient and p-value for the speech variable
  estimate <- model_summary$coefficients[2, "Estimate"]
  p_value <- model_summary$coefficients[2, "Pr(>|t|)"]
  
  # Create plot with ggplot2
  plot <- ggplot(filtered_data, aes_string(x = speech_variable, y = "hamd17_total_pre")) +
    geom_point(color = "#800099", size = 2) +  # Dark red color for points
    geom_smooth(method = "lm", se = TRUE, color = "#800080", fill = "#DDA0DD", size = 1.5) +
    labs(
      title = paste("β =", round(estimate, 2), ", p =", round(p_value, 2)),
      x = axis_labels$x, 
      y = axis_labels$y
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black")
    )
  print(model_summary)
  print(plot)
}

library(car)
library(ggpubr)
library(lmtest)

# Function to check linear regression assumptions
check_regression_assumptions <- function(model) {
  # Ensure the input model is of class 'lm'
  if (!inherits(model, "lm")) {
    stop("The input model must be of class 'lm'.")
  }
  
  # 1. Linearity - Residuals vs Fitted plot
  linearity_plot <- ggplot(data = model, aes(.fitted, .resid)) +
    geom_point(color = "#800099") +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Fitted values", y = "Residuals") +
    ggtitle("Residuals vs Fitted") +
    theme_minimal()
  
  # 2. Homoscedasticity - Breusch-Pagan test
  bp_test <- bptest(model)
  bp_test_result <- sprintf("Breusch-Pagan test: p-value = %.4f", bp_test$p.value)
  
  # 3. Independence - Durbin-Watson test
  dw_test <- durbinWatsonTest(model)
  dw_test_result <- sprintf("Durbin-Watson test: statistic = %.4f, p-value = %.4f", 
                            dw_test$statistic, dw_test$p)
  
  # 4. Normality - Q-Q plot and Shapiro-Wilk test
  qq_plot <- ggplot(data = model, aes(sample = .resid)) +
    stat_qq(color = "#800099") +
    stat_qq_line(color = "red") +
    ggtitle("Normal Q-Q Plot") +
    theme_minimal()
  
  shapiro_test <- shapiro.test(model$residuals)
  shapiro_test_result <- sprintf("Shapiro-Wilk test: W = %.4f, p-value = %.4f", 
                                 shapiro_test$statistic, shapiro_test$p.value)
  
  
  diag.plots <-plot(model)
  # Print results
  cat("Assumption Checks for Linear Regression:\n")
  cat("---------------------------------------\n")
  cat(bp_test_result, "\n")
  cat(dw_test_result, "\n")
  cat(shapiro_test_result, "\n")
  print(diag.plots)
  
  # Plot diagnostic plots
  plot_list <- list(linearity_plot, qq_plot)
  ggarrange(plotlist = plot_list, ncol = 2, nrow = 1)
}


check_regression_assumptions(lm(hamd17_total_pre ~ fundamental_frequency_mean + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ fundamental_frequency_variance + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ intensity_mean_db + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ speech_rate + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ medium_pause_duration + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ sentiment_arousal + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ sentiment_valence + sex + age_screening + age_learned_english, data = WL_full_jou_bl))
check_regression_assumptions(lm(hamd17_total_pre ~ sentiment_dominance + sex + age_screening + age_learned_english, data = WL_full_jou_bl))

#Winsorize speech vars
if (!require(DescTools)) install.packages("DescTools")
# Load the DescTools package
library(DescTools)
WL_full_jou_bl$fundamental_frequency_mean_wins <- Winsorize(WL_full_jou_bl$fundamental_frequency_mean, probs = c(0.05, 0.95))
WL_full_jou_bl$fundamental_frequency_variance_wins <- Winsorize(WL_full_jou_bl$fundamental_frequency_variance, probs = c(0.05, 0.95))
WL_full_jou_bl$intensity_mean_db_wins <- Winsorize(WL_full_jou_bl$intensity_mean_db, probs = c(0.05, 0.95))
WL_full_jou_bl$speech_rate_wins <- Winsorize(WL_full_jou_bl$speech_rate, probs = c(0.05, 0.95))
WL_full_jou_bl$medium_pause_duration_wins <- Winsorize(WL_full_jou_bl$medium_pause_duration, probs = c(0.05, 0.95))
WL_full_jou_bl$sentiment_arousal_wins <- Winsorize(WL_full_jou_bl$sentiment_arousal, probs = c(0.05, 0.95))
WL_full_jou_bl$sentiment_valence_wins <- Winsorize(WL_full_jou_bl$sentiment_valence, probs = c(0.05, 0.95))
WL_full_jou_bl$sentiment_dominance_wins <- Winsorize(WL_full_jou_bl$sentiment_dominance, probs = c(0.05, 0.95))


speech_by_hamd(WL_full_jou_bl, "fundamental_frequency_mean", axis_labels = list(x = "Hz", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "fundamental_frequency_variance", axis_labels = list(x = "Hz", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "intensity_mean_db", axis_labels = list(x = "dB", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "speech_rate", axis_labels = list(x = "Words per Minute", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "medium_pause_duration", axis_labels = list(x = "Seconds", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_arousal", axis_labels = list(x = "Arousal Score", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_valence", axis_labels = list(x = "Valence Score", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_dominance", axis_labels = list(x = "Dominance Score", y = "HAMD-17"))

#re do lms with winsorized data
speech_by_hamd(WL_full_jou_bl, "fundamental_frequency_mean_wins", axis_labels = list(x = "Hz", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "fundamental_frequency_variance_wins", axis_labels = list(x = "Hz", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "intensity_mean_db_wins", axis_labels = list(x = "dB", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "speech_rate_wins", axis_labels = list(x = "Words per Minute", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "medium_pause_duration_wins", axis_labels = list(x = "Seconds", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_arousal_wins", axis_labels = list(x = "Arousal Score", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_valence_wins", axis_labels = list(x = "Valence Score", y = "HAMD-17"))
speech_by_hamd(WL_full_jou_bl, "sentiment_dominance_wins", axis_labels = list(x = "Dominance Score", y = "HAMD-17"))




#Linear models in pateints with gad7 

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
    filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(gad7_total_pre))
  
  # Linear regression without interaction term
  formula <- as.formula(paste("gad7_total_pre ~", s, "+ sex + age_screening + age_learned_english + testing_location"))
  model <- lm(formula, data = filtered_data)
  
  # Model summary
  model_summary <- summary(model)
  
  # Store the summary result
  results[[paste(s, "gad7_total_pre", sep = "_")]] <- model_summary
  
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
  plot <- ggplot(filtered_data, aes_string(x = s, y = "gad7_total_pre")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = paste(s, "vs", "gad7"),
         x = s, y = "ga7") +
    theme_minimal() +
    theme(plot.title = element_text(size = 17))
  
print(plot)
}

# Print and save results
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


# Function to check assumptions of ANCOVA ---------------------------------

library(car)
library(ggpubr)

check_ancova_assumptions <- function(model, data) {
  # Ensure the input model is of class 'lm'
  if (!inherits(model, "lm")) {
    stop("The input model must be of class 'lm'.")
  }
  
  # Extract the terms from the model
  terms_info <- terms(model)
  response_var <- as.character(attr(terms_info, "variables")[-1][attr(terms_info, "response")])
  variables <- attr(terms_info, "term.labels")
  
  
  # Identify factors and covariates used in the model
  factors <- variables[sapply(variables, function(v) is.factor(data[[v]]) || is.character(data[[v]]))]
  covariates <- variables[!variables %in% factors]
  
  ancova_checks <- list()
  
  for (covariate in covariates) {
    for (factor in factors) {
      cat("Checking interactions between ", factor, " and ", covariate, "\n", sep = "")
      
      # 1. Homogeneity of Regression Slopes
      interaction_term <- paste(factor, covariate, sep = ":")
      interaction_model <- update(model, as.formula(paste(". ~ . +", interaction_term)))
      interaction_test <- summary(interaction_model)
      
      if (interaction_term %in% rownames(interaction_test$coefficients)) {
        interaction_p_value <- interaction_test$coefficients[interaction_term, "Pr(>|t|)"]
        interaction_result <- sprintf("Homogeneity of Slopes (Interaction) Test for %s and %s: p-value = %.4f",
                                      factor, covariate, interaction_p_value)
      } else {
        interaction_result <- sprintf("Interaction term %s not significant.", interaction_term)
      }
      
      # 2. Independence of Covariate and Factor
      if (is.numeric(data[[covariate]])) {
        independence_test <- aov(as.formula(paste(covariate, "~", factor)), data = data)
        independence_test_result <- summary(independence_test)[[1]][["Pr(>F)"]][1]
        independence_result <- sprintf("Independence of Covariate and Factor (ANOVA) Test for %s and %s: p-value = %.4f",
                                       covariate, factor, independence_test_result)
      } else {
        independence_result <- sprintf("Covariate %s is not numeric, skipped independence check for %s.", covariate, factor)
      }
      
      # Collect results
      ancova_checks <- c(ancova_checks, list(interaction_result, independence_result))
      
      # Print the current check results
      cat(interaction_result, "\n")
      cat(independence_result, "\n\n")
    }
  }
  
}


# Function to perform ANCOVAS ---------------------------------------------
library(dplyr)
library(car)
library(ggplot2)
library(ggsignif)


# Function to perform ANCOVA and generate a plot for a single speech variable
perform_ancova_analysis <- function(data, speech_variable, covariates, stimulus_file, y_axis_label) {
  # Check if the required columns exist in the data
  required_columns <- c("stimulus_filename", "participant_group", speech_variable, covariates)
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Error: The following required columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  # Filter the data based on the stimulus file and remove rows with NAs in key columns
  filtered_data <- data %>%
    filter(stimulus_filename == stimulus_file) %>%
    filter(!is.na(participant_group), !is.na(.data[[speech_variable]]))
  
  # Convert participant_group to factor and set desired levels
  filtered_data <- filtered_data %>%
    mutate(participant_group = factor(participant_group, levels = c("Control", "MDD")))
  
  # Create the formula for ANCOVA
  formula <- as.formula(paste(speech_variable, "~ participant_group +", paste(covariates, collapse = " + ")))
  model <- lm(formula, data = filtered_data)
  
  # Perform ANCOVA
  ancova <- Anova(model, type = "III")
  
  # Extract relevant statistics
  p_value <- ancova$`Pr(>F)`[2]
  f_value <- ancova$`F value`[2]
  df1 <- ancova$`Df`[1]
  df2 <- ancova$`Df`[3]
  ss_effect <- ancova$`Sum Sq`[2]
  ss_error <- ancova$`Sum Sq`[3]
  partial_eta_squared <- ss_effect / (ss_effect + ss_error)
  
  # Calculate mean and SD for each group
  group_stats <- filtered_data %>%
    group_by(participant_group) %>%
    summarise(Mean = mean(.data[[speech_variable]], na.rm = TRUE), 
              SD = sd(.data[[speech_variable]], na.rm = TRUE))
  
  # Create a data frame for ANCOVA statistics
  ancova_stats_df <- data.frame(
    Variable = speech_variable,
    F_Value = f_value,
    Pr_F = p_value,
    Df = paste(df1, df2, sep = "/"),
    Partial_Eta_Squared = partial_eta_squared,
    Mean_TRD = ifelse(any(group_stats$participant_group == "MDD"), group_stats$Mean[group_stats$participant_group == "MDD"], NA),
    SD_TRD = ifelse(any(group_stats$participant_group == "MDD"), group_stats$SD[group_stats$participant_group == "MDD"], NA),
    Mean_Control = ifelse(any(group_stats$participant_group == "Control"), group_stats$Mean[group_stats$participant_group == "Control"], NA),
    SD_Control = ifelse(any(group_stats$participant_group == "Control"), group_stats$SD[group_stats$participant_group == "Control"], NA)
  )
  
  # Print ANCOVA statistics
  print(ancova_stats_df)
  
  # Print ANCOVA results
  print(ancova)
  
  # Prepare plot elements
  p_label <- ifelse(is.na(p_value), "p = NA", paste("p =", format(p_value, scientific = FALSE, digits = 2)))
  signif_level <- ifelse(p_value < 0.001, '***', 
                         ifelse(p_value < 0.01, '**', 
                                ifelse(p_value < 0.05, '*', 'ns')))
  upper_limit <- max(filtered_data[[speech_variable]], na.rm = TRUE) * 1.1 # Adjust the multiplier as needed
  
  # Create the plot
  plot <- ggplot(filtered_data, aes(x = participant_group, y = .data[[speech_variable]], fill = participant_group)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(x = "", y = y_axis_label, subtitle = p_label) +
    theme_classic() +
    theme(axis.title.x = element_text(size = 30), axis.text.x = element_text(size = 30),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 20), 
          plot.subtitle = element_text(size = 15)) +
    scale_fill_manual(values = c("Control" = "#CCFFFF", "MDD" = "#D783C6")) +
    theme(legend.position = "none") +
    geom_signif(comparisons = list(c("Control", "MDD")), annotations = signif_level, map_signif_level = FALSE, textsize = 6) +
    scale_y_continuous(limits = c(NA, upper_limit))
  
  print(plot)
}


# Check ANCOVA assumptions for each speech variable -----------------------
# Example usage:
check_ancova_assumptions(lm(hamd17_total_pre ~ fundamental_frequency_mean + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ fundamental_frequency_variance + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ intensity_mean_db + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ medium_pause_duration + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ speech_rate + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ sentiment_valence + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ sentiment_dominance + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)
check_ancova_assumptions(lm(hamd17_total_pre ~ sentiment_arousal + sex + age_screening + age_learned_english+testing_location, data = WL_full_jou_bl), WL_full_jou_bl)


# Perform Ancovas for each speech feature ---------------------------------


perform_ancova_analysis(data = WL_full_jou_bl, "fundamental_frequency_mean", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Fundamental Frequency (Hz)")
perform_ancova_analysis(data = WL_full_jou_bl, "fundamental_frequency_variance", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Fundamental Frequency (Hz)")
perform_ancova_analysis(data = WL_full_jou_bl, "intensity_mean_db", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Intensity (dB")
perform_ancova_analysis(data = WL_full_jou_bl, "medium_pause_duration", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Pause Duration (s)")
perform_ancova_analysis(data = WL_full_jou_bl, "speech_rate", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Words per minute")
perform_ancova_analysis(data = WL_full_jou_bl, "sentiment_valence", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Valence Score")
perform_ancova_analysis(data = WL_full_jou_bl, "sentiment_arousal", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Arousal Score")
perform_ancova_analysis(data = WL_full_jou_bl, "sentiment_dominance", 
                        covariates = c("sex", "age_learned_english", "age_screening", "testing_location"), 
                        stimulus_file = "en_instruction_journal_feeling.mp3", "Dominance Score")


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



