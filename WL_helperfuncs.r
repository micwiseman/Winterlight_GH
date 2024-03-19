## Function to read and preprocess data
speech_read_and_preprocess <- function(file_path, additional_filters = NULL,
                                       participant_id_changes = NULL) {
  data <- read.csv(file_path)
  if (!is.null(additional_filters)) {
    data <- subset(data, additional_filters)
  }

  if (!is.null(participant_id_changes)) {
    for (change in participant_id_changes) {
      data$participant_external_id <- ifelse(change$condition, change$new_value,
                                             data$participant_external_id)
    }
  }

  return(data)
}

## Function to read and process each sheet
demo_read_and_process <- function(sheet_number) {
  file_path <- "~/PhD/Thesis/Data/TMS_CTRL_Demographics.xlsx"
  data <- read_excel(file_path, sheet = sheet_number)
  names(data)[1] <- "participant_external_id"
  if (sheet_number == 2) {
    data$sex <- ifelse(data$sex == 0, "F", "M")
  }
  return(data)
}

## Plots histograms of scores by demographic variable
plot_clinical_scores_by_demographics <- function(data, score_columns, demographic_var = NULL, bins = 30) {
  library(ggplot2)
  library(dplyr)
  library(rlang)  # Ensure rlang is loaded for the sym() function
  
  for (col in score_columns) {
    if (is.numeric(data[[col]])) {
      if (!is.null(demographic_var) && demographic_var %in% names(data)) {
        # Group by demographic variable if it's provided
        aggregated_data <- data %>%
          group_by(participant_external_id, !!sym(demographic_var)) %>%
          summarise(mean_score = mean(!!sym(col), na.rm = TRUE)) %>%
          ungroup()

        p <- ggplot(aggregated_data, aes(x = mean_score, fill = !!sym(demographic_var))) +
          geom_histogram(bins = bins, alpha = 0.7) +
          geom_density(alpha = 0.5) +
          facet_wrap(vars(!!sym(demographic_var)))
      } else {
        # If no demographic variable is provided
        aggregated_data <- data %>%
          group_by(participant_external_id) %>%
          summarise(mean_score = mean(!!sym(col), na.rm = TRUE)) %>%
          ungroup()

        p <- ggplot(aggregated_data, aes(x = mean_score)) +
          geom_histogram(bins = bins, alpha = 0.7) +
          geom_density(alpha = 0.5)
      }

      p <- p + labs(title = paste("Distribution of Mean", col, if (!is.null(demographic_var)) paste("by", demographic_var)),
                    x = paste("Mean", col), y = "Frequency") +
            theme_minimal()

      print(p)
    } else {
      print(paste("Column", col, "is not numeric and will be skipped."))
    }
  }
}

## Function to subset speech variables by task name 
subset_by_task <- function(data, task) {
  task_name <- "task_name"
  # Check if the task column exists in the dataframe
  if (!task_name %in% names(data)) {
    stop("Task column not found in the dataframe")
  }
  
  # Subset the dataframe by the specified task name
  subset_data <- data[data$task_name == task, ]
  
  # Return the subset
  return(subset_data)
}

## Function to subset speech variables by visit number 
subset_by_visit <- function(data, visit) {
  session_label <- "session_label"
  # Check if the task column exists in the dataframe
  if (!session_label %in% names(data)) {
    stop("Session column not found in the dataframe")
  }
  
  # Subset the dataframe by the specified task name
  subset_data <- data[data$session_label == visit, ]
  
  # Return the subset
  return(subset_data)
}

## Function that cleans data base excluding columns not relevant for analysis (all NA/Os/no variance)

clean_dataframe <- function(data, na_threshold = 0.7, variance_threshold = 0.001) {
  # Print the columns where most values are NA
  high_na_columns <- colnames(data)[colMeans(is.na(data)) >= na_threshold]
  cat("Columns with >=", na_threshold * 100, "% NAs:", high_na_columns, "\n")
  # Remove columns where most values are NA
  data <- data[, colMeans(is.na(data)) < na_threshold]
  # Find and print the column names with all values equal to 0 or NA
  zero_or_na_cols <- colnames(data)[apply(data, 2, function(col) all(is.na(col) | col == 0))]
  cat("Columns with all values equal to 0 or NA:", zero_or_na_cols, "\n")
  # Remove columns where all values are 0 or NA
  data <- data[!apply(data, 2, function(col) all(is.na(col) | col == 0))]
  # Select numeric columns only
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  # Calculate the variance for numeric columns
  variance <- apply(data[, numeric_cols, drop = FALSE], 2, function(x) var(x, na.rm = TRUE))
  # Print and remove columns with variance less than or equal to the specified threshold
  low_variance_cols <- names(variance[variance <= variance_threshold])
  cat("Columns with variance <= ", variance_threshold, ":", low_variance_cols, "\n")
  data <- data[, !names(data) %in% low_variance_cols]
  return(data)
}

# Function to create box plots for comparing numeric variables by categorical variables 
create_comparison_boxplots_latest <- function(data, numerical_var, categorical_var) {
  for (cat_var in categorical_var) {
    if (is.factor(data[[cat_var]]) || cat_var == "participant_group") {  # Categorical variables
      for (num_var in numerical_var) {
        p <- ggplot(data, aes_string(x=cat_var, y=num_var)) +
          geom_boxplot() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste(num_var, "by", cat_var))
        print(p)
      }
    }
  }
}

### Funtion to describe missing data
analyze_missing_data <- function(dataframe, participant_id_column) {
  # Check if participant_id_column exists in the dataframe
  if (!participant_id_column %in% names(dataframe)) {
    stop("Participant ID column not found in the dataframe")
  }

  # Initialize the output dataframe with participant IDs and MissingValuesCount
  output_df <- data.frame(ParticipantID = dataframe[[participant_id_column]], 
                          MissingValuesCount = rowSums(is.na(dataframe)),
                          stringsAsFactors = FALSE)

  # Filter out rows with no missing values
  missing_indices <- output_df$MissingValuesCount > 0
  output_with_missing <- output_df[missing_indices, ]

  # Process only rows with missing values
  if (nrow(output_with_missing) > 0) {
    # Add a column with a list of missing columns for each participant
    output_with_missing$MissingColumns <- apply(dataframe[missing_indices, ], 1, function(row) {
      missing_cols <- names(dataframe)[is.na(row)]
      if (length(missing_cols) == 0) {
        return(NA)
      } else {
        return(paste(missing_cols, collapse = ", "))
      }
    })

    # Determine duplicate rows based on the filtered data
    filtered_duplicate_flags <- duplicated(dataframe[missing_indices, ]) | duplicated(dataframe[missing_indices, ], fromLast = TRUE)
    output_with_missing$IsDuplicateRow <- filtered_duplicate_flags
  }

  return(output_with_missing)
}


### Function to create a correlation scatter plots 
create_grouped_scatter_plots <- function(speech_data, clinical_data, target_clinical_vars, group_var = NULL, save_plots = FALSE, save_path = "scatter_plots/") {
  combined_data <- cbind(speech_data, clinical_data[target_clinical_vars])

  # Check and convert group_var to factor
  if (!is.null(group_var) && group_var %in% names(combined_data)) {
    combined_data[[group_var]] <- as.factor(combined_data[[group_var]])
    cat("Grouping by:", group_var, "with levels:", levels(combined_data[[group_var]]), "\n")
  } else {
    group_var <- NULL
    cat("No valid grouping variable provided.\n")
  }

  correlation_matrix <- cor(combined_data[,sapply(combined_data, is.numeric)], use = "complete.obs")

  for (speech_var in names(speech_data)) {
    for (clinical_var in target_clinical_vars) {
      corr_coefficient <- correlation_matrix[speech_var, clinical_var]
      scatter_plot <- ggplot(combined_data, aes_string(x = speech_var, y = clinical_var)) +
        geom_point(aes_string(color = group_var), alpha = 0.4) +
        geom_smooth(method = "lm", se = FALSE) +
        annotate("text", x = Inf, y = Inf, label = paste("r =", round(corr_coefficient, 2)), hjust = 1.1, vjust = 1.1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(angle = 45))

      if (!is.null(group_var)) {
        scatter_plot <- scatter_plot + scale_color_brewer(palette = "Set1") +
                       guides(color = guide_legend(title = group_var))
      }

      # Display the plot
      print(scatter_plot)

      # Optionally, save the plot
      if (save_plots) {
        if (!dir.exists(save_path)) {
          dir.create(save_path)
        }
        ggsave(filename = paste0(save_path, "scatter_plot_", speech_var, "_vs_", clinical_var, "_grouped_by_", group_var, ".png"), plot = scatter_plot, width = 8, height = 6)
      }
    }
  }
}

