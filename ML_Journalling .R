## Step 1 of ML approach 
# performing tree based model on baseline data using all features
library(dplyr)
library(readxl)

WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS_2023FEB16.csv")
WL <- WL[grep("^TMS", WL$participant_external_id), ] # excluding OCD patients (incudes TMS patients, MDD
#patients not getting treatment, and baseline FUS/DBS patients **** should only be used for baseline analyses)
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS remote_2023FEB16.csv")
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(ifelse(grepl("^CTC", WL$participant_external_id), "Control", "MDD"))

## Exclude controls with sig qids
WL <- WL[!(WL$participant_external_id == c("CTC006", "CTC039", "CTC042", "CTC045")), ]


# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person")


# Replace values in session_label column
WL$session_label <- gsub("Baseline|Week 2|Week 4|Week 6", "V\\U\\1", 
                         WL$session_label, ignore.case = TRUE)


WL_Jou <- subsetTask(WL, "journaling")
WL_Jou_BL <- extractBaseline(WL_Jou)

#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_Demographics.xlsx")
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/WL_Control_Demographics.xlsx")
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL_Jou_BL <- merge(WL_Jou_BL, demoALL, by = "participant_external_id", all.x=TRUE)


# Check for duplicate entries in the sample_id column
if (any(duplicated(WL_Jou_BL$sample_id))) {
  print("There are duplicate entries in the sample_id column.")
} else {
  print("All entries in the sample_id column are unique.")
}


# Separate by subtask, starting with journalling 
WL_feeling_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_journal_feeling.mp3")

### dataframe cleaning, getting rid of NAs ETC 
# Convert integer columns to numeric
int_cols <- sapply(WL_feeling_BL, is.integer)
WL_feeling_BL[, int_cols] <- lapply(WL_feeling_BL[, int_cols], factor)

#Manually scanned for participants with mostly NAs and remove
WL_feeling_BL <-WL_feeling_BL[!(WL_feeling_BL$participant_external_id =="TMS042"),]
WL_feeling_BL <-WL_feeling_BL[!(WL_feeling_BL$participant_external_id =="CTC023"),]
# Check for columns that are all NAs and remove 
na_cols <- sapply(WL_feeling_BL, function(x) all(is.na(x)))
print(names(na_cols[na_cols]))
WL_feeling_BL <- WL_feeling_BL[, !na_cols]

## Random forest 
library(randomForest)
library(pROC)

# Randomly shuffle the data
set.seed(42) # Set a seed for reproducibility
WL_feeling_BL <- WL_feeling_BL[sample(nrow(WL_feeling_BL)), ]

# Split data into features (X) and target (y)
target_column_index <- WL_feeling_BL$participant_group
X <- WL_feeling_BL[, !colnames(WL_feeling_BL) %in% target_column_index]
y <- WL_feeling_BL[, target_column_index]

# Define the number of folds
num_folds <- 3

# Initialize an empty vector to store the AUC values for each fold
auc_values <- numeric(num_folds)

# Perform 4-fold cross-validation
for (fold in 1:num_folds) {
  # Determine the indices for the current fold
  fold_start <- ((fold - 1) * nrow(WL_feeling_BL) / num_folds) + 1
  fold_end <- fold * nrow(WL_feeling_BL) / num_folds
  
  # Split the data into training and testing sets for the current fold
  X_train <- X[-c(fold_start:fold_end), ]
  y_train <- y[-c(fold_start:fold_end)]
  X_test <- X[fold_start:fold_end, ]
  y_test <- y[fold_start:fold_end]
  
  # Train the random forest model using all features
  rf_model <- randomForest(X_train, y_train, ntree = 100)
  
  # Make predictions on the test set
  y_pred <- predict(rf_model, X_test)
  
  # Calculate the AUC value for the current fold
  auc_values[fold] <- auc(roc(y_test, y_pred))
}

# Calculate the mean AUC value across all folds
mean_auc <- mean(auc_values)

# Print the AUC values for each fold and the mean AUC
cat("AUC values for each fold:", auc_values, "\n")
cat("Mean AUC value:", mean_auc, "\n")