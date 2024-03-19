#Prep database 
library(dplyr)
library(readxl)

WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS_2023FEB16.csv")
WL <- WL[grep("^TMS", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook rTMS remote_2023FEB16.csv")
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(ifelse(grepl("^CTC", WL$participant_external_id), "Control",
                                      ifelse(grepl("^TMS", WL$participant_external_id), "MDD", NA)))

# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person")


# Replace values in session_label column
WL$session_label <- gsub("Baseline", "V1", WL$session_label)
WL$session_label <- gsub("Week 2", "V2", WL$session_label)
WL$session_label <- gsub("Week 6", "V3", WL$session_label)

WL_Jou <- subsetTask(WL, "journaling")
WL_Jou_BL <- extractBaseline(WL_Jou)

#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet = "TMS")
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet = "CTRL")
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
demoALL <- bind_rows(demoMDD, demoCTRL)
WL_Jou_BL <- merge(WL_Jou_BL, demoALL, by = "participant_external_id", all.x=TRUE)


# Convert integer columns to numeric
int_cols <- sapply(WL_Jou_BL, is.integer)
WL_Jou_BL[, int_cols] <- lapply(WL_Jou_BL[, int_cols], factor)


# Check for duplicate entries in the sample_id column
if (any(duplicated(WL_Jou_BL$sample_id))) {
  print("There are duplicate entries in the sample_id column.")
} else {
  print("All entries in the sample_id column are unique.")
}

MDD_Jou_BL <- WL_Jou_BL %>% filter(participant_group == "MDD")
#merge HAMD results to MDD data
HAMD <- read.csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
col_index<-which(colnames(HAMD)=="hamd17_total_pre")
colnames(HAMD)[col_index] <-"Baseline_HAMD"
colnames(HAMD)[1] <- "participant_external_id"
MDD_Jou_BL <- merge(MDD_Jou_BL, HAMD[,c("participant_external_id", "Baseline_HAMD")], by = "participant_external_id", all.x = TRUE)
Ctrl_Jou_BL <- WL_Jou_BL %>% filter(participant_group == "Control")

# Separate by subtask
WL_feeling_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_journal_feeling.mp3")
#WL_feeling_BL <- merge(WL_feeling_BL, HAMD[,c("participant_external_id", "Baseline_HAMD")], by = "participant_external_id", all.x = TRUE)
#WL_feeling_BL <- subset(WL_feeling_BL, participant_external_id!= c("CTC023","TMS042"))
#WL_yesterday_BL <- subsetStimulus(WL_Jou_BL, "en_instruction_yesterday.mp3")
#WL_yesterday_BL <- merge(WL_yesterday_BL, HAMD[,c("participant_external_id", "Baseline_HAMD")], by = "participant_external_id", all.x = TRUE)



# Taking out out unwanted columns 
to_exclude <-c("X", "sample_id", "task_name",
               "session_label", "sample-datetime_completed_utc", "stimulus_filename",
               "terminal_state", "terminal_message", "race", "prior_tms_response", 
               "prior_tms", "prior_ect_response", "prior_ect", "prior_t3", "prior_lithium", 
               "prior_ketamine", "prior_anticonv_sufficient", "prior_maoi_sufficient", 
               "prior_3aa_sufficient", "prior_2aa_sufficient", "prior_vortioxetine_sufficient", 
               "prior_trazodone_sufficient", "prior_tca_sufficient", "prior_mirt_sufficient", 
               "prior_bupropion_sufficient", "prior_snri_sufficient", "prior_ssri_sufficient", 
               "yrs_speak_english", "age_learned_english", "primary_language", 
               "handedness","tms_protocol","gender","dob_mo", "dob_yr","sample_datetime_completed_utc")
# exclude TMS042 and CTC023
WL_feeling_BL <-WL_feeling_BL[!(WL_feeling_BL$participant_external_id =="TMS042"),]
WL_feeling_BL <-WL_feeling_BL[!(WL_feeling_BL$participant_external_id =="CTC023"),]
LASSO_feeling<-WL_feeling_BL[,!(names(WL_feeling_BL) %in% to_exclude)]
LASSO_feeling <-LASSO_feeling[,1:(ncol(LASSO_feeling)-78)]
na_cols <- sapply(LASSO_feeling, function(x) all(is.na(x)))
LASSO_feeling <- LASSO_feeling[, !na_cols]
# Identify columns with all zero values
zero_cols <- sapply(LASSO_feeling, function(x) all(x == 0))
# Remove columns with all zero values
LASSO_feeling <- LASSO_feeling[, -which(zero_cols)]
LASSO_feeling$sex[LASSO_feeling$participant_external_id == 'TMS028'] <- "M"


# Calculate number of NAs per column to decide which columns to remove 
no.NAs <- data.frame(variable = colnames(LASSO_feeling), count = numeric(ncol(LASSO_feeling)))
for (col in colnames(LASSO_feeling)) {
  no.NA <- sum(is.na(LASSO_feeling[[col]]))
  no.NAs[no.NAs$variable == col, "count"] <- no.NA
}
no.NAs

# Remove columns with > 1/3 NAs
cols_w_NAs <- c("VERB_imageability", "VERB_familiarity", "VERB_age_of_acquisition",
                    "NOUN_imageability", "NOUN_familiarity", "NOUN_age_of_acquisition", 
                    "MATTR_50", "MATTR_40", "MATTR_30", "MATTR_20")
LASSO_feeling <- LASSO_feeling[, !(colnames(LASSO_feeling) %in% cols_w_NAs)]


# Impute missing values with mean of the column
  # Identify numeric columns
  numeric_cols <- sapply(LASSO_feeling, is.numeric)
  # Replace missing values with mean of column for numeric columns only
  for(i in seq_along(LASSO_feeling)) {
    if(numeric_cols[i] && any(is.na(LASSO_feeling[[i]]))) {
      LASSO_feeling[[i]][is.na(LASSO_feeling[[i]])] <- mean(LASSO_feeling[[i]], na.rm = TRUE)
    }
}


## Load GLMNET ##
library(glmnet)  

#subset outcome variable 
FeelingY<-as.factor(LASSO_feeling$participant_group)
FeelingX<-data.matrix(LASSO_feeling[-1])

# Model 
cv_feeling <- cv.glmnet(x = FeelingX, y = FeelingY, family="binomial",
                        measure="class", nfolds=5, alpha=1)
feelingLambda <-cv_feeling$lambda.min
feelingLambda
plot(cv_feeling)
fit = glmnet(x = FeelingX, y = FeelingY, family="binomial", 
                measure = "class", alpha=1, lambda = feelingLambda)
fit$beta[,1]
coef(fit) #none
y_predicted_feeling <- predict(fit, s = feelingLambda, newx = FeelingX)
plot(y_predicted_feeling, FeelingY)


### Combo of L1 and L2 regularization (elastic net) - alpha = 0.5
# Subset outcome variable 
FeelingY <- as.factor(LASSO_feeling$participant_group)
FeelingX <- data.matrix(LASSO_feeling[-1])
# Model 
cv_feeling <- cv.glmnet(x = FeelingX, y = FeelingY, family = "binomial", 
                        measure = "deviance", nfolds = 5, alpha = 0.5)  # Decrease alpha value
feelingLambda <- cv_feeling$lambda.min
feelingLambda
plot(cv_feeling)
# Fit model using optimal lambda
fit <- glmnet(x = FeelingX, y = FeelingY, family = "binomial", alpha = 0.5,  # Decrease alpha value
              lambda = feelingLambda)
# Get coefficients
coef(fit)
# Get predicted probabilities
y_predicted_feeling <- predict(fit, s = feelingLambda, newx = FeelingX, 
                               type = "response")
# Plot predicted probabilities against true labels
plot(y_predicted_feeling, FeelingY)
# Calculate residuals
residuals_feeling <- as.numeric(FeelingY) - y_predicted_feeling

# Calculate Mean Squared Error (MSE)
mse_feeling <- mean(residuals_feeling^2)

### Ridge  - alpha = 0
# Subset outcome variable 
FeelingY <- as.factor(LASSO_feeling$participant_group)
FeelingX <- data.matrix(LASSO_feeling[-1])
# Model 
cv_feeling <- cv.glmnet(x = FeelingX, y = FeelingY, family = "binomial", 
                        measure = "class", nfolds = 5, alpha = 0)  # Decrease alpha value
feelingLambda <- cv_feeling$lambda.min
feelingLambda
plot(cv_feeling)
# Fit model using optimal lambda
fit <- glmnet(x = FeelingX, y = FeelingY, family = "binomial", alpha = 0,  # Decrease alpha value
              lambda = feelingLambda)
# Get coefficients
coef(fit)
# Get predicted probabilities
y_predicted_feeling <- predict(fit, s = feelingLambda, newx = FeelingX, 
                               type = "response")
# Plot predicted probabilities against true labels
plot(y_predicted_feeling, FeelingY, col = ifelse(FeelingY == "A", "blue", "red"), 
     pch = 20)


####### TEST FROM YOUTUBE ########

# divide into training and testing
library(caret)
index <- createDataPartition(FeelingY, p =.8, list=FALSE, times =1) #function from caret package
train_x <- FeelingX[index,]
test_x <- FeelingX[-index,]
train_y <- FeelingY[index]
test_y <- FeelingY[-index]

#ridge
alpha0.fit <- cv.glmnet(x = train_x, y = train_y, family = "binomial", 
                        measure = "class", nfolds = 5, alpha = 0)  
alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=test_x, type = "response")
plot(test_y, alpha0.predicted)
