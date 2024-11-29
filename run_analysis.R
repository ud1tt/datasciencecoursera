## run_analysis.R
##
## This script is designed to clean and organize a raw dataset into a tidy format for further analysis.
##
## Input files: x_train.txt, x_test.txt, y_train.txt, y_test.txt, subject_train.txt, subject_test.txt,
##              features.txt, activity_labels.txt
## Output files: tidy_average_data.txt, combined_cleaned_data.txt
##
## Required package: reshape2 (for melt() and dcast() functions)
##
## Usage in R:
## Place this script and the data files in the same directory. Then:
## > source("run_analysis.R")
## > run_analysis()
## OR
## > analysis_result <- run_analysis()
## > analysis_result
## [1] "TRUE"  ## Confirms that the data has been cleaned and a tidy dataset has been created.
##
#########################################################################################

run_analysis <- function() {
  
  #######################################################################################
  ## Step 0: Load necessary package
  ## Check if the required package 'reshape2' is installed, otherwise stop execution.
  #######################################################################################
  
  if (!("reshape2" %in% rownames(installed.packages()))) {
    stop("Please install the required package: reshape2!\n")
  }
  
  library(reshape2)

  #######################################################################################
  ## Step 1: Merge the training and test data
  #######################################################################################

  cat("\nMerging the training and test data...\n")

  ## Load training and test data, and combine them
  train_data <- read.table("./train/X_train.txt")
  test_data <- read.table("./test/X_test.txt")
  combined_data <- rbind(train_data, test_data)
  
  ## Verify dimensions
  dim(train_data)  ## (7352, 561)
  dim(test_data)   ## (2947, 561)
  dim(combined_data)  ## (10299, 561)

  ## Load and merge activity labels
  train_labels <- read.table("./train/y_train.txt")
  test_labels <- read.table("./test/y_test.txt")
  combined_labels <- rbind(train_labels, test_labels)
  
  ## Verify dimensions
  dim(train_labels)  ## (7352, 1)
  dim(test_labels)   ## (2947, 1)
  dim(combined_labels)  ## (10299, 1)

  ## Load and merge subject data
  train_subjects <- read.table("./train/subject_train.txt")
  test_subjects <- read.table("./test/subject_test.txt")
  combined_subjects <- rbind(train_subjects, test_subjects)

  ## Verify dimensions
  dim(train_subjects)  ## (7352, 1)
  dim(test_subjects)   ## (2947, 1)
  dim(combined_subjects)  ## (10299, 1)

  #######################################################################################
  ## Step 2: Extract only mean and standard deviation measurements
  #######################################################################################

  cat("\nExtracting measurements for mean and standard deviation...\n")
  
  features <- read.table("features.txt")
  dim(features)  ## (561, 2)

  ## Identify columns with 'mean' or 'std' in their names
  mean_std_indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  length(mean_std_indices)  ## Should return 66
  
  ## Create a new data frame with just the mean and std measurements
  filtered_data <- combined_data[, mean_std_indices]
  dim(filtered_data)  ## (10299, 66)
  
  ## Clean column names
  colnames(filtered_data) <- features[mean_std_indices, 2]
  colnames(filtered_data) <- gsub("\\(|\\)", "", colnames(filtered_data))
  colnames(filtered_data) <- gsub("-", ".", colnames(filtered_data))
  colnames(filtered_data) <- tolower(colnames(filtered_data))

  #######################################################################################
  ## Step 3: Assign descriptive activity names
  #######################################################################################

  cat("\nAssigning descriptive activity names...\n")
  
  activity_labels <- read.table("activity_labels.txt")
  activity_labels[, 2] <- tolower(gsub("_", "", activity_labels[, 2]))
  
  ## Map activity numbers to their descriptive names
  activity_names <- activity_labels[combined_labels[, 1], 2]
  combined_labels[, 1] <- activity_names
  colnames(combined_labels) <- "activity"

  #######################################################################################
  ## Step 4: Label the data with descriptive names
  #######################################################################################

  cat("\nLabeling the data...\n")

  colnames(combined_subjects) <- "subject"
  
  ## Combine all data (subject, activity, and measurements)
  cleaned_data <- cbind(combined_subjects, combined_labels, filtered_data)
  dim(cleaned_data)  ## (10299, 68)
  
  ## Optionally save the combined cleaned data to a file
  write.table(cleaned_data, "combined_cleaned_data.txt")
  
  #######################################################################################
  ## Step 5: Create a tidy dataset with the average of each variable for each subject
  #######################################################################################

  cat("\nCreating the tidy dataset with averages...\n")

  ## Reshape the data using melt() and calculate averages with dcast()
  melted_data <- melt(cleaned_data, id = c("activity", "subject"))
  tidy_data <- dcast(melted_data, activity + subject ~ variable, mean)
  
  ## Save the tidy dataset to a file
  write.table(tidy_data, "tidy_average_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t")
  
  cat("\nData cleaning complete! A tidy dataset has been created.\n")
  return("TRUE")
}

## Run the analysis
run_analysis()
