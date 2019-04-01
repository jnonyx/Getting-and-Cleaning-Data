  setwd("C:/Users/jnony/Desktop/R Data Science/")
  

  # In order for this R script, 'run_analysis.R', to run you need to have the following:
  # A folder with name 'UCI HAR Dataset' present in the working directory set above and
  # the following data files within that folder
  # - UCI HAR Dataset/activity_labels.txt
  # - UCI HAR Dataset/features.txt
  # - UCI HAR Dataset/test/subject_test.txt
  # - UCI HAR Dataset/test/X_test.txt
  # - UCI HAR Dataset/test/y_test.txt
  # - UCI HAR Dataset/train/subject_train.txt
  # - UCI HAR Dataset/train/X_train.txt
  # - UCI HAR Dataset/train/y_train.txt
  #
  # DAta Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
  
  # Goals
  # 1. Merges the training and the test sets to create one data set.
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  # 3. Uses descriptive activity names to name the activities in the data set
  # 4. Appropriately labels the data set with descriptive variable names.
  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  
  library(dplyr)

  
  # Load files
  
  ## Creates the list of files with paths
  read.table_instructions <- list(

    file = list(
      activity_labels = "UCI HAR Dataset/activity_labels.txt",
      features = "UCI HAR Dataset/features.txt",
      subject_train = "UCI HAR Dataset/train/subject_train.txt",
      y_train = "UCI HAR Dataset/train/y_train.txt",
      X_train = "UCI HAR Dataset/train/X_train.txt",
      subject_test = "UCI HAR Dataset/test/subject_test.txt",
      y_test = "UCI HAR Dataset/test/y_test.txt",
      X_test = "UCI HAR Dataset/test/X_test.txt"
    )
  )
  
  ## Import the files
  data_files <- with(read.table_instructions,
                     Map(read.table,
                         file = file,
                         quote = "", comment.char = "",
                         stringsAsFactors = FALSE))
  

  
  ## Merges the train and test sets
  merged_data <- with(data_files,
                      rbind(cbind(subject_train, y_train, X_train),
                            cbind(subject_test,  y_test,  X_test)))
  
  
  
  
  ##       2. Add 2 to each index to adjust for the two extra column
  ##          in the beginning of the merged data frame, 'subject' and 'activity',
  ##          to create a vector with all target variables indexes.
  ##       3. Extract only the target variables from the merged data frame.
  
  ## Finds target features indexes whose names contain the word "mean" or "std"
  target_features_indexes <- grep("mean\\(\\)|std\\(\\)",
                                  data_files$features[[2]])
  
  ## Add 2 columns for "subject" and "activity"
  target_variables_indexes <- c(1, 2, target_features_indexes + 2)
  
  ## Extracts the target variables to create the target data frame
  target_data <- merged_data[ , target_variables_indexes]
  
  
  
  
  ## Relabel the activities based on the activity_labels such as "Sitting" 
  target_data[[2]] <- factor(target_data[[2]],
                             levels = data_files$activity_labels[[1]],
                             labels = data_files$activity_labels[[2]])
  
  

  
  ## Extract the target variables names
  descriptive_variable_names <- data_files$features[[2]][target_features_indexes]
  
  ## Correct a typo
  descriptive_variable_names <- gsub(pattern = "BodyBody", replacement = "Body",
                                     descriptive_variable_names)
  
  ## Create a tidy data set with appropriate labels 
  tidy_data <- target_data
  names(tidy_data) <- c("subject", "activity", descriptive_variable_names)
  
  
  
  ## Create a dataset with the mean of each column for 'subject' and 'activity'
  tidy_data_summary <- tidy_data %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean)) %>%
    ungroup()
  
  new_names_for_summary <- c(names(tidy_data_summary[c(1,2)]),
                             paste0("Avrg-", names(tidy_data_summary[-c(1, 2)])))
  names(tidy_data_summary) <- new_names_for_summary
  
  ## Save the data frame created as a text file in working directory
  write.table(tidy_data_summary, "tidy_data_summary.txt", row.names = FALSE)
  
  message("The script 'run_analysis.R ran successfully.")