# set environment
setwd("P:/David/data_science_course/C3_data_clean_progAsgnt")

# Libraries
library(tidyverse)

# Read in data

## activity labels
activity_labels <- 
  read_table(
    file = "activity_labels.txt",
    col_names = c("index", "label"),
    col_types = list(col_integer(), col_character())
  )

## features labels
feature_labels <- 
  read_delim(
    file = "features.txt",
    delim = " ",
    col_names = c("index", "label"),
    col_types = list(col_integer(), col_character())
  )
### subset features for mean and std
feature_labels_subset <- feature_labels[grep("mean\\(\\)|std\\(\\)", feature_labels$label),]
### clean up
rm(feature_labels)

### Tidy feature labels levels
feature_labels_subset <-
  feature_labels_subset %>%
    mutate( label = gsub("mean\\(\\)", "mean", x = label), ## remove the brackets from each 'mean()'
            label = gsub("std\\(\\)", "std", x = label), ## remove the brackets from each 'std()'
            label = gsub("^t", "time_", x = label), ## replace prefix "t" with more informative "time"
            label = gsub("^f", "fft_", x = label), ## replace prefix "f" with more informative "fft"
            label = gsub("-", "_", x = label) ## replace dashes with underscores
    )


## read in subject data
train_subject <- 
  read_lines(              
    file = "train/subject_train.txt"
  )
test_subject <- 
  read_lines( 
    file = "test/subject_test.txt"
  )
### combine training and test
subject <- as.factor(c(train_subject, test_subject))
### clean up
rm(train_subject, test_subject)

## read in vector data
train_vectors <- 
  read_table( 
    file = "train/X_train.txt",
    col_names = F
  ) [,feature_labels_subset$index ]  ## subset for mean and std vectors
test_vectors <- 
  read_table( 
    file = "test/X_test.txt",
    col_names = F
  ) [,feature_labels_subset$index ] ## subset for mean and std vectors
### combine training and test
feature_vectors <- bind_rows(train_vectors, test_vectors)
### clean up
rm(train_vectors, test_vectors)

## read in activity data
train_activity <- 
  read_lines( 
    file = "train/y_train.txt"
  )
test_activity <- 
  read_lines( 
    file = "test/y_test.txt"
  )
### combine training and test
activity <- as.factor(c(train_activity, test_activity))
### clean up
rm(train_activity, test_activity)


# Convert numbers to labels for activity
activity <- factor(activity, labels = activity_labels$label)
rm(activity_labels)

# Rename feature vector columns
colnames(feature_vectors) <- feature_labels_subset$label
rm(feature_labels_subset)

# Combine data plus metadata
complete_data <- bind_cols(tibble(subject, activity), feature_vectors)

means <-
  complete_data %>%
    ## gather melts the data into tidy form
    gather("feature", "value", -subject, -activity, factor_key = T) %>%
    ## group the data for use with summarise
    group_by(subject, activity, feature) %>%
    ## calculate the mean on the groups
    summarise(mean = mean(value))

means

## write means to file
#write.table(
#  x = means,
#  file = "cleaning_data_project.txt",
#  row.names = F)
