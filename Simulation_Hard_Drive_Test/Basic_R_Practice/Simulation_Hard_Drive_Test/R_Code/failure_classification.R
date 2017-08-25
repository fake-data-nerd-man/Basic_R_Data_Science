
# Clean the environment
rm(list = ls())

start_time <- Sys.time()

############# START OF CODE ########################
library(plyr)
library(rpart)

# Setting Working Directory
setwd('C:/Users/Kaustav Saha/Desktop/GitHub/Basic_R/Simulation_Hard_Drive_Test/hard-drive-test-data/')

# Constant Declaration
file_name <- "harddrive.csv"

######### Functions

# Reading Data
data_reading <- function(file_name) {
  data <- read.csv(file = file_name, header = TRUE, sep=",");
  return(data)
}

# Data Cleaning
data_cleaning <- function(data) {
  
  # Creation of a Data-Frame
  df_source <- as.data.frame(data);
  
  # Data-Distribution of Failure(1) and    Non-Failure States(0)
  print(table(df_source$failure))
  #     0         1 
  # 1048509      66 
  
  # Creating Clean Data-Frame
  df_clean  <- df_source
  
  # Drop un-necessary Fields
  df_clean$date <- NULL
  df_clean$serial_number <- NULL
  
  # make the model parameter categorical (character)
  df_clean$model <- as.character(df_clean$model)
  
  # Changing the colnames to more readable format
  colnames(df_clean) <- c("model", 
                          "capacity", "failure", 
                          "1_normalized", "1_raw",
                          "2_normalized", "2_raw",
                          "3_normalized", "3_raw",
                          "4_normalized", "4_raw",
                          "5_normalized", "5_raw",
                          "6_normalized", "6_raw",
                          "7_normalized", "7_raw",
                          "8_normalized", "8_raw",
                          "9_normalized", "9_raw",
                          "10_normalized", "10_raw",
                          "11_normalized", "11_raw",
                          "12_normalized", "12_raw",
                          "13_normalized", "13_raw",
                          "14_normalized", "14_raw",
                          "15_normalized", "15_raw",
                          "16_normalized", "16_raw",
                          "17_normalized", "17_raw",
                          "18_normalized", "18_raw",
                          "19_normalized", "19_raw",
                          "20_normalized", "20_raw",
                          "21_normalized", "21_raw",
                          "22_normalized", "22_raw",
                          "23_normalized", "23_raw",
                          "24_normalized", "24_raw",
                          "25_normalized", "25_raw",
                          "26_normalized", "26_raw",
                          "27_normalized", "27_raw",
                          "28_normalized", "28_raw",
                          "29_normalized", "29_raw",
                          "30_normalized", "30_raw",
                          "31_normalized", "31_raw",
                          "32_normalized", "32_raw",
                          "33_normalized", "33_raw",
                          "34_normalized", "34_raw",
                          "35_normalized", "35_raw",
                          "36_normalized", "36_raw",
                          "37_normalized", "37_raw",
                          "38_normalized", "38_raw",
                          "39_normalized", "39_raw",
                          "40_normalized", "40_raw",
                          "41_normalized", "41_raw",
                          "42_normalized", "42_raw",
                          "43_normalized", "43_raw",
                          "44_normalized", "44_raw",
                          "45_normalized", "45_raw");
  
  return(df_clean)
  ##########################
}

## Count the NA's Analysis
na_count_analysis <- function(data) {
  na_count <<-sapply(data, function(x) sum(length(which(is.na(x)))))
  na_count <<- as.data.frame(na_count)
  return(na_count)
}

## Variable Elimination based on percentage of NA in the data
variable_elimination <- function(data,missing_factor) {
  data <- data [ lapply ( data, function(x) sum(is.na(x)) / length(x) ) < missing_factor ]
  return(data)
}

# EITHER APPROACH
## Basic  Imputation # Use MICE
# Replace with functions from MICE

# OR APPROACH
## Remove columns with missing values
missing_value_elimination <- function(data) {
  missing_value_removed_df <- as.data.frame(data[ , colSums(is.na(data)) == 0])
  return(missing_value_removed_df)
}

## Dividing between training and testing data
split_training_testing <- function(data_df) {
  trainIndex <- sample(1:nrow(data_df), size = round(0.7*nrow(data_df)), replace=FALSE)
  training_data_df <<- data_df[trainIndex ,]
  testing_data_df <<- data_df[-trainIndex ,]
}

## Basic Model Building and Hyper-Paramter Adjustment for R
model_building <- function(training_data) {
  decision_tree <- rpart(failure ~ .,
                        data = training_data, 
                        method = "class",
                        parms = list(split = 'information'), # information | gini
                        maxdepth=5)
  return(decision_tree)
}

## Prediction Results
prediction <- function(model,data) {
  predicted_results <- predict(model, data, type="class")
  return(predicted_results)
}

## Basic Prediction Accuracy checking 
accuracy <- function(data,results) {
  confusion_matrix <- table(data$failure,results)
  accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
  return(accuracy)
}

######################################

##### Control Code

# Data Read
data <- data_reading(file_name)

# Data Clean
df_clean <- data_cleaning(data)

reduced_df <- df_clean

## NA_Count_Analysis before Variable Elimination 
print(na_count_analysis(reduced_df))

## 50% missing values
reduced_df <- variable_elimination(reduced_df,0.5)

## NA_Count_Analysis after Variable Elimination 
print(na_count_analysis(reduced_df))

# Removing columns with missing values
missing_value_removed_df <- missing_value_elimination(reduced_df)

## NA_Count_Analysis after Missing Value Elimination 
print(na_count_analysis(missing_value_removed_df))

# Creation of Training and Testing Data
split_training_testing(missing_value_removed_df)

# Creation of the model
model <- model_building(training_data_df)

# Extraction of the Prediction Results
predicted_results <- prediction(model,testing_data_df)

# Now you can calculate the accuracy by dividing the sum diagonal of the 
# matrix - which are the correct predictions - by the total sum of the matrix:
accuracy_results <- accuracy(testing_data_df,predicted_results)

print(accuracy_results)
#### End of Control Code

end_time <- Sys.time()

time_taken <- end_time - start_time

## Time Taken = 2.248038 mins
print(time_taken)

############# END OF CODE ########################
