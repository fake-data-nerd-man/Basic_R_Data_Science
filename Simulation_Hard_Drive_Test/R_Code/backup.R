
############# START OF CODE ########################
library(plyr)
library(rpart)

# Setting Working Directory
setwd('C:/Users/Kaustav Saha/Desktop/Pre-Sales/STI/Simulation_Hard_Drive_Test/hard-drive-test-data/')

# Reading Data
data_reading <- function() {
  data <- read.csv(file="harddrive.csv",header=TRUE,sep=",");
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
  #df_clean$serial_number <- as.character(df_clean$serial_number)
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


# Creation of a Data-Frame
df_source <- as.data.frame(data);

# Data-Distribution of Failure(1) and    Non-Failure States(0)
table(df_source$failure)
#     0         1 
# 1048509      66 

# Creating Clean Data-Frame
df_clean  <- df_source

# Drop un-necessary Fields
df_clean$date <- NULL
df_clean$serial_number <- NULL
#df_clean$serial_number <- as.character(df_clean$serial_number)
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


##########################

reduced_df <- df_clean

## Count the NA's Analysis
na_count_analysis <- function(data) {
  na_count <<-sapply(data, function(x) sum(length(which(is.na(x)))))
  na_count <<- as.data.frame(na_count)
  return(na_count)
}

## NA_Count_Analysis before Variable Elimination 
print(na_count_analysis(reduced_df))

variable_elimination <- function(missing_factor) {
  reduced_df <<- reduced_df[ lapply ( reduced_df, function(x) sum(is.na(x)) / length(x) ) < missing_factor ]
}

## 50% missing values
variable_elimination(0.5)

## NA_Count_Analysis after Variable Elimination 
print(na_count_analysis(reduced_df))

#################################################################

## Basic Imputation # Use MICE
# Replace with functions from MICE

# Basic Missing Value Removal
missing_value_removed_df <- as.data.frame(reduced_df[ , colSums(is.na(reduced_df)) == 0])
print(na_count_analysis(missing_value_removed_df))

#######################################

## Dividing between training and testing data
data_df <- missing_value_removed_df
trainIndex <- sample(1:nrow(data_df), size = round(0.7*nrow(data_df)), replace=FALSE)
training_data_df <- data_df[trainIndex ,]
testing_data_df <- data_df[-trainIndex ,]

##################################################################

## Basic Hyper-Paramter Adjustment for R
model_building <- function(training_data) {
  decision_tree = rpart(failure ~ .,
                        data = training_data, 
                        method = "class",
                        parms = list(split = 'information'), # information | gini
                        maxdepth=5)
  return(decision_tree)
}

model <- model_building(training_data_df)

prediction <- function(model,data) {
  predicted_results <- predict(model, data, type="class")
  return(predicted_results)
}

predicted_results <- prediction(model,testing_data_df)

## Basic Prediction Accuracy checking 

# Now you can calculate the accuracy by dividing the sum diagonal of the 
# matrix - which are the correct predictions - by the total sum of the matrix:

accuracy <- function(data,results) {
  confusion_matrix <- table(data$failure,results)
  accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
  return(accuracy)
}

accuracy_results <- accuracy(testing_data_df,predicted_results)

############# END OF CODE ########################
