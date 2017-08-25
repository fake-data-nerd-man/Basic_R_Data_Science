
# Clean the environment
rm(list = ls())

start_time <- Sys.time()

############ COMPONENTS OF THE CODE #########################

WORKING_DIRECTORY <- 'C:/Users/Kaustav Saha/Desktop/GitHub/Basic_R_Practice/Simulation_Hard_Drive_Test/R_Production_Style/Control/'

BASE_PATH <- "C:/Users/Kaustav Saha/Desktop/GitHub/Basic_R_Practice/Simulation_Hard_Drive_Test/R_Production_Style/Components/"

## DATA #################################
## Sourcing Code for Reading Data
DATA_PATH              <- "Data/"
DATA_FILE_NAME         <- "harddrive.csv"
FULL_DATA_PATH         <- paste0(BASE_PATH,DATA_PATH,DATA_FILE_NAME)  
###########################################

## Sourcing Code for Reading Data
DATA_READING_PATH      <- "Data_Reading/"
DATA_READING_FILE_NAME <- "Script_Data_Reading.R"
FULL_DATA_READING_PATH <- paste0(BASE_PATH,DATA_READING_PATH,DATA_READING_FILE_NAME)  
source(FULL_DATA_READING_PATH)
###########################################

## Sourcing Code for Cleaning Data
DATA_CLEANING_PATH      <- "Data_Frame_Formation/"
DATA_CLEANING_FILE_NAME <- "Data_Cleaning.R"
FULL_DATA_CLEANING_PATH <- paste0(BASE_PATH,DATA_CLEANING_PATH,DATA_CLEANING_FILE_NAME)  
source(FULL_DATA_CLEANING_PATH)

##############################################

## Sourcing Code for Dimensionality Reduction
DIMENSIONALITY_REDUCTION_PATH      <- "Dimensionality_Reduction/"
DIMENSIONALITY_REDUCTION_FILE_NAME <- "dimensionality_reduction.R"
FULL_DIMENSIONALITY_REDUCTION_PATH <- paste0(BASE_PATH,DIMENSIONALITY_REDUCTION_PATH,DIMENSIONALITY_REDUCTION_FILE_NAME)  
source(FULL_DIMENSIONALITY_REDUCTION_PATH)

#####################################

## Sourcing Code for Splitting the Data into Training and Testing
DATA_SPLIT_PATH      <- "Data_Split/"
DATA_SPLIT_FILE_NAME <- "Train_Test_Split.R"
FULL_DATA_SPLIT_PATH <- paste0(BASE_PATH,DATA_SPLIT_PATH,DATA_SPLIT_FILE_NAME)  
source(FULL_DATA_SPLIT_PATH)

#####################################

## Sourcing Code for Model Building
MODEL_PATH      <- "Model/"
MODEL_FILE_NAME <- "model.R"
FULL_MODEL_PATH <- paste0(BASE_PATH,MODEL_PATH,MODEL_FILE_NAME)  
source(FULL_MODEL_PATH)

## Sourcing Code for Prediction
PREDICTION_PATH      <- "Prediction/"
PREDICTION_FILE_NAME <- "prediction.R"
FULL_PREDICTION_PATH <- paste0(BASE_PATH,PREDICTION_PATH,PREDICTION_FILE_NAME)  
source(FULL_PREDICTION_PATH)

## Sourcing Code for Accuracy
ACCURACY_PATH      <- "Accuracy/"
ACCURACY_FILE_NAME <- "accuracy.R"
FULL_ACCURACY_PATH <- paste0(BASE_PATH,ACCURACY_PATH,ACCURACY_FILE_NAME)  
source(FULL_ACCURACY_PATH)
################ START OF CONTROL CODE ########################

# Setting Working Directory
setwd(WORKING_DIRECTORY)

# Data Read
data <- data_reading(FULL_DATA_PATH)

# Data Clean
df_clean <- data_cleaning(data)

reduced_df <- df_clean

## 50% missing values
reduced_df <- variable_elimination(reduced_df,0.5)

# Removing columns with missing values
missing_value_removed_df <- missing_value_elimination(reduced_df)

####
# Creation of Training and Testing Data
## Can we return 2 dataframes in 1 single return statement
split_training_testing(missing_value_removed_df)

# Creation of the model
model <- model_building(training_data_df)

# Extraction of the Prediction Results
predicted_results <- prediction(model,testing_data_df)

# Now you can calculate the accuracy by dividing the sum diagonal of the 
# matrix - which are the correct predictions - by the total sum of the matrix:
accuracy_results <- accuracy(testing_data_df,predicted_results)

print(accuracy_results)
#### 

############### END OF CONTROL CODE ##########################

end_time <- Sys.time()

time_taken <- end_time - start_time

## Time Taken = 2.248038 mins
print(time_taken)

##############################################################
