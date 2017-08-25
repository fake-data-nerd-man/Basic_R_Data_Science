
## Dividing between training and testing data
split_training_testing <- function(data_df) {
  trainIndex <- sample(1:nrow(data_df), size = round(0.7*nrow(data_df)), replace=FALSE)
  training_data_df <<- data_df[trainIndex ,]
  testing_data_df <<- data_df[-trainIndex ,]
}