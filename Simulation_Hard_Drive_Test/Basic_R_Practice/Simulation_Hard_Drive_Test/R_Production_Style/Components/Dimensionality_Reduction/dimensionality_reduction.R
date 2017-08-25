
## Variable Elimination based on percentage of NA in the data
variable_elimination <- function(data,missing_factor) {
  data <- data [ lapply ( data, function(x) sum(is.na(x)) / length(x) ) < missing_factor ]
  return(data)
}

## Remove columns with missing values
missing_value_elimination <- function(data) {
  missing_value_removed_df <- as.data.frame(data[ , colSums(is.na(data)) == 0])
  return(missing_value_removed_df)
}
