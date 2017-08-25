
# Reading Data
data_reading <- function(file_name) {
  data <- read.csv(file = file_name, header = TRUE, sep=",");
  return(data)
}
