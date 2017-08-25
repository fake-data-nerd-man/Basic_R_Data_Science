
## Basic Prediction Accuracy checking 
accuracy <- function(data,results) {
  confusion_matrix <- table(data$failure,results)
  accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
  return(accuracy)
}