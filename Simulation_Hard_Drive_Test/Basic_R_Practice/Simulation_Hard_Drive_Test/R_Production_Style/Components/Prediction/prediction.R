
## Prediction Results
prediction <- function(model,data) {
  predicted_results <- predict(model, data, type="class")
  return(predicted_results)
}
