
library(rpart)
## Basic Model Building and Hyper-Paramter Adjustment for R
model_building <- function(training_data) {
  decision_tree <- rpart(failure ~ .,
                         data = training_data, 
                         method = "class",
                         parms = list(split = 'information'), # information | gini
                         maxdepth=5)
  return(decision_tree)
}
