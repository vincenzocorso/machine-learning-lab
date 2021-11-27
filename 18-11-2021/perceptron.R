# This function generates a simple dataset
random.dataset = function(rows_number, feature_numbers, threshold) {
  samples = runif(rows_number * feature_numbers, min = 0, max = 10)
  dataset = matrix(samples, ncol = feature_numbers)
  target = ifelse(apply(dataset, 1, sum) >= threshold, 1, -1) # If MARGIN=1 the operation is applied on rows
  non_inputs = rep(1, rows_number)
  return(cbind(target, V0 = non_inputs, dataset))
}

# This function classify an input using the perceptron with the given weights
classify.perceptron = function(input, weights) {
  #return(ifelse(sum(input, weights) >= 0, 1, -1))
  return(sign(sum(input, weights)))
}

# Define the perceptron algorithm
train.perceptron = function(trainset, initial_threshold, learning_rate) {
  weights = c(-initial_threshold, runif(ncol(trainset)-2))
  n = nrow(trainset)
  target = trainset[, 1]
  inputs = trainset[, 2:ncol(trainset)]
  
  misclassified = TRUE
  while(!misclassified) {
    misclassified = FALSE
    for(i in 1:n) {
      prediction = classify.perceptron(inputs[i], weights)
      if(prediction != target[i]) {
        weights = weights + learning_rate * target[i] * inputs[i,]
        misclassified = TRUE
      }
    }
  }
  
  return(weights)
}

# Generate a two-feature dataset, plot its points and the decision boundary
library(ggplot2)
dataset = random.dataset(100, 2, 7.5)
weights = train.perceptron(dataset, 2, 0.1)
m = -weights[2] / weights[3]
q = -weights[1] / weights[3]
qplot(dataset[,3], dataset[,4], colour = factor(dataset[,1]), xlab = "V3", ylab = "V4") + labs(colour = "class") + geom_abline(slope = m, intercept = q)
