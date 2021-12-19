# Split the iris dataset into trainset and testset
data(iris)
ind = sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]

# Add three columns to the trainset to specify the class of each input
# To simplify the use of the package neural net
trainset$setosa = trainset$Species == "setosa"
trainset$virginica = trainset$Species == "virginica"
trainset$versicolor = trainset$Species == "versicolor"

# Train a neural net with three nodes in the hidden layer 
library(neuralnet)
network = neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden=3)
network$startweights
network$weights
#plot(network)

# Predict the class for each test instance
net.predict = compute(network, testset[,1:4])$net.result
net.prediction = c("versicolor", "virginica", "setosa")[apply(net.predict, 1, which.max)]

# Determine the accuracy of the model
predict.table = table(testset$Species, net.prediction)
predict.table

# Compare the training result using different activation functions
steps = 30
activation.functions = c("logistic", "tanh")
for(a in activation.functions) {
  net.avg.error = 0
  net.avg.epochs = 0
  for(i in 1:steps) {
    network = neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden=4, act.fct = a, stepmax=1e7)
    net.error = network$result.matrix[1]
    net.epochs = network$result.matrix[3]
    net.avg.error = net.avg.error + net.error
    net.avg.epochs = net.avg.epochs + net.epochs
  }
  net.avg.error = net.avg.error / steps
  net.avg.epochs = net.avg.epochs / steps
  cat("Average error using", a, "->", net.avg.error, fill = TRUE)
  cat("Average epochs using", a, "->", net.avg.epochs, fill = TRUE)
}