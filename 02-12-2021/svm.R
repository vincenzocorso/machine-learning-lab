library(e1071)

# Split the iris dataset into trainset and testset
data(iris)
ind = sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))
testset = iris[ind == 2,]
trainset = iris[ind == 1,]

# Train the model
svm.model = svm(Species ~ ., data = trainset, kernel = 'linear', cost = 1)

# Predict the labels and create the confusion matrix
svm.pred = predict(svm.model, testset)
svm.table=table(svm.pred, testset$Species)
svm.table

# Print some useful information
print(svm.model)

#### Lecture Notes ####
# To understand how set the cost hyperparameter
# Select a subset of the iris dataset with two class and plot it
iris.subset = subset(trainset, select=c("Sepal.Length", "Sepal.Width", "Species"), Species %in% c("setosa","virginica"))
plot(x=iris.subset$Sepal.Length, y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)

# Train the model and mark the support vectors with blue color
svm.model = svm(Species ~ ., data = iris.subset, kernel = 'linear', cost = 1, scale = FALSE)
points(iris.subset[svm.model$index, c(1,2)], col="blue", cex=2)

# Add a separation line and the margin on the plot
w = t(svm.model$coefs) %*% svm.model$SV
c = -svm.model$rho
abline (a=-c/w[1,2],b=-w[1,1]/w[1,2], col="red", lty=5)
abline(a=(-c-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
abline(a=(-c+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)

# Try with higher cost
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
svm.model = svm(Species ~ ., data=iris.subset, kernel='linear', cost=10000, scale=FALSE)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
w = t(svm.model$coefs) %*% svm.model$SV
c = -svm.model$rho
abline (a=-c/w[1,2],b=-w[1,1]/w[1,2], col="red", lty=5)
abline(a=(-c-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
abline(a=(-c+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)