library(e1071)

# Select a subset of the iris dataset with two class
iris.subset = subset(trainset, select=c("Sepal.Length", "Sepal.Width", "Species"), Species %in% c("setosa","virginica"))

kernels = c("linear", "polynomial", "sigmoid", "radial")
for(k in kernels) {
  # Train the model
  svm.model = svm(Species ~ ., data = iris.subset, kernel = k, cost = 1:1000, scale = FALSE)
  
  # Plot the margin and the separation line, mark the support vectors with blue color
  plot(x=iris.subset$Sepal.Length, y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19, main = k)
  points(iris.subset[svm.model$index, c(1,2)], col="blue", cex=2)
  w = t(svm.model$coefs) %*% svm.model$SV
  c = -svm.model$rho
  abline(a=-c/w[1,2],b=-w[1,1]/w[1,2], col="red", lty=5)
  abline(a=(-c-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
  abline(a=(-c+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
}
