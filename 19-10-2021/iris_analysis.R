library(caret)

# Load the iris dataset
data("iris")

# Determine the size of the dataset
dim(iris)

# Determine the types
sapply(iris, class)

# Determine the nominal classes in the dataset
levels(iris$Species)

# Determine the input space and the output space
x = iris[, 1:4]
y = iris[, 5]

# Analyze Sepal.Length
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
median(iris$Sepal.Length)
range(iris$Sepal.Length)
quantile(iris$Sepal.Length)
hist(iris$Sepal.Length)
boxplot(Sepal.Length ~ Species, data = iris)

# Analyze Sepal.Width
mean(iris$Sepal.Width)
sd(iris$Sepal.Width)
var(iris$Sepal.Width)
min(iris$Sepal.Width)
max(iris$Sepal.Width)
median(iris$Sepal.Width)
range(iris$Sepal.Width)
quantile(iris$Sepal.Width)
hist(iris$Sepal.Width)
boxplot(Sepal.Width ~ Species, data = iris)

# Analyze Petal.Length
mean(iris$Petal.Length)
sd(iris$Petal.Length)
var(iris$Petal.Length)
min(iris$Petal.Length)
max(iris$Petal.Length)
median(iris$Petal.Length)
range(iris$Petal.Length)
quantile(iris$Petal.Length)
hist(iris$Petal.Length)
boxplot(Petal.Length ~ Species, data = iris)

# Analyze Petal.Width
mean(iris$Petal.Width)
sd(iris$Petal.Width)
var(iris$Petal.Width)
min(iris$Petal.Width)
max(iris$Petal.Width)
median(iris$Petal.Width)
range(iris$Petal.Width)
quantile(iris$Petal.Width)
hist(iris$Petal.Width)
boxplot(Petal.Width ~ Species, data = iris)

# Analyze the distribution of the Species (target variable)
table.iris = table(iris$Species)
pie(table.iris)
barplot(table.iris)
plot(x=iris$Petal.Length, y=iris$Petal.Width, col=iris$Species)
plot(x=iris$Sepal.Length, y=iris$Sepal.Width, col=iris$Species)
plot(y, col=c(4,6,3))

# Univariate analysis: box plot
par(mfrow=c(1, 4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# Univariate analysis: feature plot
featurePlot(x, y, plot="density", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))
