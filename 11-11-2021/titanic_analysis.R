# Import the dataset from the csv file
dataset = read.csv("dataset.csv", na.strings=c("NA", ""), sep=";")

# Visualizing the dataset, we observe that "Survived" is an integer
# So we cast it to factor as this is a classification problem
# We also cast "PClass", "Sex", "Embarked" which are nominal variables
# We cast "Fare" from chr to double
str(dataset)
dataset$Survived = factor(dataset$Survived)
dataset$Pclass = factor(dataset$Pclass)
dataset$Sex = factor(dataset$Sex)
dataset$Embarked = factor(dataset$Embarked)
dataset$Fare = as.double(dataset$Fare)

# We analyse the dataset
barplot(
  table(dataset$Survived), # Frequencies of survived and perished
  names = c("Perished", "Survived"), # Names of the two categories
  main="Passenger Survival") # Main title

barplot(table(dataset$Sex), names = c("Female", "Male"), main="Passenger Gender")

hist(dataset$Age, breaks=10, main="Passenger Age", xlab = "Age")

barplot(table(dataset$Survived, dataset$Sex),
        legend = c("Perished", "Survived"),
        names = c("Female", "Male"),
        main = "Passenger Survival by Sex",
        col=c("darkblue","red"))

# Split the data in training and test sets
split.data = function(data, proportion = 0.7, seed = 1) {
  set.seed(seed) 
  index = sample(1:nrow(data))
  train = data[index[1:floor(nrow(data) * proportion)], ]
  test = data[index[(ceiling(nrow(data) * proportion) + 1):nrow(data)], ] 
  return(list(train=train, test=test))
}

allset = split.data(dataset, proportion = 0.7, seed = 1)
trainset = allset$train
testset = allset$test

# Analyse frequencies in the training set
table(trainset$Survived)
prop.table(table(trainset$Survived))

# We try to create a trivial classificator (baseline model) which predict "everyone dies"
testset$Prediction = rep(0, nrow(testset)) # Add a column "Prediction" which is filled with zeros
testset$Prediction = factor(testset$Prediction)

# We estimate how good is this baseline model using a confusion matrix
confusion.matrix = table(testset$Survived, testset$Prediction)
confusion.matrix
first_baseline_model_accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)

# We try to create another baseline model which predict "only female survives"
testset$Prediction = ifelse(testset$Sex == 'female', 1, 0)
testset$Prediction = factor(testset$Prediction)

# We estimate how good is this baseline model
confusion.matrix = table(testset$Survived, testset$Prediction)
confusion.matrix
second_baseline_model_accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)

# Now we try to create a classification model using decision trees
# We start creating a tree which use all the variables
# This is an error because some variables are strings (ex. "Name")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
decisionTree = rpart(Survived ~ ., data = trainset, method = "class")
#plot(decisionTree)
#text(decisionTree)
fancyRpartPlot(decisionTree)

# Should use only a subset of input features
decisionTree = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = trainset, method = "class")
#plot(decisionTree)
#text(decisionTree)
fancyRpartPlot(decisionTree)

# Estimate the accuracy of this decision tree
testset$Prediction = predict(decisionTree, testset, type = "class")
confusion.matrix = table(testset$Survived, testset$Prediction)
confusion.matrix
first_tree_model_accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)

# We can extract some useful information
summary(decisionTree)
printcp(decisionTree)
plotcp(decisionTree)

# In order to avoid overfitting, it is better to prune the tree specifing the cp parameter
prunedDecisionTree = prune(decisionTree, cp = 0.035)
fancyRpartPlot(prunedDecisionTree)

# Estimate the accuracy of this pruned tree
testset$Prediction = predict(prunedDecisionTree, testset, type = "class")
confusion.matrix = table(testset$Survived, testset$Prediction)
confusion.matrix
pruned_tree_model_accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)

# Now using Information Gain instead of Gini Index
igDecisionTree = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = trainset, method = "class", parms = list(split = "information"))
fancyRpartPlot(igDecisionTree)
testset$Prediction = predict(igDecisionTree, testset, type = "class")
confusion.matrix = table(testset$Survived, testset$Prediction)
confusion.matrix
ig_tree_model_accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)