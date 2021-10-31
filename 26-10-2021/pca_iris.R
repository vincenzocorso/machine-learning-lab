library("FactoMineR")
library("factoextra")

# Load the iris dataset
data("iris")

# Select the PCA input dataset
iris.active = iris[, 1:4]

iris.pca <- PCA(iris.active, graph = TRUE)

# How many components are necessary to explain the variance of the data?
# Get the eigenvalues
iris.eig.val <- get_eigenvalue(iris.pca)
iris.eig.val
# Draw some graphs
fviz_eig(iris.pca, addlabels = TRUE, ylim = c(0, 100))
# R: The first two components explain 95.8% of variance

# Plot all individuals
iris.ind <- get_pca_ind(iris.pca)
iris.ind
fviz_pca_ind(iris.pca,
  axes = c(1,2),
  geom = c("point", "text"),
  col.ind = "cos2",
  ggtheme = theme_minimal()
)

# Filter data by plotting only the top-5 individuals
fviz_pca_ind(iris.pca,
  select.ind = list(cos2 = 5),
  ggtheme = theme_minimal()
)

# Plot all variables
iris.var <- get_pca_var(iris.pca)
iris.var
fviz_pca_var(iris.pca,
  axes = c(1,2),
  geom = c("arrow", "text"),
  ggtheme = theme_minimal()
)
# Petal.Width e Petal.Length are positive related

# Filter data by plotting the 2 top-quality variables
fviz_pca_var(iris.pca,
  select.var = list(cos2 = 2),
  ggtheme = theme_minimal()
)

# Plot top 3 individuals and variables
fviz_pca_biplot(
  iris.pca,
  axes = c(1,2),
  select.ind = list(cos2 = 3),
  select.var = list(cos2 = 3),
  geom.ind = c("point", "text"),
  geom.var = c("arrow", "text"),
  col.ind = iris$Species,
  palette = c("red", "blue", "green"),
  legend.title = "Species",
  ggtheme = theme_minimal()
)
