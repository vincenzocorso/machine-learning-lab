library("FactoMineR")
library("factoextra")

# Load the iris dataset
data("iris")

# Select the PCA input dataset
iris.active = iris[, 1:4]

iris.pca <- PCA(iris.active, scale.unit = TRUE, ncp = 5, graph = TRUE)

iris.eig.val <- get_eigenvalue(iris.pca)
iris.eig.val

# How many components are necessary to explain the variance of the data?
# R: Two component (the first) -> 95.8%%
fviz_eig(iris.pca, addlabels = TRUE, ylim = c(0, 100))

# Determine how many components need to be selected to explain most of the variance of the species
var <- get_pca_var(iris.pca)
head(var$coord, 4)

# Filter data by plotting only the top-5 individuals
ind <- get_pca_ind(iris.pca)
ind
fviz_pca_ind(iris.pca,
  select.ind = list(cos2 = 5),
  ggtheme = theme_minimal()
)

# Filter data by plotting the 2 top-quality variables
iris.var <- get_pca_var(iris.pca)
iris.var
fviz_pca_var(iris.pca,
  select.var = list(cos2 = 2),
  ggtheme = theme_minimal()
)
