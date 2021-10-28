library("FactoMineR")
library("factoextra")

# Import the dataset 'decathlon2' from the 'factoextra' package
data(decathlon2)
dim(decathlon2)

# Get a subset of rows and columns
decathlon2.active <- decathlon2[1:23, 1:10]

# Execute the Principal Component Analysis
res.pca <- PCA(decathlon2.active, scale.unit = TRUE, ncp = 5, graph = TRUE)

# Get the eigenvalues which measure the amount of variation retained by each principal component
# This can be used to decide the number of components to keep.
eig.val <- get_eigenvalue(res.pca)
eig.val

# Another way to visualize the above results
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results, for variables, from the PCA outputs
# $coord are coordinates of the variables to create a scatter plot
# $cos2 represents the quality of representation for variables on the factor map.
# $contrib contains the contributions (in percentage) of the variables to the principal components.
var <- get_pca_var(res.pca)
var

# Show the correlation between a variable and the first two components.
fviz_pca_var(res.pca, col.var = "black")

# Extract the results, for individuals, from the PCA outputs
ind <- get_pca_ind(res.pca)
ind

# Plot the individuals and color each by their cos2 values
# A high cos2 indicates a good representation of the individual on the principal component
# A low cos2 indicates that the individual is not perfectly represented by the PCs.
fviz_pca_ind(res.pca, col.ind = "cos2",
  gradient.cols = c("#00AFBB","#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping (slow if many points)
)