source("preprocessing_methods.R")

# Create data based on iris dataset, we only select 2 classes since svm_rfe only
# works this way
data = iris[1:100,] %>%
  partitionDataTag() %>%
  partitionTrainTest()

# The dimensionality reduction functions return a list with the reduced data and the transformation matrix
ndim=2

fReduction_pca(data, ndim)
fReduction_lpe(data, ndim)
fReduction_pflpp(data, ndim)
fReduction_rndproj(data, ndim)

fReduction_fscore(data, ndim)
fReduction_lfda(data, ndim)
fReduction_lsls(data, ndim)
fReduction_mmc(data, ndim)
fReduction_save(data, ndim)
fReduction_slpe(data, ndim)

fReduction_mds(data, ndim)
fReduction_mmds(data, ndim)
fReduction_lle(data, ndim)
fReduction_lea(data, ndim)
fReduction_npe(data, ndim)
fReduction_sne(data, ndim)

fReduction_autoencoder(data, ndim)


# The functions can be used without the test dataset
fReduction_pca(data$train, ndim)


# To obtain the transformation matrix in linear algorithms, it is necessary to call the function from the Rdimtools library
Rdimtools::do.pca(
  as.matrix(data$train$d),
  ndim
)

n
# To estimate the reduction in non-linear algorithms, the aproximate_nonlinear_transformation function is used
dataReduced = Rdimtools::do.mds(
  as.matrix(data$train$d),
  ndim
)$Y

aproximate_nonlinear_transformation(
  as.matrix(data$train$d),
  dataReduced,
  as.matrix(data$test$d),
  k=5
)


# The feature selector returns the list of features ordered from highest to lowest importance
fSelection_svm_rfe(data$train)



