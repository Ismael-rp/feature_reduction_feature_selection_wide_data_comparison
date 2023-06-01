library(sigFeature)
library(Rdimtools)
library(h2o)
library(dplyr)
source("dataManagement.R")

# Feature reduction -------------------------------------------------------

##### Linear ----

###### Generic functions ----

#' Wrapper for the Rdimtools library linear unsupervised feature reduction
#' algorithms. It applies the learned transformation in the train data to the
#' test data.
#' 
#' This functions is called from the functions of type "fReduction_pca" in this
#' file.
#' 
#' If the data is separated in train and test, it only transforms the data.
#' 
#' @param f: function to execute
#' 
#' @param data:
#' Option 1 - List with the elements "train" and "test" and sub-elements 'd'
#' for the dataset and 'tag' for its tags.
#' list(train(list(d, tag)), test(list(d, tag)))
#'  
#' Option 2 - List with the elements "d" for the dataset and "tag" for its tags.
#' tags. If it does not find the "test" element, it assumes that this option
#' has been chosen.
#' list(d, tag)
#' 
#' @param ndim, if it is NA, by default it will be the number of dimensions -1
#' of the original data
#' 
#' @return Data with the same structure as the parameter data where train$d
#' and test$d have been transformed with the projection obtained by applying
#' the dimensionality reducer f to train$d
#' 
fReduction_linear_unsupervised_rdimtools = function(f, data, ndim, ...){
  
  if ("test" %in% names(data)){
    data = fReduction_linear_unsupervised_rdimtools_with_test_data(f, data, ndim, ...)
    
  }else{
    data = fReduction_linear_unsupervised_rdimtools_without_test_data(f, data, ndim, ...)
  }
  
  data
}

fReduction_linear_unsupervised_rdimtools_with_test_data = function(f, data, ndim, ...){
  
  data$train$d = as.matrix(data$train$d)
  data$test$d = as.matrix(data$test$d)
  
  fRedOut = f(data$train$d, ndim, ...)
  
  data$train$d = as.data.frame(fRedOut$Y)
  data$test$d = as.data.frame(as.matrix(data$test$d) %*% fRedOut$projection)
  
  data 
}


fReduction_linear_unsupervised_rdimtools_without_test_data = function(f, data, ndim, ...){
  
  data$d = as.matrix(data$d)
  
  fRedOut = f(data$d, ndim, ...)
  
  data$d = as.data.frame(fRedOut$Y)
  
  data 
}


#' Wrapper for executing the supervised dimensionality reduction algorithms
#' of the Rdimtools library, applies the transformation learned in the train
#' on the test.
#'
#' If the data is passed separated by train and test, it only transforms the
#' data indicated.
#'
#' @param f: function to execute
#' 
#' @param data:
#' Option 1 - List with the elements "train" and "test" and sub-elements 'd'
#' for the dataset and 'tag' for its tags.
#' list(train(list(d, tag)), test(list(d, tag)))
#'  
#' Option 2 - List with the elements "d" for the dataset and "tag" for its tags.
#' tags. If it does not find the "test" element, it assumes that this option
#' has been chosen.
#' list(d, tag)
#'  
#' @param ndim: Number of dimensions to reduce
#' 
#' @return Data with the same structure as the data parameter where train$d and
#' test$d have been transformed with the projection obtained when applying the
#' dimensionality reducer f on train$d 
#' 
fReduction_linear_supervised_rdimtools = function(f, data, ndim, ...){
  
  if ("test" %in% names(data)){
    data = fReduction_linear_supervised_rdimtools_with_test_data(f, data, ndim, ...)
    
  }else{
    data = fReduction_linear_supervised_rdimtools_without_test_data(f, data, ndim, ...)
  }
  
  data
}


fReduction_linear_supervised_rdimtools_with_test_data = function(f, data, ndim, ...){
  
  data$train$d = as.matrix(data$train$d)
  data$test$d = as.matrix(data$test$d)
  
  fRedOut = f(data$train$d, data$train$tag, ndim, ...)
  
  data$train$d = as.data.frame(fRedOut$Y)
  data$test$d = as.data.frame(as.matrix(data$test$d) %*% fRedOut$projection)
  
  data
}


fReduction_linear_supervised_rdimtools_without_test_data = function(f, data, ndim, ...){
  
  data$d = as.matrix(data$d)
  
  fRedOut = f(data$d, data$tag, ndim, ...)
  
  data$d = as.data.frame(fRedOut$Y)
  
  data 
}


#' Indicates the number of columns of data
#' It has to check if the data has test
fReduction_get_ncol = function(data){
  
  if ("test" %in% names(data)){
    return( ncol(data$train$d) )
  }
  
  return( ncol(data$d) )
}


#' Indicates the number of rows of data
#' It has to check if the data has test
fReduction_get_nrow = function(data){
  
  if ("test" %in% names(data)){
    return( nrow(data$train$d) )
  }
  
  return( nrow(data$d) )
}


#' Indicates the number of classes
#' It has to check if the data has test
fReduction_get_nClasses = function(data){
  
  if ("test" %in% names(data)){
    return( length(levels(droplevels(as.factor(data$train$tag)))) )
  }
  
  return( length(levels(droplevels(as.factor(data$tag)))) )
}

##### Unsupervised ----

#' This functions executes a unsupervised dimensionality reduction algorithm from the Rdimtools library
#' 
#' @param data: 
#' @param data:
#' Option 1 - List with the elements "train" and "test" and sub-elements 'd'
#' for the dataset and 'tag' for its tags.
#' list(train(list(d, tag)), test(list(d, tag)))
#'  
#' Option 2 - List with the elements "d" for the dataset and "tag" for its tags.
#' tags. If it does not find the "test" element, it assumes that this option
#' has been chosen.
#' list(d, tag)
#'
fReduction_pca = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)-1
  }
  
  fReduction_linear_unsupervised_rdimtools(
    Rdimtools::do.pca,
    data,
    ndim,
    ...
  )
}


fReduction_lpe = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)-1
  }
  
  fReduction_linear_unsupervised_rdimtools(
    Rdimtools::do.lpe,
    data,
    ndim,
    ...
  )
}


fReduction_pflpp = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)
  }
  
  fReduction_linear_unsupervised_rdimtools(
    Rdimtools::do.pflpp,
    data,
    ndim,
    ...
  )
}


fReduction_rndproj = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)
  }
  
  fReduction_linear_unsupervised_rdimtools(
    Rdimtools::do.rndproj,
    data,
    ndim,
    ...
  )
}


##### ---- Supervised ----


#' The maximum number of possible levels is k-1 being k the number of classes,
#' this maximum is chosen by default.
#' 
fReduction_fscore = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.fscore,
    data,
    ndim,
    ...
  )
}

fReduction_lfda = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.lfda,
    data,
    ndim,
    ...
  )
}


fReduction_lsls = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.lsls,
    data,
    ndim,
    ...
  )
}


fReduction_mmc = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)-1
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.mmc,
    data,
    ndim,
    ...
  )
}

fReduction_save = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)-1
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.save,
    data,
    ndim,
    ...
  )
}


fReduction_slpe = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = fReduction_get_ncol(data)-1
  }
  
  fReduction_linear_supervised_rdimtools(
    Rdimtools::do.slpe,
    data,
    ndim,
    ...
  )
}





#---- Non Linear ----

#' Wrapper to execute non linear dimensionality reduction algorithms of the
#' Rdimtools library
#' 
#' @param f: function to execute
#' @param data: List with the elements "train" and "test" and sub-elements
#' "d" for the dataset and "tag" for its labels.
#' @param ndim: Number of dimensions to obtain
#' 
#' @return Data with the same structure as the data parameter where train$d
#' and test$d have been transformed with the projection obtained when applying
#' the dimensionality reducer f on train$d
#' 
fReduction_nonlinear_rdimtools = function (f, data, ndim, ...){
  
  if ("test" %in% names(data)){
    data = fReduction_nonlinear_rdimtools_with_test_data(f, data, ndim, ...)
    
  }else{
    data = fReduction_nonlinear_rdimtools_without_test_data(f, data, ndim, ...)
  }
  
  data
}


fReduction_nonlinear_rdimtools_with_test_data = function(f, data, ndim, ...){
  
  data$train$d = as.matrix(data$train$d)
  data$test$d = as.matrix(data$test$d)
  
  fRedOut = f(data$train$d, ndim, ...)
  
  data$test$d =
    aproximate_nonlinear_transformation(data$train$d, fRedOut$Y, data$test$d)
  data$train$d = data.frame(fRedOut$Y) # Is not as.data.frame because of the colnames
  
  data
}


fReduction_nonlinear_rdimtools_without_test_data = function(f, data, ndim, ...){
  
  data$d = as.matrix(data$d)
  
  fRedOut = f(data$d, ndim, ...)
  
  data$d = as.data.frame(fRedOut$Y) # Is not as.data.frame because of the colnames
  
  data 
}


#' Approximates the nonlinear dimensionality reduction transformation done to
#' the data dataInit to obtain dataT the data dataNew.
#' As the funcion Rdimtools::do.pca is used, dataT must have fewer columns than
#' dataInit.
#'
#' Steps:
#' 1. For each point p of dataNew obtain its k nearest neighbor of dataInit
#' (dataInit_nn)
#' 2. Reduce with PCA dataInit_nn to another subspace with the same number of
#' that dataT has using PCA, obtaining a new dataset (dataInit_nn_pca) and
#' its projection (pcaOut$projection).
#' 3. Transform p, reducing its dimensionality with pcaOut$projection and
#' estimate its values with a linear regression from dataInit_nn_pca
#'
#' @param dataInit: Initial data of the transformation to approximate
#' @param dataT: Final data of the transformation to approximate
#' @param dataNew: New data to transform
#' @param k: Number of neighbors
#'
#' @return daraframe of the transformation approximated over dataNew
#' 
aproximate_nonlinear_transformation = function(dataInit, dataT, dataNew, k=5){
  
  stopifnot("dataT contains NAs" = !any(is.na(dataT)))
  
  final_n_dim = ncol(dataT)
  output = data.frame()
  
  # for each instance
  for (i in 1:nrow(dataNew)){
    
    # Nearest Neighbour (nn) of the p point from dataInit
    p = dataNew[i,] %>% as.numeric() %>% matrix(nrow=1)
    i_nn = FNN::get.knnx(dataInit, p, k=k)$nn.index
    
    dataInit_nn = dataInit[i_nn,] %>% as.matrix()
    dataT_nn = dataT[i_nn,]
    
    
    # PCA
    pcaOut = Rdimtools::do.pca(dataInit_nn, ndim = final_n_dim)
    
    dataInit_nn_pca = pcaOut$Y %>% as.data.frame()
    dataNew_pca = p %*% pcaOut$projection %>% as.data.frame()
    
    # Linear regression
    df = data.frame(dataInit_nn_pca, dataT_nn)
    
    outParams = colnames(df[,(ncol(df)/2 + 1):ncol(df)]) %>%
      paste(collapse=",")
    
    form = paste("cbind(", outParams, ") ~ .",sep="") %>%
      as.formula()
    
    p_out = lm(form, data = df) %>% 
      predict(dataNew_pca)
    
    output = rbind(output, data.frame(p_out))
  }
  
  output
}


#' Non metric MDS
#' ndim must be between 2 and min(ncol-1, nrow-1)
#' but it is forced to be between 2 and min(ncol-1, nrow-1)
#' to fit with the other algorithms.
#' 
fReduction_mds = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.mds,
    data,
    ndim,
    ...
  )
}


#' Metric MDS
#' 
fReduction_mmds = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.mmds,
    data,
    ndim,
    ...
  )
}


#' LLE
#' ndim must be between 2 and min(ncol-1, nrow-1)
#' 
fReduction_lle = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.lle,
    data,
    ndim,
    ...
  )
}


#' NPE
#' ndim must be between 2 and min(ncol-1, nrow-1)
#' 
fReduction_npe = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.npe,
    data,
    ndim,
    ...
  )
}


#' LEA
#' ndim must be between 2 and min(ncol-1, nrow-1)
#' 
fReduction_lea = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.lea,
    data,
    ndim,
    ...
  )
}

#' SNE
#' ndim must be between 2 and min(ncol-1, nrow-1)
#' 
fReduction_sne = function(data, ndim=NA, ...){
  
  if(is.na(ndim)){
    ndim = min( fReduction_get_ncol(data)-1, fReduction_get_nrow(data)-1 )
  }
  
  fReduction_nonlinear_rdimtools(
    Rdimtools::do.sne,
    data,
    ndim,
    ...
  )
}



#' It launches an autoencoder from the h2o library, uses Java underneath
#' 
#' @param data list with the data to reduce, it will replace the data of
#' data$train$d and data$test$d with the model trained with the data$train$d.
#' @param nthreads number of threads to use
#' @param gbMemory maximum GB of memory to use
#' @param hidden numeric vector that indicates the size of each layer
#' @param layer layer from which the new data is extracted from, eg: if there are 3
#' hidden layers c(5,2,5), layer=2 replaces the test data with what the layer of size 2 returns.
#' @param epoch number of epoch
#' @param activation activation function (??h2o.deeplearning)
#' @param showError show the error of the model after training
#' @param ... other parameters of the function ??h2o.deeplearning
#' 
fReduction_autoencoder = function(
    data,
    nthreads=1,
    gbMemory=1,
    hidden=NA,
    layer=1,
    epoch=10,
    activation="Tanh",
    showError=F,
    ...){
  
  apply=base::apply # h20 overwrites the apply function
  
  # To make the dimension fit with the other algorithms in wide data, it can be removed
  if (is.na(hidden)){
    hidden = nrow(data$train$d)-1
  }
  h2o::h2o.no_progress()  # turn off progress bars
  max_mem_size = paste(gbMemory, "g", sep="")
  h2o.init(max_mem_size = max_mem_size, nthreads=nthreads)
  
  model = h2o::h2o.deeplearning(
    x = seq_along(data$train$d),
    training_frame = as.h2o(data$train$d),
    autoencoder = TRUE,
    hidden = hidden,
    activation = activation,
    epoch = epoch,
    ...
  )
  
  data$train$d =
    h2o::h2o.deepfeatures(model, as.h2o(data$train$d), layer = layer) %>% 
    as.data.frame()
  data$test$d = 
    h2o::h2o.deepfeatures(model, as.h2o(data$test$d), layer = layer) %>%
    as.data.frame()
  
  if (showError){
    print(paste("MSE:", h2o.mse(model)))
    print(paste("RMSE:", h2o.rmse(model)))
  }
  
  h2o.shutdown(prompt=F)
  
  data 
}


# Feature selection -------------------------------------------------------

#' Algorithm for feature selection based on SVM-RFE (Recursive Feature Elimination)
#' 
#' @param data: List with the element "d" with the dataset and "tag" its labels
#' the data of the dataset must be NUMERIC
#' the data of the label must be CATEGORICAL and have 2 classes
#' 
#' @return dataset with the names of the parameters ordered from best to worst
#' along with its p-value (the lower the p value, the greater the importance)
#' 
fSelection_svm_rfe = function(data){
  
  # Transorm to factor, delete unused levels
  data$tag = as.factor(data$tag)
  data$tag = droplevels(data$tag)
  
  if (length(levels(data$tag)) != 2 ) 
    stop("Number of classes must be 2, not ", length(levels(data$tag)))
  
  orderFeatures = svmrfeFeatureRanking(data$d, data$tag)
  names(data$d)[orderFeatures]
}











