# A performance comparison between feature reduction and feature selection algorithms preprocessing on wide data

In this repository the R code for the feature selection and feature selection algorithms used in article *A performance comparison between feature reduction and feature selection algorithms preprocessing on wide data*, the stored algorithms are:

<table>
   <tr>
      <th>Algorithm</th>
      <th>Original package</th>
   </tr>
   <tr>
      <td><strong>Feature reduction - Linear Unsupervised</strong></td>
      <td></td>
   </tr>
   <tr>
      <td>PCA (Principal Component Analysis)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>LPE (Locality Pursuit Embedding)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>PFLPP (Parameter-Free Locality Preserving Projection)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>RNDPROJ (Random Projection)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td><strong>Feature reduction - Linear - Supervised</strong></td>
      <td></td>
   </tr>
   <tr>
      <td>FSCORE (Fisher Score)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>LSLS (Least Squares Linear Discriminant Analysis)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>LFDA (Local Fisher Discriminant Analysis)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>MMC (Maximum Margin Criterion)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>SAVE (Spectral Anticorrelation via Variance Expansion)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>SLPE (Supervised Locality Preserving Embedding)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td><strong>Feature reduction - Non linear</strong></td>
      <td></td>
   </tr>
   <tr>
      <td>MDS (Multidimensional Scaling)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>MMDS (Maximum Margin Dimensionality Reduction)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>LLE (Locally Linear Embedding)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>NPE (Neighborhood Preserving Embedding)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>LEA (Laplacian Eigenmaps)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>SNE (Stochastic Neighbor Embedding)</td>
      <td>Rdimtools</td>
   </tr>
   <tr>
      <td>Autoencoder</td>
      <td>h2o</td>
   </tr>
   <tr>
      <td><strong>Feature selection</strong></td>
      <td></td>
   </tr>
   <tr>
      <td>SVM-RFE (Support Vector Machine - Recursive Feature Elimination)</td>
      <td>sigFeature</td>
   </tr>
   <td></td><td></td>
</table>



# How to use

Run *requeriments.R* which will install the necessary libraries.

```Rscript requeriments.R```

Import preprocessing functions:

```
source("featureReducers.R")
```

This line also imports *preprocessing_methods.R* file which has the functions to format and manage the datasets to the one needed by the algorithms.

These algorithms receive as input a list with element "d" as the dataset and "tag" as its tags, function *partitionDataTag* can be used to format any dataframe in the desired format. Notice that the tag is placed in the last column.

```
# Create data based on iris dataset, we only select 2 classes since svm_rfe only
# works this way
data = iris[1:100,] %>%
  partitionDataTag() %>%
  partitionTrainTest()
data = iris[1:100,] %>%
  partitionDataTag() %>%
  partitionTrainTest()
```

Then,  we can launch any of the feature reduction algorithms:

```
# The dimensionality reduction functions return a list with the reduced data and the transformation matrix

ndim=2

# Linear unsupervised
fReduction_pca(data, ndim)
fReduction_lpe(data, ndim)
fReduction_pflpp(data, ndim)
fReduction_rndproj(data, ndim)

# Linear supervised
fReduction_fscore(data, ndim)
fReduction_lfda(data, ndim)
fReduction_lsls(data, ndim)
fReduction_mmc(data, ndim)
fReduction_save(data, ndim)
fReduction_slpe(data, ndim)

# Non linear
fReduction_mds(data, ndim)
fReduction_mmds(data, ndim)
fReduction_lle(data, ndim)
fReduction_lea(data, ndim)
fReduction_npe(data, ndim)
fReduction_sne(data, ndim)
fReduction_autoencoder(data, ndim)
```


The functions can be used without the test dataset:
```
fReduction_pca(data$train, ndim)
```

To obtain the transformation matrix in linear algorithms, it is necessary to call the function from the Rdimtools library

```
Rdimtools::do.pca(
  as.matrix(data$train$d),
  ndim
)
```


To estimate the reduction in non-linear algorithms, the aproximate_nonlinear_transformation function is used
```
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
```

The feature selector returns the list of features ordered from highest to lowest importance
```
fSelection_svm_rfe(data$train)
```

# Cite this article

```
@article{ramos2023reduction,
  title={A performance comparison between feature reduction and feature selection algorithms preprocessing on wide data},
  author={Ramos-P{\'e}rez, Ismael and Barbero-Aparicio, Jos{\'e} Antonio and Canepa-Oneto, Antonio and Arnaiz-Gonz{\'a}lez, {\'A}lvar and Maudes-Raedo, Jes{\'u}s M.},
  journal={Applications Sciences},
  year = "in press"
}
```
