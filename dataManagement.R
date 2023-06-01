#' Returns all columns from a dataset but the tag, which must be placed on the last column
#' 
getDataCol = function(data){
  data[-ncol(data)]
}

#' Returns tag column from a dataset, it must be placed on the last column
#' 
getTagCol = function(data){
  data[,ncol(data)]
}

#' Returns the same data separating the tag from the rest, tag column must be
#' the last one
#' 
#' @return list whit parameters:
#' "d" with the dataset and
#' "tag" with it tags
#'  
partitionDataTag = function(data){
  return(list(d=getDataCol(data), tag=getTagCol(data)))
}

#' Returns the same data separating the tag from the rest, tag column must be
#' the last one
#' 
#' @param tagFactor indica si se quiere que el tag sea factor
#' 
#' @return list whit parameters:
#' "d" with the dataset and
#' "tag" with it tags
#'  
partitionDataTag = function(data, tagFactor = F, tagName = NA){
  d=getDataCol(data)
  
  if (is.na(tagName)){
    tag = getTagCol(data)
  }else{
    tag = data[[tagName]]
  }
  
  if(tagFactor){
    return(list(d=d, tag=as.factor(tag)))
  }else{
    return(list(d=d, tag=tag))
  }
  
}


#'It divides the data into train and test
#' 
#' @param data: List with the element "d" with the dataset and "tag" its labels
#' @param p: Percentage of instances of the training data
#' 
#' @return List with 2 lists train and test, which both contain the parameter "d"
#' with the dataset and "tag" its labels
#' 
partitionTrainTest = function(data,  p = 0.8){
  
  # Por si se hace iris[5] en vez de iris[,5]
  if ( class(data$tag) == "data.frame" )
    data$tag = data$tag[,1]
  
  iTrain = caret::createDataPartition (y = data$tag,
                                       p = p,
                                       list = FALSE)
  
  train=list(d=data$d[iTrain,], 
             tag=data$tag[iTrain])
  
  test=list(d=data$d[-iTrain,],
            tag=data$tag[-iTrain])
  
  data=list(train=train, test=test)
  
  return(data)  
}
