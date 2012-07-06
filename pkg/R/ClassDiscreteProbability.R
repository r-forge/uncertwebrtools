setClass(
  Class="DiscreteProbability",
  representation=representation(categories="character", probabilities="numeric")
  )

setMethod(
  f="initialize",
  signature="DiscreteProbability",
  definition=function(.Object, category, probability)
  {
    .Object@categories<-category
    .Object@probabilities<-probability
    return(.Object)    
  }
  )

setGeneric(
  name="getDiscreteCategories",
  def=function(.Object){standardGeneric("getDiscreteCategories")}
  )

setMethod(
  f="getDiscreteCategories",
  signature="DiscreteProbability",
  definition=function(.Object)
  {
    return(.Object@categories)    
  }
  )

setGeneric(
  name="getDiscreteProbabilities",
  def=function(.Object){standardGeneric("getDiscreteProbabilities")}
  )

setMethod(
  f="getDiscreteProbabilities",
  signature="DiscreteProbability",
  definition=function(.Object)
  {
    return(.Object@probabilities)    
  }
  )

setGeneric(
  name="getDiscreteCategoryCount",
  def=function(.Object){standardGeneric("getDiscreteCategoryCount")}
  )

setMethod(
  f="getDiscreteCategoryCount",
  signature="DiscreteProbability",
  definition=function(.Object)
  {
    return(length(.Object@categories))
  }
  )

setGeneric(
  name="addDiscreteCategories<-",
  def=function(.Object, category){standardGeneric("addDiscreteCategories<-")}
  )

setReplaceMethod(
  f="addDiscreteCategories",
  signature="DiscreteProbability",
  definition=function(.Object, category)
  {
    .Object@categories<-c(.Object@categories, category)
    return(.Object)   
  }
  )

setGeneric(
  name="setDiscreteProbabilities<-",
  def=function(.Object, probability){standardGeneric("setDiscreteProbabilities<-")}
  )

setReplaceMethod(
  f="setDiscreteProbabilities",
  signature="DiscreteProbability",
  definition=function(.Object, probability)
  {
    .Object@probabilities<-probability
    return(.Object)
    
  }
  )