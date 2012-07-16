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
  name="getDiscreteProbability",
  def=function(.Object, Category){standardGeneric("getDiscreteProbability")}
  )

setMethod(
  f="getDiscreteProbability",
  signature="DiscreteProbability",
  definition=function(.Object, Category)
  {
    index<-which(.Object@categories==Category)
    return(.Object@probabilities[[index]])    
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
  name="setDiscreteProbability<-",
  def=function(.Object, Category, value){standardGeneric("setDiscreteProbability<-")}
  )

setReplaceMethod(
  f="setDiscreteProbability",
  signature="DiscreteProbability",
  definition=function(.Object, Category, value)
  {
    index<-which(.Object@categories==Category)
    .Object@probabilities[[index]]<-value
    return(.Object)
    
  }
  )