setClass(
  Class="ConfusionMatrix",
  representation=representation(sourceCategories="character", 
                                targetCategories="character",
                                counts="integer")
  )

setMethod(
  f="initialize",
  signature="ConfusionMatrix",
  definition=function(.Object, Source, Target, Count)
  {
    .Object@sourceCategories<-Source
    .Object@targetCategories<-Target
    .Object@counts<-Count
    return(.Object)    
  }
  )

setGeneric(
  name="getConfusionMatrixSourceCategories",
  def=function(.Object){standardGeneric("getConfusionMatrixSourceCategories")}
  )

setMethod(
  f="getConfusionMatrixSourceCategories",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    return(.Object@sourceCategories)    
  }
  )

setGeneric(
  name="getConfusionMatrixTargetCategories",
  def=function(.Object){standardGeneric("getConfusionMatrixTargetCategories")}
  )

setMethod(
  f="getConfusionMatrixTargetCategories",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    return(.Object@targetCategories)    
  }
  )
# 
# setGeneric(
#   name="getConfusionMatrixCounts",
#   def=function(.Object){standardGeneric("getConfusionMatrixCounts")}
#   )
# 
# setMethod(
#   f="getConfusionMatrixCounts",
#   signature="ConfusionMatrix",
#   definition=function(.Object)
#   {
#     return(.Object@counts)    
#   }
#   )

setGeneric(
  name="getConfusionMatrixCategoryCount",
  def=function(.Object){standardGeneric("getConfusionMatrixCategoryCount")}
  )

setMethod(
  f="getConfusionMatrixCategoryCount",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    return(length(.Object@sourceCategories))
  }
  )

setGeneric(
  name="addConfusionMatrixSourceCategories<-",
  def=function(.Object, Category){standardGeneric("addConfusionMatrixSourceCategories<-")}
  )

setReplaceMethod(
  f="addConfusionMatrixSourceCategories",
  signature="ConfusionMatrix",
  definition=function(.Object, Category)
  {
    .Object@sourceCategories<-c(.Object@sourceCategories, Category)
    return(.Object)   
  }
  )

setGeneric(
  name="addConfusionMatrixTargetCategories<-",
  def=function(.Object, Category){standardGeneric("addConfusionMatrixTargetCategories<-")}
  )

setReplaceMethod(
  f="addConfusionMatrixTargetCategories",
  signature="ConfusionMatrix",
  definition=function(.Object, Category)
  {
    .Object@targetCategories<-c(.Object@targetCategories, Category)
    return(.Object)   
  }
  )