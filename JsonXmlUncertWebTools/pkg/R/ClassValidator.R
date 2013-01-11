setClass(
  Class="validator"
)

setGeneric(
  name="validate",
  def=function(.Object) {standardGeneric("validate")}
)

setMethod(
  f="validate",
  signature="validator",
  definition=function(.Object){
    
    success=TRUE
    
    Class=class(.Object)[1]
    # use schema reader to get the Uncertainty type from schema
    Slots=slotNames(Class)
  
    for(i in 1:length(Slots)){
      # validate the slot value
    }
    return success   
  }
)

validReal<-function(val){
  if (!is.real(val)){
    paste("Value is not real:", val)
    FALSE
  }   
  else
    TRUE
}

validPosReal<-function(val){
  if (validReal(val)){
    if (val<0){
      paste("Value is not positive real:", val)
      FALSE
    }
    else
      TRUE
  }
  else
    FALSE
}

# validND<-function(.Object){
#       if (!is.real(.Object@mean))
#         "'mean' of the Normal Distribution is not a real quantity"
#       else{
#         if (!is.real(.Object@variance))
#           "'variance' of the Normal Distribution is not a real quantity"
#         else{
#           if (.Object@variance<0)
#             "'variance' of the Normal Distribution is not a positive quantity "
#           else
#             TRUE
#         }
#       }
# }