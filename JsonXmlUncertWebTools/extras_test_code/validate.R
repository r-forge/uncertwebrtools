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

validND<-function(.Object){
      if (!is.real(.Object@mean))
        "'mean' of the Normal Distribution is not a real quantity"
      else{
        if (!is.real(.Object@variance))
          "'variance' of the Normal Distribution is not a real quantity"
        else{
          if (.Object@variance<0)
            "'variance' of the Normal Distribution is not a positive quantity "
          else
            TRUE
        }
      }
}