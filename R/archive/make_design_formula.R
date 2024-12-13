#' Construct design formula out of 1 or 2 factors
#'
#' @param fct1 Character string. Name of the first factor.
#' @param fct2 Character string. Name of an optional second factor.
#' @export

make_design_formula <- function(fct1, fct2 = NULL){
  if(!is.null(fct2)){
    expr <- paste(fct2, fct1, sep = "+")
  }  else {
    expr <- fct1
  } 
  contrast <- as.formula(paste("~",expr, sep = ""))
  return(contrast)
}
