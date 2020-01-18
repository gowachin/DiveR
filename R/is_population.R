#' is_population
#'
#' The function determinines if the object is a population or not
#' @param x the object which must be a population to validate the condition
#' @return a logical "TRUE" or "FALSE"
#' @author Cresciense Lecaude
#'
#' @export
is_population <- function(x){
  if (class(x) == "population"){
    return(TRUE)
  }
 else {
  return(FALSE)
  }
}
