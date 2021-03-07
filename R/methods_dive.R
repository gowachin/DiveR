#' is.dive
#'
#' check if the object is of \code{\link[DiveR]{dive}} class
#' 
#' @param x any R object
#' 
#' @return
#' TRUE or FALSE
#' 
#' @examples
#' d <- dive(20,40)
#' is.dive(d)
#' is.dive('dive')
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
is.dive <- function(x){
  inherits(x, 'dive')
}

#' depth
#'
#' \code{depth} retrieve the maximum depth of a singular or multiple dive 
#' sequence. As conso has a dive inside, the depth function also works on it.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}}, \code{\link[DiveR]{ndive}} and
#'  \code{\link[DiveR]{conso}} objects.
#' 
#' @return It returns a numeric with the depth of the dive. 
#' Is a vector if working on \code{\link[DiveR]{ndive}} object
#' 
#' @examples 
#' # Simple dive
#' depth(dive(20,40))
#' # Multiple dives
#' depth(ndive(dive(20,40), dive(15, 80), inter = 540))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
depth <- function(object) {
  UseMethod("depth")
}

#' @rdname depth
#' 
#' @export
depth.dive <- function(object) {
  return(max(object$dtcurve$depths))
}

#' @rdname depth
#' 
#' @export
depth.ndive <- function(object) {
  d1 <- depth(object$dive1)
  d2 <- depth(object$dive2)
  return(c(d1,d2))
}