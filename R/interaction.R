#' @import R.utils
NULL

#' interactions
#'
#' \code{mat_interaction} creates an interactions matrix object defining the positive and negative interactions between populations.
#' The interaction is not assumed symetrical, so a population can have a different effect on a population than this latest has on the first one.
#' The interaction of a population on itself is equal to 0. The element \code{[i,j]} of the matrix is the effect of the population on the j column on the population of the i line.
#' If no interaction vector is given, the default values are 0.
#'
#' @param nb_pop is the number of populations in the model. The matrix interaction with only one population is set to 0.
#' @param list.interactions is a vector of length \code{nb_pop*(nb_pop-1)} giving the interaction of each population on the other one.
#' A positive value means a positive impact (facilitation) while a negative value means a negative impact (predation, competition).
#' The vector starts with the vector of the interactions of the first population on the other ones (ranging from 2 to the last one, not including itself),
#' and then the vector of the interactions of the second population on the others...
#' @param labels is a vector of characters giving the labels of the populations.
#'
#' @examples
#'   interactions(nb_pop=3, list.interactions=c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'   interactions(nb_pop=2)
#'   interactions(nb_pop=1)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
interactions <- function(nb_pop,
                            list.interactions = rep(0, nb_pop * (nb_pop - 1)),
                            labels = sprintf("Pop_%d",seq_len(nb_pop))
                           ) {
  if (nb_pop == 1) {
    warning("As there is only one population, the interaction matrix has only one element equal to 0.")
    it <- matrix(0)
    rownames(it) <- labels
    colnames(it) <- labels
    class(it) <- "interactions"

    return(it)
  }

  if (!is.numeric(nb_pop) ||
      as.integer(nb_pop) != nb_pop ||
      nb_pop < 2) {
    stop("Nb_pop must be an integer superior or equal to 1.")
  }

  if (!is.vector(list.interactions) ||
      !is.numeric(list.interactions) ||
      length(list.interactions) != nb_pop * (nb_pop - 1)) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  }

  it <- insert(list.interactions, c(0:(nb_pop - 1)) * nb_pop + 1, 0)
  dim(it) <- c(nb_pop, nb_pop)
  rownames(it) <- labels
  colnames(it) <- labels
  class(it) <- "interactions"

  it
}


#' is_interactions
#'
#' \code{is_interactions} returns \code{TRUE} if x is an interaction matrix of the class 'interactions'.
#' It returns \code{FALSE} otherwise.
#' To create such an object you can use the \code{\link[BeePODYNA]{interactions}} function.
#'
#' @param x is an \code{R} object.
#'
#' @examples
#'   is_interactions(3)
#'   x=interactions(nb_pop=3, list.interactions=c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'   is_interactions(x)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
is_interactions <- function(x){
 class(x) == "interactions"
}


#' as_interactions
#'
#' \code{as_interactions} transforms a vector in an interactions matrix object defining the positive and negative interactions between populations.
#' The interaction is not assumed symetrical, so a population can have a different effect on a population than this latest has on the first one.
#' The interaction of a population on itself is equal to 0. If no interaction vector is given, the default values are 0.
#' See \code{\link[BeePODYNA]{interactions}} function to create a new interaction matrix from scratch.
#' If the object is of the type \code{numeric}, it should be a vector of numeric of length \code{nb_pop*(nb_pop-1)} giving the interaction of each population on the other one.
#' Each interaction is a decimal ranging between -1 and 1. A positive value means a positive impact like facilitation while a negative value means a negative impact like predation, competition.
#' The vector starts with the vector of the interactions of the first population on the other ones ranging from 2 to the last one, not including itself,
#' and then the vector of the interactions of the second population on the others...
#'
#' @param object is an \code{R} object to transform into an interaction matrix.
#'
#' @examples
#'   as_interactions(c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#'
#' @export
as_interactions <- function(object) {
  UseMethod("as_interactions")
}

#' @rdname as_interactions
#' @export
as_interactions.default <- function(object) {
  stop(sprintf("I cannot cast an object of type %s to interaction object", class(object)))
}

#' @rdname as_interactions
#' @export
as_interactions.interactions <- function(object) {
  object
}

#' @rdname as_interactions
#' @export
as_interactions.numeric <- function(object) {
  if (!is.numeric(object)) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  }

  nb_pop <- (1 + sqrt(1 + 4 * length(object))) / 2

  if (as.integer(nb_pop) != nb_pop ||
      nb_pop < 2) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  }

  interactions(nb_pop,object)
}