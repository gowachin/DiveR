#' BeePODYNA
#'
#' A package to study population dynamic.
#'
#' @docType package
#' @name beepodyna
NULL

#' beepodyna
#'
#' The \code{beepodyna} function creates a \code{R} object of the class \code{beepodyna}.
#'
#' @details The \code{beepodyna} object is a list containing a community, the corresponding interaction matrix
#' and a list of the functions to apply to each population, in the order they appear in the community object.
#'
#' @param label is the label of the \code{beepodyna} object.
#' @param community is the community object of class \code{community} to study
#' @param interactions is the interaction matrix of the class \code{interactions} between the populations of the community. Default is a matrix without interactions.
#' @param functions is a function or a list of functions to apply to each population in the same order as it is in the community.
#' If the length of the list is not the same as the community, the functions are repeated so the length of the list is the same as the number of populations.
#' @param verbose Default is \code{TRUE}. Set to \code{FALSE} if you don't want the warnings.
#'
#' @seealso \code{\link[BeePODYNA]{community}} and \code{\link[BeePODYNA]{interactions}} to create the object in parameters.
#'
#' @examples
#' hare = population("hirsuta",30,2,80)
#' lynx = population("daonensis",4,1.2,60)
#' hudson = community("hudson",hare,lynx)
#'
#' beepodyna(label="model_1",
#'           community=hudson,
#'           interactions=interactions(2),
#'           exp,
#'           verbose = FALSE
#'           )
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
beepodyna <- function(label,
                      community,
                      interactions = interactions(length(community)),
                      functions = identity,
                      verbose = TRUE) {

  ### check the class of the parameters

  if (!is.character(label) ||
    length(label) > 1) {
    stop("Label must be a single charactere string")
  }

  if (class(community) != "community") {
    stop("The community given is not of the class community.")
  }
  if (class(interactions) != "interactions") {
    stop("The interactions matrix given is not of the class interactions")
  }
  if (class(functions) != "function" && class(functions) != "list") {
    stop("The functions given are not a single function or a list.")
  }
  if (class(functions) == "list") {
    for (l in 1:length(functions)) {
      if (class(functions[[l]]) != "function") {
        stop(sprintf("The %d element of the functions list in not a function."))
      }
    }
  }

  ### check the length of the parameters

  nb_pop <- length(community)

  if (dim(interactions)[1] != nb_pop) {
    stop("All the parameters haven't the same size.")
  }

  if (length(functions) > nb_pop) {
    if (verbose) {
      warning("The functions list is too long comparing to the number of populations. The last functions of the list are not used.")
    }
    functions <- functions[1:nb_pop]
  }

  if (length(functions) < nb_pop) {
    if (verbose) {
      warning("The functions list has been repeted since its length doesn't match the number of population.")
    }
    functions <- rep(list(functions), ceiling(nb_pop / length(functions)))[1:nb_pop]
  }

  ### Create the beepodyna object
  structure(list(
    label = label,
    community = community,
    interactions = interactions,
    functions = functions
  ),
  class = "beepodyna"
  )
}


#' @export
length.beepodyna <- function(x) {
  length(x$community)
}
