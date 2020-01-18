#' community
#'
#' Create a community with giving its name and a minimum of one population.
#'
#' @usage
#' community(label, ...)
#'
#' @param label the name of the community (character string)
#' @param ... one or several population objects. All other objects will be rejected and a warning will be print.
#'
#' @details There is no need to put a name for additionnal populations, because each element of community will be named after the label which is in every population object you provide.
#'
#' @seealso \code{\link[BeePODYNA]{population}} to see how to make an object of class population.
#'
#' @examples
#'hare = population("hirsuta",30,2,80)
#'lynx = population("daonensis",4,1.2,60)
#'
#'hudson = community('hudson',hare,lynx)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
community <- function(label,
                      ...) {
  # checking entry
  if (!is.character(label) ||
    length(label) > 1) {
    stop("Label must be a single charactere string")
  }

  pops <- list(...)

  is_pops <- sapply(pops, is_population)

  good_pops <- pops[is_pops]
  bad_pops <- pops[!is_pops]

  if (length(good_pops) == 0) {
    stop("no population provides as arguments")
  }

  if (length(bad_pops) > 0) {
    warning(sprintf(
      "%d provided arguments (%s) are not belonging the population class",
      length(bad_pops),
      which(!is_pops) + 1
    ))
  }

  name_pops <- sapply(good_pops, function(x) x$label)

  if (length(grep("^label|populations$", name_pops)) > 0) {
    stop("a population cannot be named `label` or `populations`")
  }

  names(good_pops) <- name_pops

  structure(list(
    label = label,
    populations = good_pops
  ),
  class = "community"
  )
}


#' is_community
#'
#' Check if the object is a community or not.
#'
#' @usage
#' is_community(x)
#'
#' @param x the object which must be a community to validate the condition
#'
#' @return a logical "TRUE" or "FALSE"
#'
#' @seealso \code{\link[BeePODYNA]{community}} to see how to make an object of class community.
#'
#' @examples
#'hare = population("hirsuta",30,2,80)
#'lynx = population("daonensis",4,1.2,60)
#'
#'hudson = community('hudson',hare,lynx)
#'is_community(hudson)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
is_community <- function(x){
  class(x) == "community"
}

#' @export
`$.community` <- function(x,name) {
  if (name == "label")
    return(x[['label']])

  if (name == "populations")
    return(x[['populations']])

  x[["populations"]][[name]]
}

#' @export
length.community <- function(x) {
  length(x[["populations"]])
}

