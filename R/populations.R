#' Calculates a growth rate from a birth rate and a death rate.
#'
#' @param birth_rate the birth rate of the population
#' @param death_rate the death rate of the population
#'
#' @return returns the growth rate of the population
#'
#' @examples
#'   growth_rate(0.5, 0.2)
#'
#' @export

growth_rate <- function(birth_rate,death_rate) {
  if(!is.numeric(birth_rate)||
     birth_rate < 0)
    stop("Birth rate must be a positive number")
  if(!is.numeric(death_rate)||
     death_rate < 0)
    stop("Death rate must be a positive number")

  return(birth_rate - death_rate)
}


#' Population
#'
#' Create a population with giving its name, initial size, growth rate and capacity.
#'
#' @param label the name of the population
#' @param initial_size the size of the population at TO
#' @param growth_rate the growth_rate of the population, given by the user or calculated by function growth_rate with birth_rate and death_rate
#' @param capacity the capacity limit of the environment for the population
#'
#' @examples
#'   population('example', 2, 0.5, 10)
#'
#' @export
population <- function(label,
                       initial_size,
                       growth_rate,
                       capacity    = Inf) {


  if (!is.character(label) ||
      length(label) > 1 )
    stop("Label must be a single charactere string")

  if (!is.numeric(initial_size) ||
      length(initial_size) > 1  ||
      initial_size < 0
     )
    stop("Population size must be a single positive number")

  if (!is.numeric(growth_rate) ||
      length(growth_rate) > 1 )
    stop("Growth rate size must be a single number")

  if (!is.numeric(capacity) ||
      length(capacity) > 1  ||
      capacity <= 0)
    stop("Capacity size must be a single strictly positive number")

  structure(list(label = label,
            size  = initial_size,
            time  = 0.0,
            growth_rate = growth_rate,
            capacity = capacity),
            class = "population"
            )
}