#' Lynx and Hare Pelts in Canada
#'
#' A dataset containing a canadian community of hares and lynxs, represented by the number (in thousands) of pelt.
#' It represent a classical exemple in dynamics model, and was first analyzed by biologist Charles Gordon Hewitt.
#'
#' Howard (2009) provides numerical data for the number of pelts collected by the Hudson’s Bay Company
#' in the years 1900-1920, which we have included in comma-separated value (CSV) form in the source
#' repository with the case study.
#'
#' Ref : Howard, P. (2009). Modeling basics. Lecture Notes for Math 442, Texas A&M University.
#'
#' @format A community with 2 populations:
#' \describe{
#'   \item{hudson}{the label of the community}
#'   \item{populations}{the list of two populations}
#'   \itemize{
#'   \item{hare : the population of hares, interpolated from the number of pelts}
#'   \item{lynx : the population of hares, interpolated from the number of pelts}
#'   }
#'
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @source \url{https://mc-stan.org/users/documentation/case-studies/lotka-volterra-predator-prey.html#data-lynx-and-hare-pelts-in-canada}
"hudson"
