#' @import plot.matrix
#' @import graphics
#' @import grDevices
NULL

#' plot.dive
#' 
#' Plot the dive curve depending on time and depth. 
#' Only represent sqare profile
#' 
#' @param x an object of class dive.
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#' @param text_print set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#' @param add set to \code{FALSE} by default, to add another population plot on a precedent one.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[mn90]{dive}} for every aspect about dive object creations.
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot.dive <- function(x,
                      ...,
                      text_print = TRUE,
                      # safe = TRUE,
                      add = FALSE) {
  
  # x <- dive
  curve <- x$curve
  
  delta_x <- (max(curve$times) - min(curve$times)) * 0.1
  delta_y <- (max(curve$depths) - min(curve$depths)) * 0.2
  
  # palier ; curve
  # plot(curve$times, -curve$depths, type = "l")
  # abline(h = 0, col = "darkblue", lty = 3)
  
  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time",
    ylab = "Depth",
    main = x$label,
    xlim = c(min(curve$times) - delta_x, max(curve$times) + delta_x),
    ylim = -c(max(curve$depths) + delta_y, min(curve$depths) - delta_y)
  )
  
  call_par <- list(...)
  
  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) | is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }
  
  if ("log" %in% names(call_par)) {
    call_par$log <- sub("x", "", call_par$log)
    if (regexec("y", call_par$log)[[1]] > 0) {
      if (default_par$ylim[1] < 0) call_par$ylim[1] <- 1
    }
  }
  
  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    
    do.call(plot, empty_par)
  }
  
  line_par <- c(list(x = curve$times, y = -curve$depths), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL
  
  do.call(lines, line_par)
  
  # if(safe){
  #   polygon(curve$times[2:4], -curve$depths[2:4], col = "red")
  # }
  
  if (text_print) {
    text(
      x = curve$times[2], y = -max(curve$depths),
      paste("max depth =", -max(curve$depths),"m"),
      pos = 1
      ,
      col = call_par$col
    )
  }
}