#' @import graphics
#' @import grDevices
NULL

#' plot.dive
#' 
#' Plot the dive curve depending on time and depth. 
#' Only represent sqare profile
#' 
#' @param x an object of class dive.
#' @param ... every argument for the \code{\link[graphics]{plot}} function 
#' such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the 
#' dive curve.
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, 
#'   see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Depth'} by default, 
#'   see \code{\link[graphics]{title}}.
#'   }
#' @param text_print set to \code{TRUE} by default, whether there is a text 
#' for depths and duration of deco stops.
#' @param add set to \code{FALSE} by default, to add another dive plot 
#' on a precedent one.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and 
#' \code{\link[graphics]{par}} for plot parameter that were omitted 
#' on this documentation
#' \item \code{\link[mn90]{dive}} for every aspect about dive object creations.
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
plot.dive <- function(x,
                      ...,
                      text_print = TRUE,
                      # safe = TRUE,
                      add = FALSE) { # TODO

  # x <- dive
  dtcurve <- x$dtcurve

  delta_x <- (max(dtcurve$times) - min(dtcurve$times)) * 0.1
  delta_y <- (max(dtcurve$depths) - min(dtcurve$depths)) * 0.2

  # palier ; dtcurve
  # plot(dtcurve$times, -dtcurve$depths, type = "l")
  # abline(h = 0, col = "darkblue", lty = 3)

  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time",
    ylab = "Depth",
    main = x$label,
    xlim = c(min(dtcurve$times) - delta_x, max(dtcurve$times) + delta_x),
    ylim = -c(max(dtcurve$depths) + delta_y, min(dtcurve$depths) - delta_y)
  )

  call_par <- list(...)

  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) |
      is.null(call_par[[names_defaut_par[i]]])) {
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

  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL

  do.call(lines, line_par)

  # if(safe){
  #   polygon(dtcurve$times[2:4], -dtcurve$depths[2:4], col = "red")
  # }

  if (text_print) {
    text(
      x = dtcurve$times[2], y = -max(dtcurve$depths),
      paste("max depth =", -max(dtcurve$depths), "m"),
      pos = 1,
      col = call_par$col
    )
  }
}

#' plot.ndive
#' 
#' Plot the dive curve depending on time and depth. 
#' Only represent sqare profile
#' 
#' @param x an object of class ndive.
#' @param ... every argument for the \code{\link[graphics]{plot}} function 
#' such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the 
#' dive curve
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, 
#'   see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Depth'} by default, 
#'   see \code{\link[graphics]{title}}.
#'   }
#' @param text_print set to \code{TRUE} by default, whether there is a text 
#' for depths and duration of deco stops.
#' @param add set to \code{FALSE} by default, to add another dive plot 
#' on a precedent one.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and 
#' \code{\link[graphics]{par}} for plot parameter that were omitted 
#' on this documentation
#' \item \code{\link[mn90]{ndive}} for every aspect about 
#' ndive object creations.
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
plot.ndive <- function(x,
                      ...,
                      text_print = TRUE,
                      # safe = TRUE,
                      add = FALSE) { # TODO
  x <- mult_dive ; text_print = T; add = F
  # x <- dive
  x <- x$dive1
  dtcurve <- x$dtcurve
  
  delta_x <- (max(dtcurve$times) - min(dtcurve$times)) * 0.1
  delta_y <- (max(dtcurve$depths) - min(dtcurve$depths)) * 0.2
  
  # palier ; dtcurve
  # plot(dtcurve$times, -dtcurve$depths, type = "l")
  # abline(h = 0, col = "darkblue", lty = 3)
  
  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time",
    ylab = "Depth",
    main = x$label,
    xlim = c(min(dtcurve$times) - delta_x, max(dtcurve$times) + delta_x),
    ylim = -c(max(dtcurve$depths) + delta_y, min(dtcurve$depths) - delta_y)
  )
  
  call_par <- list(...)
  
  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) | 
        is.null(call_par[[names_defaut_par[i]]])) {
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
  
  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL
  
  do.call(lines, line_par)
  
  # if(safe){
  #   polygon(dtcurve$times[2:4], -dtcurve$depths[2:4], col = "red")
  # }
  
  if (text_print) {
    text(
      x = dtcurve$times[2], y = -max(dtcurve$depths),
      paste("max depth =", -max(dtcurve$depths),"m"),
      pos = 1
      ,
      col = call_par$col
    )
  }
}