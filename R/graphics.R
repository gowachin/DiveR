#' @import graphics
NULL

#' plot_population
#'
#' plot one given population and its capacity line.
#'
#' @param x a population object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{text_x} set to \code{0} by default, where to position the capacity text on X axis, Y axis is the capacity value.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#' @param capacity_line set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#' @param capacity_lty set to \code{3} by default.
#' @param text_print set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#' @param add set to \code{FALSE} by default, to add another population plot on a precedent one.
#'
#'
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[BeePODYNA]{population}} for every aspect about population and community creations.
#' }
#'
#' @examples
#' library(Template)
#' data(hudson)
#' hare <- hudson$hare
#' lynx <- hudson$lynx
#'
#' plot(hare, col = 'red', pch=15, type = 'b', capacity_lty = 3,
#'      lty =2, main = "Hudson data")
#' plot(lynx, col = 'blue', pch=15, type = 'b', capacity_lty = 3,
#'      add = TRUE, lty = 1, capacity_line = TRUE)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot.population <- function(x,
                            ... ,
                            capacity_line = TRUE,
                            capacity_lty = 3,
                            text_print = TRUE,
                            add = FALSE) {

  pop <- x
  mc <- match.call()
  # checks for arguments
  if (!is_population(pop)) {
    stop("The object must be a population.")
  }

  # check for custom graphical parameters
  if (is.null(mc$col)) {
    color <- 1
  } else {
    color <- eval(mc$col)
  }

  if (is.null(mc$text_x)) {
    text_x <- 0
  } else {
    text_x <- eval(mc$text_x)
  }

  if (is.null(mc$type)) {
    type <- "b"
  } else {
    type <- eval(mc$type)
  }

  if (is.null(mc$xlab)) {
    xlab <- "Time"
  } else {
    xlab <- eval(mc$xlab)
  }

  if (is.null(mc$ylab)) {
    ylab <- "Size"
  } else {
    ylab <- eval(mc$ylab)
  }

  if (is.null(mc$main)) {
    main <- pop$label
  } else {
    main <- eval(mc$main)
  }

  if (!add) {
    # limitations
    if (is.null(mc$xlim)) {
      time_min <- min(pop$time)
      time_max <- max(pop$time)
      x_dist <- time_max - time_min
      time_min <- time_min - x_dist * 0.1
      time_max <- time_max + x_dist * 0.1

      xlim <- c(time_min, time_max)
    } else {
      xlim <- eval(mc$xlim)
      time_min <- xlim[1]
      time_max <- xlim[2]
    }
    text_x <- xlim[1]

    if (is.null(mc$ylim)) {
      pop_min <- min(pop$size)
      pop_max <- max(pop$size)
      y_dist <- pop_max - pop_min
      pop_min <- pop_min - y_dist * 0.1
      pop_max <- pop_max + y_dist * 0.1

      ylim <- c(pop_min, pop_max)
    } else {
      ylim <- eval(mc$ylim)
      pop_min <- ylim[1]
      pop_max <- ylim[2]
    }

    # plotting env
    localplot <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
                              capacity_lty, capacity_line, add, text_x, text_print) {
      plot(x, y, ..., type = "n", xlab = " ", ylab = " ", main = " ")
    }
    localplot(x = 1, y = 1, xlim, ylim, ...)

    # setting titles
    localTitle <- function(..., type, xlim, ylim, xlab, ylab, main,
                               capacity_lty, capacity_line, add, text_x, text_print) title(...)
    localTitle(main, sub = NULL, xlab, ylab)

  } else {
    if (is.null(mc$xlim)) {
      time_min <- min(pop$time)
      time_max <- max(pop$time)
      x_dist <- time_max - time_min
      time_min <- time_min - x_dist * 0.1
      time_max <- time_max + x_dist * 0.1

      text_x <- time_min
    } else {
      text_x <- eval(mc$xlim)[1]
    }
  }
  # plotting the pop itself
  localLines <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
                           capacity_lty, capacity_line, add, text_x, text_print) lines(x, y, ...)
  localLines(pop$time, pop$size, type, ...)

  if (capacity_line) {
    localAbline <- function(..., type, xlim, ylim, xlab, ylab, main,
                            capacity_lty, capacity_line, add, text_x, text_print) abline(...)
    localAbline(h = pop$capacity, lty = capacity_lty, col = color)
  }

  if (text_print) {
    localTexte  <- function(..., type, xlim, ylim, xlab, ylab, main,
                            capacity_lty, capacity_line, add, text_x, text_print) text(...)
    localTexte(text_x, pop$capacity, paste("K", pop$label, sep =" "), pos = 4, col = color)
  }

}

#' plot.community
#'
#' plot one given population and its capacity line.
#'
#' @param x a population object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{capacity_line} set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#'   \item \strong{capacity_lty} set to \code{3} by default.
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{text_print} set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#'   \item \strong{text_x} set to \code{0} by default, where to position the capacity text on X axis, Y axis is the capacity value.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#'
#' @note For \strong{capacity_line}, \strong{capacity_lty}, \strong{type}, \strong{text_print} and \strong{text_x} parameters, values can be a single value or a vector of value. If the vector length is inferior to the population number in the community, the value or the vector will be copy.
#'
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[BeePODYNA]{plot.population}}, \code{\link[BeePODYNA]{population}}, \code{\link[BeePODYNA]{community}} for every aspect about population and community creations.
#' }
#'
#' @import graphics
#'
#' @examples
#' data(hudson)
#' plot(hudson)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot.community <- function(x, ...) {

  com <- x
  n_pop <- length(com)
  mc <- match.call()

  # checks for arguments
  if (!is_community(com)) {
    stop("The object must be a community.")
  }
  # check for custom graphical parameters
  if (is.null(mc$col)) {
    color <- rep(1, n_pop)
  } else {
    color <- eval(mc$col)
    if (length(color) < n_pop) color <- rep(color, n_pop)
  }

  if (is.null(mc$text_print)) {
    text_print <- rep(TRUE, n_pop)
  } else {
    text_print <- eval(mc$text_print)
    if (length(text_print) < n_pop) text_print <- rep(text_print, n_pop)
  }

  if (is.null(mc$text_x)) {
    text_x <- rep(0, n_pop)
  } else {
    text_x <- eval(mc$text_x)
    if (length(text_x) < n_pop) text_x <- rep(text_x, n_pop)
  }

  if (is.null(mc$capacity_line)) {
    capacity_line <- rep(TRUE, n_pop)
  } else {
    capacity_line <- eval(mc$capacity_line)
    if (length(capacity_line) < n_pop) capacity_line <- rep(capacity_line, n_pop)
  }

  if (is.null(mc$capacity_lty)) {
    capacity_lty <- rep(3, n_pop)
  } else {
    capacity_lty <- eval(mc$capacity_lty)
    if (length(capacity_lty) < n_pop) capacity_lty <- rep(capacity_lty, n_pop)
  }

  if (is.null(mc$type)) {
    type <- rep("b", n_pop)
  } else {
    type <- eval(mc$type)
    if (length(type) < n_pop) type <- rep(type, n_pop)
  }

  if (is.null(mc$xlab)) {
    xlab <- "Time"
  } else {
    xlab <- eval(mc$xlab)
  }

  if (is.null(mc$ylab)) {
    ylab <- "Size"
  } else {
    ylab <- eval(mc$ylab)
  }

  if (is.null(mc$main)) {
    main <- com[[1]]
  } else {
    main <- eval(mc$main)
  }

  # limitations
  if (is.null(mc$xlim)) {
    time_min <- min(as.vector(sapply(com$populations, "[[", 3)))
    time_max <- max(as.vector(sapply(com$populations, "[[", 3)))
    x_dist <- time_max - time_min
    time_min <- time_min - x_dist * 0.1
    time_max <- time_max + x_dist * 0.1

    xlim <- c(time_min, time_max)
  } else {
    xlim <- eval(mc$xlim)
    time_min <- xlim[1]
    time_max <- xlim[2]
  }
  text_x <- rep(xlim[1], n_pop)

  if (is.null(mc$ylim)) {
    com_min <- min(as.vector(sapply(com$populations, "[[", 2)))
    com_max <- max(as.vector(sapply(com$populations, "[[", 2)))
    y_dist <- com_max - com_min
    com_min <- com_min - y_dist * 0.1
    com_max <- com_max + y_dist * 0.1

    ylim <- c(com_min, com_max)
  } else {
    ylim <- eval(mc$ylim)
    com_min <- ylim[1]
    com_max <- ylim[2]
  }

  # plotting env
  localplot <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
                        capacity_lty, capacity_line, text_x, text_print) {
    plot(x, y, ..., type = "n", xlab = " ", ylab = " ", main = " ")
  }
  localplot(x = 1, y = 1, xlim, ylim, ...)

  # setting titles
  localTitle <- function(..., type, xlim, ylim, xlab, ylab, main,
                         capacity_lty, capacity_line, text_x, text_print) title(...)
  localTitle(main, sub = NULL, xlab, ylab)

  # plotting the com itself
  localLines <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main, col,
                         capacity_lty, capacity_line, log_pop, add, text_x, text_print) lines(x, y, ..., col = color[i])
  localAbline <- function(..., type, xlim, ylim, xlab, ylab, main,
                          capacity_lty, capacity_line, log_pop, add, text_x, text_print) abline(...)
  localTexte <- function(..., type, xlim, ylim, xlab, ylab, main,
                         capacity_lty, capacity_line, log_pop, add, text_x, text_print) text(...)
  for (i in 1:n_pop) {
    localLines(x = com$populations[[i]][[3]], y = com$populations[[i]][[2]], type[i], ...)
    if (capacity_line[i]) {
      localAbline(h = com$populations[[i]]$capacity, lty = capacity_lty[i], col = color[i])
    }
    if (text_print[i]) {
      localTexte(text_x[i], com$populations[[i]]$capacity, paste("K", com$populations[[i]]$label, sep = " "), pos = 4, col = color[i])
    }
  }
}

