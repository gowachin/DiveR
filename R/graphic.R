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
#' @param lines_print set to \code{TRUE} by default, whether there is lines 
#' for depths.
#' @param def_cols FALSE by default, 
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
                      line_print = TRUE,
                      def_cols = FALSE,
                      legend = FALSE,
                      # safe = TRUE,
                      add = FALSE) {
  
  x$dtcurve$times <- x$dtcurve$times + x$hour[1]
  
  dtcurve <- x$dtcurve

  delta_x <- diff(x$hour) * 0.1
  delta_y <- (depth(x) - min(dtcurve$depths)) * 0.2

  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time (min)",
    ylab = "Depth (m)",
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


  if (def_cols) {
    call_par$col <- "darkred"
    par(bg = "gray")
  }

  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"

    do.call(plot, empty_par)


    if (def_cols) {
      colfunc <- colorRampPalette(c("cyan", "royalblue"))
      rect(par("usr")[1], 0, par("usr")[2], par("usr")[4], col = "white")
      n <- 100
      rec <- seq(0, par("usr")[3], length.out = n)
      for (i in 2:n) {
        rect(par("usr")[1], rec[i], par("usr")[2], rec[i - 1],
          col = colfunc(n)[i - 1],
          border = colfunc(n)[i - 1]
        )
      }

      box(col = call_par$col)
      axis(1, col = call_par$col, col.ticks = call_par$col, 
           col.axis = call_par$col)
      axis(2, col = call_par$col, col.ticks = call_par$col, 
           col.axis = call_par$col)
    }

    if (line_print) {
      for (i in seq(from = 0, to = -80, by = -5)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.1)
      }
      for (i in seq(from = 0, to = -80, by = -10)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.3)
      }
    }
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

  if (text_print) {
    text( # max_depth
      x = dtcurve$times[2], y = -max(dtcurve$depths),
      paste(-max(dtcurve$depths), "m"),
      pos = 1,
      col = call_par$col
    )

    text( # depth_time
      x = mean(dtcurve$times[c(2, 3)]), y = -max(dtcurve$depths),
      paste(dtcurve$times[3] - x$hour[1], "'", sep = ""), pos = 3,
      col = call_par$col
    )

    points(x = x$hour, y = rep(0, 2), pch = c(25, 24), 
           bg = rep(call_par$col, 2))
    text(
      x = x$hour, y = 0,
      sprintf("%s: %g'", c("start", "end"), x$hour), pos = 3,
      col = call_par$col
    )

    # paliers infos
    for (i in x$palier$depth[x$palier$time > 0]) {
      # depth of palier
      text(
        x = max(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(-i, "m"), pos = 4, col = call_par$col
      )
      # time of palier
      text(
        x = mean(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(x$palier$time[x$palier$depth == i], "'", sep = ""), pos = 3,
        col = call_par$col
      )
    }

    # dtr
    lines(
      x = dtcurve$times[c(3, rep(length(dtcurve$depths), 2))],
      y = -dtcurve$depths[c(3, 3, length(dtcurve$depths))],
      lty = 3, col = call_par$col
    )
    text(
      x = mean(dtcurve$times[c(3, length(dtcurve$depths))]),
      y = -max(dtcurve$depths),
      paste("dtr = ", x$dtr, "'", sep = ""), pos = 3,
      col = call_par$col
    )
  }

  if (legend) {
    legend("top", sprintf(
      "%s speed : %g m/min", c("ascent", "deco stop"),
      unlist(speed(x))
    ), horiz = T)
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
                       line_print = TRUE,
                       def_cols = FALSE,
                       legend = FALSE,
                       # safe = TRUE,
                       add = FALSE) {
  # x <- mult_dive ; text_print = T; add = F
  # x <- dive
  
  delta_x <- (max(x$dive2$hour) - min(x$dive1$hour)) * 0.1
  delta_y <- (max(depth(x)) - min(min(x$dive1$dtcurve$depths),
                                  min(x$dive2$dtcurve$depths))) * 0.2
  
  x$dive2$dtcurve$times <- x$dive2$dtcurve$times + x$dive2$hour[1]
  
  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time (min)",
    ylab = "Depth (m)",
    main = x$label,
    xlim = c(min(x$dive1$dtcurve$times) - delta_x, 
             max(x$dive2$dtcurve$times) + delta_x),
    ylim = -c(max(x$dive1$dtcurve$depths) + delta_y, 
              min(x$dive2$dtcurve$depths) - delta_y)
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
  
  
  if (def_cols) {
    call_par$col <- "darkred"
    par(bg = "gray")
  }
  
  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    
    do.call(plot, empty_par)
    
    
    if (def_cols) {
      colfunc <- colorRampPalette(c("cyan", "royalblue"))
      rect(par("usr")[1], 0, par("usr")[2], par("usr")[4], col = "white")
      n <- 100
      rec <- seq(0, par("usr")[3], length.out = n)
      for (i in 2:n) {
        rect(par("usr")[1], rec[i], par("usr")[2], rec[i - 1],
             col = colfunc(n)[i - 1],
             border = colfunc(n)[i - 1]
        )
      }
      
      box(col = call_par$col)
      axis(1, col = call_par$col, col.ticks = call_par$col, 
           col.axis = call_par$col)
      axis(2, col = call_par$col, col.ticks = call_par$col, 
           col.axis = call_par$col)
    }
    
    if (line_print) {
      for (i in seq(from = 0, to = -80, by = -5)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.1)
      }
      for (i in seq(from = 0, to = -80, by = -10)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.3)
      }
    }
  }
  
  dtcurve <- x$dive1$dtcurve
  
  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL
  
  do.call(lines, line_par)
  
  if (text_print) {
    text( # max_depth
      x = dtcurve$times[2], y = -max(dtcurve$depths),
      paste(-max(dtcurve$depths), "m"),
      pos = 1,
      col = call_par$col
    )
    
    text( # depth_time
      x = mean(dtcurve$times[c(2, 3)]), y = -max(dtcurve$depths),
      paste(dtcurve$times[3] - x$dive1$hour[1], "'", sep = ""), pos = 3,
      col = call_par$col
    )
    
    points(x = x$dive1$hour, y = rep(0, 2), pch = c(25, 24), 
           bg = rep(call_par$col, 2))
    text(
      x = x$dive1$hour, y = 0,
      sprintf("%s: %g'", c("start", "end"), x$dive1$hour), pos = 3,
      col = call_par$col
    )
    
    # paliers infos
    for (i in x$dive1$palier$depth[x$dive1$palier$time > 0]) {
      # depth of palier
      text(
        x = max(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(-i, "m"), pos = 4, col = call_par$col
      )
      # time of palier
      text(
        x = mean(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(x$dive1$palier$time[x$dive1$palier$depth == i], "'", sep = ""), pos = 3,
        col = call_par$col
      )
    }
    
    # dtr
    lines(
      x = dtcurve$times[c(3, rep(length(dtcurve$depths), 2))],
      y = -dtcurve$depths[c(3, 3, length(dtcurve$depths))],
      lty = 3, col = call_par$col
    )
    text(
      x = mean(dtcurve$times[c(3, length(dtcurve$depths))]),
      y = -max(dtcurve$depths),
      paste("dtr = ", x$dive1$dtr, "'", sep = ""), pos = 3,
      col = call_par$col
    )
  }
  
  if (legend) {
    legend("top", sprintf(
      "%s speed : %g m/min", c("ascent", "deco stop"),
      unlist(speed(xdive1))
    ), horiz = T)
  }
  
  dtcurve <- x$dive2$dtcurve
  
  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL
  
  do.call(lines, line_par)
  
  if (text_print) {
    text( # max_depth
      x = dtcurve$times[2], y = -max(dtcurve$depths),
      paste(-max(dtcurve$depths), "m"),
      pos = 1,
      col = call_par$col
    )
    
    text( # depth_time
      x = mean(dtcurve$times[c(2, 3)]), y = -max(dtcurve$depths),
      paste(dtcurve$times[3] - x$dive2$hour[1], "'", sep = ""), pos = 3,
      col = call_par$col
    )
    
    points(x = x$dive2$hour, y = rep(0, 2), pch = c(25, 24), 
           bg = rep(call_par$col, 2))
    text( 
      x = x$dive2$hour, y = 0,
      sprintf("%s: %g'", c("start", "end"), x$dive2$hour), pos = 3,
      col = call_par$col
    )
    
    # paliers infos
    for (i in x$dive2$palier$depth[x$dive2$palier$time > 0]) {
      # depth of palier
      text(
        x = max(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(-i, "m"), pos = 4, col = call_par$col
      )
      # time of palier
      text(
        x = mean(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(x$dive2$palier$time[x$dive2$palier$depth == i], "'", sep = ""), pos = 3,
        col = call_par$col
      )
    }
    
    # dtr
    lines(
      x = dtcurve$times[c(3, rep(length(dtcurve$depths), 2))],
      y = -dtcurve$depths[c(3, 3, length(dtcurve$depths))],
      lty = 3, col = call_par$col
    )
    text(
      x = mean(dtcurve$times[c(3, length(dtcurve$depths))]),
      y = -max(dtcurve$depths),
      paste("dtr = ", x$dive2$dtr, "'", sep = ""), pos = 3,
      col = call_par$col
    )
  }
  
  if (legend) {
    legend("top", sprintf(
      "%s speed : %g m/min", c("ascent", "deco stop"),
      unlist(speed(xdive2))
    ), horiz = T)
  }
}