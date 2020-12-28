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
#' @param dtr_print set to \code{TRUE} by default, whether there is text and 
#' line for ascent time.
#' @param hour_print set to \code{TRUE} by default, whether there is the hour
#' at beginning and end of the dive.
#' @param line_print set to \code{TRUE} by default, whether there is lines 
#' for depths.
#' @param deco_print set to \code{TRUE} by default, whether there is information
#' about desat stop for depths and times.
#' @param depth_print set to \code{TRUE} by default, whether there is depth 
#' values along the dive.
#' @param time_print set to \code{TRUE} by default, whether there is time 
#' values along the dive.
#' @param def_cols set to \code{FALSE} by default, modify plot colors to match
#' shinyapp css.
#' @param legend set to \code{FALSE} by default, print legend about the ascent 
#' speeds.
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
                      dtr_print = TRUE,
                      hour_print = TRUE,
                      line_print = TRUE,
                      deco_print = TRUE,
                      depth_print = TRUE,
                      time_print = TRUE,
                      def_cols = FALSE,
                      legend = FALSE,
                      # safe = TRUE,
                      add = FALSE) {
  # modify hour
  if (!add) {x$dtcurve$times <- x$dtcurve$times + x$hour[1]}
  
  dtcurve <- x$dtcurve
  # set global plot ----
  delta_x <- diff(x$hour) * 0.1
  delta_y <- (depth(x) - min(dtcurve$depths)) * 0.2
  default_par <- list(
    col = 1, type = "l",
    xlab = "Time (min)", ylab = "Depth (m)",
    xlim = c(min(dtcurve$times) - delta_x, max(dtcurve$times) + delta_x),
    ylim = -c(max(dtcurve$depths) + delta_y, min(dtcurve$depths) - delta_y)
  )
  # check ... ----
  call_par <- list(...)
  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) |
      is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }

  if ("log" %in% names(call_par)) {
    call_par$log <- NULL
    warning('log can not be used in this plot type')
  }
  # def bg cols ----
  if (def_cols) {
    call_par$col <- "darkred" # TODO here modify def col
    par(bg = "gray")
  }

  if (!add) {
    # init plot ----
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    empty_par$axes=FALSE
    do.call(plot, empty_par)
    # def bg cols bis ----
    if (def_cols) {
      colfunc <- colorRampPalette(c("cyan", "royalblue")) 
      # TODO change the colors here
      rect(par("usr")[1], 0, par("usr")[2], par("usr")[4], col = "white")
      n <- 100
      rec <- seq(0, par("usr")[3], length.out = n)
      for (i in 2:n) {
        rect(par("usr")[1], rec[i], par("usr")[2], rec[i - 1],
          col = colfunc(n)[i - 1],
          border = colfunc(n)[i - 1]
        )
      }
    }
    # change color around and make axis ----
    box(col = call_par$col)
    axis(1, col = call_par$col, col.ticks = call_par$col, 
         col.axis = call_par$col) #, 
         # labels=letters[1:10], at=1:10) # TODO here make the hour placement !
    axis(2, col = call_par$col, col.ticks = call_par$col, 
         col.axis = call_par$col)
    mtext(call_par$xlab, side=1, line=3, col=call_par$col)
    mtext(call_par$ylab, side=2, line=3, col=call_par$col)
    
    # add depth lines ----
    if (line_print) {
      for (i in seq(from = 0, to = -80, by = -5)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.1)
      }
      for (i in seq(from = 0, to = -80, by = -10)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.3)
      }
    }
  }

  # print the dive line ----
  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL ; line_par$ylim <- NULL
  line_par$xlab <- NULL ; line_par$ylab <- NULL
  line_par$main <- NULL
  do.call(lines, line_par)
  
  # hour points ----
  points(x = x$hour, y = rep(0, 2), pch = c(25, 24), 
         bg = rep(call_par$col, 2))
  if (hour_print){
    text(
      x = x$hour, y = 0,
      # sprintf("%s: %02.0f:%02.0f:%02.0f", c("start", "end"), x$hour %/% 60, 
      sprintf("%02.0f:%02.0f:%02.0f", x$hour %/% 60, 
              x$hour %% 60, (x$hour %% 60 %% 1) * 60 ), 
      pos = 3, col = call_par$col
    )
  }
  
  if(depth_print | deco_print){
    depth_i <- depths_inf(x, col = call_par$col, 
                          only_pal = (!depth_print) & deco_print )
    do.call(text, depth_i)
  }
  
  if(time_print){
    time_i <- times_inf(x, col = call_par$col)
    do.call(text, time_i)
  } else if(deco_print){
    for (i in x$palier$depth[x$palier$time > 0]) {
      text(
        x = mean(dtcurve$time[dtcurve$depths == i]), y = -i,
        paste(x$palier$time[x$palier$depth == i], "'", sep = ""), pos = 3,
        col = call_par$col
      )
    }
  }

  if (dtr_print) {
    # dtr
    last <- max(which(dtcurve$depths == max(dtcurve$depths)))
    lines(
      x = dtcurve$times[c(last, rep(length(dtcurve$depths), 2))],
      y = -dtcurve$depths[c(last, last, length(dtcurve$depths))],
      lty = 3, col = call_par$col
    )
    text(
      x = mean(dtcurve$times[c(3, length(dtcurve$depths))]),
      y = -max(dtcurve$depths),
      paste(x$dtr, "'", sep = ""), pos = 3,
      # paste("dtr = ", x$dtr, "'", sep = ""), pos = 3,
      col = call_par$col
    )
  }
  # add default legend about ascent speed ----
  if (legend) {
    legend("bottomright", sprintf(
      "%s speed : %g m/min", c("ascent", "deco stop"),
      unlist(speed(x))
    ), horiz = T, cex = 0.7, box.col = call_par$col, text.col = call_par$col)
  }
}

#' depths_inf
#' 
#' @param x a \code{\link[mn90]{dive}} object.
#' @param col a color value
#' @param only_pal set to \code{FALSE} by default,
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
depths_inf <- function(x, col, only_pal = FALSE){
  depths <- unique(x$dtcurve$depths)
  depths <- depths[depths != 0]
  if(only_pal){
    pal <- depths %in% x$palier$depth
    depths <- depths[pal]
    first <- NULL
  } else {
    first = 1
  }
  times <- x$dtcurve$times[x$dtcurve$depths %in% depths]
  times <- times[c(first,seq(length(times), 2 + length(first) , by = -2))]
  times <- sort(times)
  pos <- c(first, rep(4,length(times) - length(first)))
  
  
  depth_inf <- list(x = times, y = -depths, labels = paste(-depths, "m"), 
                    pos = pos, col = col)
  
  return(depth_inf)
}

#' times_inf
#' 
#' @param x a \code{\link[mn90]{dive}} object.
#' @param col a color value
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
times_inf <- function(x, col){
  
  times <- x$dtcurve$times[-c(1, length(x$dtcurve$times))]
  depths <- x$dtcurve$depths[-c(1, length(x$dtcurve$depths))]
  
  f1 <- rev(seq(length(times), 2, by = -2))
  f2 <- rev(seq(length(times)-1, 1, by = -2))
  if((! 2 %in% f1) & (! 1 %in% f2) ){
    f1 <- c(2,f1) ; f2 <- c(1, f2)
  }
  
  dtimes <- times[f1] - times[f2]
  times <- times[f2] + dtimes /2
  
  ddepths <- depths[f1] - depths[f2]
  depths <- depths[f2] + ddepths / 2
  
  pos <- rep(3,length(dtimes))
  
  time_inf <- list(x = times, y = -depths, labels = paste0(dtimes, "'"), 
                   pos = pos, col = col)
  
  return(time_inf)
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

  if (x$type == 'success'){
    plot(x$dive1, add = TRUE, col = call_par$col, text_print= text_print)
    plot(x$dive2, add = TRUE, col = call_par$col, text_print= text_print)
    
    text(
      x = mean(c(x$dive1$hour[2], x$dive2$hour[1])), y = 0,
      sprintf("%s: %g'",c('inter', 'maj'), c(x$inter, x$dive2$maj)), pos = c(3,1),
      col = call_par$col
    )
  } else if (x$type == 'consec'){
    plot(x$dive1, add = TRUE, col = call_par$col, hour_print= FALSE)
    plot(x$dive2, add = TRUE, col = call_par$col, hour_print= FALSE)
    hours <- c(x$dive1$hour[1], x$dive2$hour[2],
               mean(x$dive1$hour[2], x$dive2$hour[1]))
    t <- c(x$dive1$hour[1], x$dive2$hour[2], x$inter)
    text(
      x = hours, y = 0,
      # x$hour, pos = 3,
      sprintf("%s: %g'", c("start", "end", 'inter'), t), pos = 3,
      col = call_par$col
    )
  }
}  