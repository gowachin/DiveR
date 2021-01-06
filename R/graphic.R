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
                      add = FALSE) {
  # modify hour
  if (!add) {x$dtcurve$times <- x$dtcurve$times + x$hour[1]}
  
  dtcurve <- x$dtcurve
  # set global plot ----
  delta_x <- diff(x$hour) * 0.1
  delta_y <- (depth(x) - min(dtcurve$depths)) * 0.2
  default_par <- list(
    col = 1, type = "l", axes = FALSE,
    xlab = "Time (min)", ylab = "Depth (m)", main = '',
    xlim = c(min(dtcurve$times) - delta_x, max(dtcurve$times) + delta_x),
    ylim = -c(max(dtcurve$depths) + delta_y, min(dtcurve$depths) - delta_y)
  )
  # check ... ----
  call_par <- list(...)
  names_defaut_par <- names(default_par)
  if(!'axes' %in% names(call_par) & !add){ default_par$axes = TRUE}
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
    call_par$col.axis <- "darkred"
    tmp_bg <- par('bg') # save bg for later
    par(bg = "gray")
  } else {
    call_par$col.axis <- par('col.axis')
  }

  if (!add) {
    # init plot ----
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    empty_par$axes=FALSE 
    empty_par$xlab = empty_par$ylab = empty_par$main = ''
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
    box(col = call_par$col.axis)
    mtext(call_par$xlab, side=1, line=3, col=call_par$col)
    mtext(call_par$ylab, side=2, line=3, col=call_par$col)
    mtext(call_par$main, side=3, line=1, col=call_par$col)
    
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
  
  if (call_par$axes){
    cust_axis(x, call_par$col.axis)
    axis(2, col = call_par$col.axis, col.ticks = call_par$col.axis, 
              col.axis = call_par$col.axis)
  } else {
    # axis(1, col = call_par$col.axis, col.ticks = call_par$col.axis, 
    #      col.axis = call_par$col.axis)
    # axis(2, col = call_par$col.axis, col.ticks = call_par$col.axis, 
    #      col.axis = call_par$col.axis)
  }

  # print the dive line ----
  line_par <- c(list(x = dtcurve$times, y = -dtcurve$depths), call_par)
  line_par$xlim <- NULL ; line_par$ylim <- NULL
  line_par$xlab <- NULL ; line_par$ylab <- NULL
  line_par$main <- NULL ; line_par$axes <- NULL
  do.call(lines, line_par)
  
  # hour points ----
  points(x = c(x$dtcurve$times[1], tail(x$dtcurve$times, 1)), 
         y = rep(0, 2), pch = c(25, 24), 
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
      paste(round(x$dtr,2), "'", sep = ""), pos = 3,
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
  
  if (def_cols) {par(bg = tmp_bg)}
}


#' cust_axis
#' 
#' Draw the xaxis with time input for the plot.dive function
#' 
#' @param dive a \code{\link[mn90]{dive}} object.
#' @param col a color 
#' @param shift a numeric to shift the axis in plots. 
#' Is used with minutes values.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
cust_axis <- function(dive, col, shift = NULL){
  dist <- switch(sum(diff(dive$hour) >= c(1, 5, 10, 20, 40, 60, 80, 100)),
                 c(0.5, 0.1), c(2, 1), c(2, 1), c(5, 1),
                 c(10, 5), c(15, 5), c(20, 10), c(20, 10)
  )
  
  x <- dive$hour[1] %% dist[1]
  if (x <= dist[2]){
    a = dive$hour[1] + dist[1] - (x * (x != dist[2]))
    b = dive$hour[1] + dist[2] - (x * (x != dist[2]))
  } else if (x > dist[2]) {
    a = dive$hour[1] + 2 * dist[1]  - (x * (x != dist[2]))
    b = dive$hour[1] + 2 * dist[2]  - (x * (x != dist[2]))
  }
  # arrondis à la minute près
  pos <- c(floor(dive$hour[1]), seq(a, dive$hour[2], by = dist[1]) )
  minpos <- c(floor(dive$hour[1]), seq(b, dive$hour[2], by = dist[2]) )
  h <- paste0(as.character(pos %/% 60), 'h')
  m <- pos %% 60
  
  sh_hour <- (m != 0 & c(FALSE, rep(TRUE, length(h)- 2), FALSE))
  h[sh_hour] =  ''
  m[m < 10] =  paste0(0,m[m < 10])
  m[m == 0] =  '00'
  
  lab <- paste0(h, m)
  
  if(!is.null(shift)){
    pos <- pos + shift
    minpos <- minpos + shift
  }
  
  axis(1,labels=lab, at= pos, col = col, col.ticks = col, col.axis = col)
  axis(1,labels=rep('',length(minpos)), at= minpos, tck=-0.02, 
       col = col, col.ticks = col, col.axis = col)
}


#' depths_inf
#' 
#' Create information about depth and time to use with text function 
#' in plot.dive. 
#' Find the places and labels to show depending on the dive curve.
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
  if(length(times) < 3){
    times <- times[1]
  } else {
    times <- times[c(first,seq(length(times), 2 + length(first) , by = -2))]
  }
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
#' @rdname depths_inf
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
  
  time_inf <- list(x = times, y = -depths, labels = paste0(round(dtimes,1), "'"), 
                   pos = pos, col = col)
  
  return(time_inf)
}

#' plot.ndive
#' 
#' Plot the dive curve depending on time and depth. 
#' Only represent square profile
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
#' @param cut_inter 60 by default above which the plot is cut in x axis to 
#' maintain readibility
#' @param legend set to \code{FALSE} by default, print legend about the ascent 
#' speeds.
#' @param add set to \code{FALSE} by default, to add another dive plot 
#' on a precedent one.
#' 
#' @examples # Consecutive dives
#' d <- ndive(dive1 = dive(18,11), dive2 = dive(15,40), inter = 1)
#' plot(d, main = 'consec')
#' # Consecutive dives with second impossible
#' d <- ndive(dive1 = dive(35,11), dive2 = dive(30,20), inter = 4)
#' plot(d, def_cols = TRUE, main = 'no_consec')
#' # Successive dive when inter <= cut_inter
#' d <- ndive(dive1 = dive(18,40), dive2 = dive(15,30), inter = 60)
#' plot(d, def_cols = TRUE, main = 'success no cut')
#' # Successive dive when inter > cut_inter
#' d <- ndive(dive1 = dive(20,40), dive2 = dive(20,30), inter = 121)
#' plot(d, def_cols = TRUE, main = 'success cut')
#' # Successive dive with second impossible
#' d <- ndive(dive1 = dive(18,40), dive2 = dive(35,30), inter = 120)
#' plot(d, def_cols = TRUE, main = 'maj_no_success')
#' # Successive dive with second impossible
#' d <- ndive(dive1 = dive(62,11), dive2 = dive(15,30), inter = 120)
#' plot(d, def_cols = TRUE, main = '60_no_sucess')
#' # Different dives
#' d <- ndive(dive1 = dive(18,11), dive2 = dive(47,8), inter = 730)
#' plot(d, def_cols = TRUE, main = 'diff cut')
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
                       dtr_print = TRUE,
                       hour_print = TRUE,
                       line_print = TRUE,
                       deco_print = TRUE,
                       depth_print = TRUE,
                       time_print = TRUE,
                       def_cols = FALSE,
                       cut_inter = 60,
                       legend = FALSE,
                       add = FALSE) {
  
  check_val(cut_inter)
  
  # get the call_par
  default_par <- list(
    col = 1,
    type = "l",
    xlab = "Time (min)", ylab = "Depth (m)", axes = TRUE
  )
  
  other_params <- c("dtr_print", "hour_print", "line_print", "deco_print", 
              "depth_print", "time_print", "def_cols", "cut_inter",
              "legend", "add")
  
  # call_par <- list()
  call_par <- list(...)
  
  names_defaut_par <- names(default_par)
  if(!'axes' %in% names(call_par) & !add){ default_par$axes = TRUE}
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) |
        is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }
  
  call_par <- c(call_par, dtr_print = dtr_print, hour_print = hour_print, 
                line_print = line_print, deco_print = deco_print, 
                depth_print = depth_print, time_print = time_print, 
                def_cols = def_cols, cut_inter = cut_inter, legend = legend,
                add = add)
  
  # SOLO plot only call plot.dive
  if (x$type == 'solo'){
    solo_par <- call_par
    solo_par$x <- x$dive1
    solo_par$cut_inter <- NULL
    
    do.call(plot, solo_par)
    return()
  }
  
  # set border for plot
  delta_x <- (max(x$dive2$hour) - min(x$dive1$hour)) * 0.1
  delta_y <- (max(depth(x)) - min(min(x$dive1$dtcurve$depths),
                                  min(x$dive2$dtcurve$depths))) * 0.2
  
  x$dive2$dtcurve$times <- x$dive2$dtcurve$times + x$dive2$hour[1]
  
  default_par$xlim = c(min(x$dive1$dtcurve$times) - delta_x, 
             max(x$dive2$dtcurve$times) + delta_x)
  default_par$ylim = -c(max(x$dive1$dtcurve$depths, x$dive2$dtcurve$depths) 
                        + delta_y, 
              min(x$dive1$dtcurve$depths, x$dive2$dtcurve$depths) - delta_y)
  
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
  
  # def bg cols ----
  if (def_cols) {
    call_par$col <- "darkred" # TODO here modify def col
    call_par$col.axis <- "darkred"
    tmp_bg <- par('bg') # save bg for later
    par(bg = "gray")
  } else {
    call_par$col.axis <- par('col.axis')
  }
  
  if (!add) {
    if(x$inter > cut_inter){
      # modify interv and xlim to cut the axis
      new_inter <- x$inter - cut_inter
      x$dive2$dtcurve$times <- x$dive2$dtcurve$times - new_inter
      delta_x <- (max(x$dive2$dtcurve$times) - min(x$dive1$hour)) * 0.1
      call_par$xlim = c(min(x$dive1$dtcurve$times) - delta_x, 
                    max(x$dive2$dtcurve$times) + delta_x)
    } else {
      new_inter <- 0
    }
    
    # init plot ----
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    empty_par$axes=FALSE 
    empty_par[other_params] <- NULL
    do.call(plot, empty_par)
    # def bg cols bis ----
    limits <- par("usr")
    if (def_cols) {
      colfunc <- colorRampPalette(c("cyan", "royalblue")) 
      # TODO change the colors here
      rect(limits[1], 0, limits[2], limits[4], col = "white")
      n <- 100
      rec <- seq(0, limits[3], length.out = n)
      for (i in 2:n) {
        rect(limits[1], rec[i], limits[2], rec[i - 1],
             col = colfunc(n)[i - 1],
             border = colfunc(n)[i - 1]
        )
      }
    }
    # change color around and make axis ----
    box(col = call_par$col.axis)
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
  
  consec_call <- call_par
  consec_call$add <- TRUE 
  consec_call$cut_inter <- NULL 
  if (x$type == 'consec'){
    cat('consec_plot \n')
    consec_call$axes <- FALSE
    consec_call$hour_print <- FALSE
  }
  
  consec_call$x <- x$dive1
  do.call(plot, consec_call)
  consec_call$axes <- FALSE
  consec_call$hour_print <- FALSE
  consec_call$x <- x$dive2
  do.call(plot, consec_call)

  if (x$type == 'consec'){
    # hours
    hours <- c(x$dive1$hour[1], x$dive2$hour[2])
    dIvE <- list(hour = hours)
    # axes
    if (call_par$axes){
      cust_axis(dIvE, call_par$col.axis)
      axis(2, col = call_par$col.axis, col.ticks = call_par$col.axis, 
           col.axis = call_par$col.axis)
    }
    # hours add
    if (hour_print){
      text(
        x = dIvE$hour, y = 0, 
        # sprintf("%02.0f:%02.0f", dIvE$hour %/% 60, 
        #         dIvE$hour %% 60),
        sprintf("%02.0f:%02.0f:%02.0f", dIvE$hour %/% 60,
                dIvE$hour %% 60, (dIvE$hour %% 60 %% 1) * 60 ),
        pos = 3, col = call_par$col
      )
    }
  } else {
    inter_t <- c(tail(x$dive1$dtcurve$times, 1), x$dive2$dtcurve$times[1])
    
    text(
      x = mean(inter_t), 
      y = 0, c(paste('inter:', sprintf("%02.0f:%02.0f:%02.0f", x$inter %/% 60, 
                                       x$inter %% 60, (x$inter %% 60 %% 1) * 60 )),
                                  paste('maj:', x$dive2$maj)), pos = c(3,1),
      col = call_par$col
    )
    
    w_hours <- c(inter_t[2], tail(x$dive2$dtcurve$times, 1))
    if (hour_print ){
      text(
        x = w_hours, y = 0,
        # sprintf("%s: %02.0f:%02.0f:%02.0f", c("start", "end"), x$hour %/% 60, 
        sprintf("%02.0f:%02.0f:%02.0f", x$dive2$hour %/% 60, 
                x$dive2$hour %% 60, (x$dive2$hour %% 60 %% 1) * 60 ), 
        pos = 3, col = call_par$col
      )
    }
    
    if (call_par$axes){
      cust_axis(x$dive2, call_par$col.axis, shift = -new_inter )
    }
  
    if(x$inter > cut_inter){
      # simplified code part from plotrix, but not working on R 3.4.
      breakpos = mean(inter_t)
      xw <- (limits[2] - limits[1]) * 0.05
      yw <- (limits[4] - limits[3]) * 0.05
      br <- c(breakpos - xw / 2, limits[3] - yw / 2, 
              breakpos + xw / 2, limits[3] )
      old.xpd <- par("xpd") ; par(xpd = TRUE)
      # draw the "blank" rectangle
      rect(br[1], br[2], br[3], br[4], col = par("bg"), border = par("bg"))
      # calculate the slash ends
      xbegin <- c(breakpos - xw, breakpos)
      xend <- c(breakpos, breakpos + xw)
      # draw the segments
      segments(xbegin, rep(br[2], 2), xend, rep((br[4]+  yw / 2), 2), 
               col = consec_call$col.axis, lty = 1)
      # restore xpd
      par(xpd = old.xpd)
    }
  }

  if (def_cols) {par(bg = tmp_bg)}
}