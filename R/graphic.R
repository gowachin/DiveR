#' @import graphics
#' @import grDevices
NULL

#' plot.dive
#' 
#' Plot the dive curve depending on time and depth. 
#' Only represent square profile
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
#' @param dtr_print set to \code{FALSE} by default, whether there is text and 
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
#' \item \code{\link[DiveR]{dive}} for every aspect about dive object creations.
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
plot.dive <- function(x,
                      ...,
                      dtr_print = FALSE,
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
  desat <- x$desat$desat_stop
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
    call_par$col <- "#fd8f0c" # TODO here modify def col
    call_par$col.axis <- "#741d16"
    tmp_bg <- par('bg') # save bg for later
    par(bg = "#909fa1")
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
      colfunc <- colorRampPalette(c("#04728d", "#051c2e")) 
      # TODO change the colors here
      rect(par("usr")[1], 0, par("usr")[2], par("usr")[4], col = "#628db0")
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
    mtext(call_par$xlab, side=1, line=3, col=call_par$col.axis)
    mtext(call_par$ylab, side=2, line=3, col=call_par$col.axis)
    mtext(call_par$main, side=3, line=1, col=call_par$col.axis)
    
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
      minute_to_time(x$hour, sec = TRUE, sep = ':'),
      # sprintf("%02.0f:%02.0f:%02.0f", x$hour %/% 60, 
      #         x$hour %% 60, (x$hour %% 60 %% 1) * 60 ), 
      pos = 3, col = call_par$col
    )
  }
  
  if(depth_print | deco_print){
    depth_i <- depths_inf(x, col = call_par$col, 
                          only_desat = (!depth_print) & deco_print )
    do.call(text, depth_i)
  }
  
  if(time_print){
    time_i <- times_inf(x, col = call_par$col)
    do.call(text, time_i)
  } 
  if(deco_print){
    for (i in desat$depth[desat$time > 0]) {
      text(
        x = mean(c(desat$hour[desat$depth == i], 
                 desat$hour[desat$depth == i] + desat$time[desat$depth == i])) +
          x$hour[1], 
        y = -i,
        labels = paste(desat$time[desat$depth == i], "'", sep = ""), pos = 3,
        col = call_par$col
      )
    }
  }

  if (dtr_print) {
    # dtr
    # last <- max(which(dtcurve$depths == max(dtcurve$depths)))
    last <- which(dtcurve$times == dtime(x) + x$hour[1])
    lines(
      x = dtcurve$times[c(last, rep(length(dtcurve$depths), 2))],
      y = -dtcurve$depths[c(last, last, length(dtcurve$depths))],
      lty = 3, col = call_par$col
    )
    text(
      x = mean(dtcurve$times[c(last, length(dtcurve$depths))]),
      y = -dtcurve$depths[last],
      paste(round(x$params["dtr"],2), "'", sep = ""), pos = 3,
      # paste("dtr = ", x$params["dtr"], "'", sep = ""), pos = 3,
      col = call_par$col
    )
  }
  # add default legend about ascent speed ----
  # if (legend) {
  #   legend("bottomright", sprintf(
  #     "%s speed : %g m/min", c("ascent", "deco stop"),
  #     unlist(speed(x))
  #   ), horiz = T, cex = 0.7, box.col = call_par$col.axis, 
  #   text.col = call_par$col.axis)
  # }
  
  if (def_cols) {par(bg = tmp_bg)}
}


#' cust_axis
#' 
#' Draw the xaxis with time input for the plot.dive function
#' 
#' @param dive a \code{\link[DiveR]{dive}} object.
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
  h <- pos %/% 60
  while(any(h >= 24)){
    h[h >= 24] <- h[h >= 24] - 24
  }
  h <- paste0(as.character(h), 'h')
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
#' \item \code{\link[DiveR]{ndive}} for every aspect about 
#' ndive object creations.
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
plot.ndive <- function(x,
                       ...,
                       dtr_print = FALSE,
                       hour_print = TRUE,
                       line_print = TRUE,
                       deco_print = TRUE,
                       depth_print = TRUE,
                       time_print = TRUE,
                       def_cols = FALSE,
                       cut_inter = 60,
                       legend = FALSE,
                       add = FALSE) {
  
  if (cut_inter <= 0 | !is.numeric(cut_inter)) {
    stop("cut_inter must be a single positive numeric value.")
  }
  
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
    return(invisible(NULL))
  }
  

  # set border for plot
  delta_x <- (max(x$dive2$hour) - min(x$dive1$hour)) * 0.1
  delta_y <- (max(depth(x)) - min(min(x$dive1$dtcurve$depths),
                                  min(x$dive2$dtcurve$depths))) * 0.2
  
  x$dive1$dtcurve$times <- x$dive1$dtcurve$times + x$dive1$hour[1]
  x$dive2$dtcurve$times <- x$dive2$dtcurve$times + x$dive2$hour[1] 
  
  default_par$xlim = c(min(x$dive1$dtcurve$times) - delta_x, 
             max(x$dive2$dtcurve$times) + delta_x)
  default_par$ylim = -c(max(x$dive1$dtcurve$depths, x$dive2$dtcurve$depths) 
                        + delta_y, 
              min(x$dive1$dtcurve$depths, x$dive2$dtcurve$depths) - delta_y)
  
  # TODO : why a duplicate here from line 457 ?
  # call_par <- list(...)
  
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
    call_par$col <- "#fd8f0c" # TODO here modify def col
    call_par$col.axis <- "#741d16"
    tmp_bg <- par('bg') # save bg for later
    par(bg = "#909fa1")
  } else {
    call_par$col.axis <- par('col.axis')
  }
  
  if (!add) {
    if(x$inter > cut_inter){
      # modify interv and xlim to cut the axis
      new_inter <- x$inter - cut_inter
      x$dive2$dtcurve$times <- x$dive2$dtcurve$times - new_inter
      # x$dive2$hour <- x$dive2$hour - new_inter
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
      colfunc <- colorRampPalette(c("#04728d", "#051c2e")) 
      # TODO change the colors here
      rect(par("usr")[1], 0, par("usr")[2], par("usr")[4], col = "#628db0")
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
    mtext(call_par$xlab, side=1, line=3, col=call_par$col.axis)
    mtext(call_par$ylab, side=2, line=3, col=call_par$col.axis)
    
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
  
  # Plot the dives ----
  consec_call <- call_par
  consec_call$add <- TRUE
  consec_call$cut_inter <- NULL
  consec_call$def_cols <- def_cols
  # if (x$type == 'consec' | x$dive1$hour[1] > 0){
  #   cat('consec_plot \n')
  #   consec_call$axes <- FALSE
  #   consec_call$hour_print <- FALSE
  # }
  consec_call$axes <- FALSE
  consec_call$hour_print <- FALSE
  
  consec_call$x <- x$dive1
  do.call(plot, consec_call)
  consec_call$x <- x$dive2
  do.call(plot, consec_call)
  
  hours <- c(x$dive1$hour[1], x$dive2$hour[2])
  dIvE <- list(hour = hours)
  inter_t <- c(tail(x$dive1$dtcurve$times, 1), x$dive2$dtcurve$times[1])
  # hours 
  if(hour_print){
    if(x$inter <= 15){
      text(
        x = c(dIvE$hour, x$dive1$hour[2]), y = 0,
        minute_to_time(c(dIvE$hour, x$dive1$hour[2]), sec = TRUE, sep = ':'),
        pos = 3, col = call_par$col
      )
    } else {
      w_hours <- c(inter_t[2], tail(x$dive2$dtcurve$times, 1))
      text( x = x$dive1$hour , y = 0, minute_to_time(x$dive1$hour),
            pos = 3, col = call_par$col)
      text(x = w_hours, y = 0, minute_to_time(x$dive2$hour), pos = 3, 
           col = call_par$col)
    }
  }
  
  if(call_par$axes){
    axis(2, col = call_par$col.axis, col.ticks = call_par$col.axis,
         col.axis = call_par$col.axis)
    if (x$inter <= cut_inter){
      cust_axis(dIvE, call_par$col.axis) #, shift = -x$dive1$hour[1] )
    } else {
      cust_axis(x$dive1, call_par$col.axis )
      cust_axis(x$dive2, call_par$col.axis, shift = -new_inter )
    }
  }
  
  if(x$inter > 15){
    text(
      x = mean(inter_t), y = 0,
      c(paste('inter:', minute_to_time(x$inter, day = F)),
        paste('maj:', x$dive2$params["maj"])), pos = c(3,1),
      col = call_par$col
    )
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

  if (def_cols) {par(bg = tmp_bg)}
}


#' plot.conso
#' 
#' Plot the dive curve and consumption depending on time and depth. 
#' Only represent sqare profile
#' 
#' @param x an object of class conso.
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
#' @param dive_print set to \code{TRUE} by default, whether there is the dive
#' curve printed.
#' @param rules_print set to \code{TRUE} by default, whether the rules are 
#' shown
#' @param hour_print set to \code{TRUE} by default, whether there is the hour
#' at beginning and end of the dive.
#' @param line_print set to \code{TRUE} by default, whether there is lines 
#' for depths.
#' @param depth_print set to \code{TRUE} by default, whether there is depth 
#' values along the dive.
#' @param time_print set to \code{TRUE} by default, whether there is time 
#' values along the dive.
#' @param def_cols set to \code{FALSE} by default, modify plot colors to match
#' shinyapp css.
#' @param legend set to \code{FALSE} by default, print legend colors for the 
#' tank.
#' @param add set to \code{FALSE} by default, to add another dive plot 
#' on a precedent one.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and 
#' \code{\link[graphics]{par}} for plot parameter that were omitted 
#' on this documentation
#' \item \code{\link[DiveR]{conso}} for every aspect about conso object 
#' creations.
#' }
#' 
#' @importFrom viridisLite viridis
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
plot.conso <- function(x,
                       ...,
                       dive_print = TRUE,
                       rules_print = TRUE,
                       hour_print = TRUE,
                       line_print = TRUE,
                       depth_print = TRUE,
                       time_print = TRUE,
                       def_cols = FALSE,
                       legend = TRUE,
                       add = FALSE) {
  
  Ltank <- ncol(x$vcons) - 2
  
  # modify time with first hour
  if (!add) {
    x$vcons$times <- x$vcons$times + x$hour[1]
    x$dtcurve$times <- x$dtcurve$times + x$hour[1]
  }
  
  # modify dtcurve to match pression
  b <- max(x$vcons[-c(1:2)], na.rm = TRUE) # find max pressure of all tanks
  a <- b/max(x$dtcurve$depths)
  raw_x <- x
  x$dtcurve$depths <- -a * x$dtcurve$depths + b
  x$desat$desat_stop$depth <-  -a * x$desat$desat_stop$depth + b
  
  # set global plot ----
  delta_x <- diff(x$hour) * 0.1
  delta_y <- (depth(x) - min(x$dtcurve$depths)) * 0.1
  default_par <- list(
    col = 1, dive_col = 'dimgrey', type = "l", axes = FALSE,
    xlab = "Time (min)", ylab = "Pressure (bar)", main = '',
    xlim = c(min(x$dtcurve$times) - delta_x, max(x$dtcurve$times) + delta_x),
    ylim = c(min(x$dtcurve$depths) - delta_y, max(x$dtcurve$depths) + delta_y)
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
    warning('log can not be used in this plot type') # TODO : testthat here !
  }
  # def bg cols ----
  if (def_cols) {
     call_par$col <- "#fd8f0c" # TODO modify def col
     call_par$dive_col <- '#909fa1'# 'peru'
     call_par$col.axis <- "#bd971e"
     tmp_bg <- par('bg') # save bg for later
     par(bg = "#073642")
   } else {
     call_par$col.axis <- par('col.axis')
   }
  
  if (!add) {
    # init plot ----
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"
    empty_par$axes=FALSE
    empty_par$dive_col = NULL
    empty_par$xlab = empty_par$ylab = empty_par$main = ''
    do.call(plot, empty_par)
    # def bg cols bis ----
    # TODO : here find cool stuff about def_cols in case of mult tanks
    # if (def_cols) {
    #   limits <- c(max(x$vpress), x$time_mid[1], 
    #               x$time_reserve[1], x$time_PA[1], 0)
    #   limits[is.na(limits)] <- 0
    #   cols <- c('#0571b0','#92c5de', '#f4a582', '#ca0020')
    #   for(i in 1:4){
    #     rect(x$hour[1], limits[i+1], x$hour[2], limits[i], 
    #          col = cols[i], border = cols[i])
    #   }
    # }
    # change color around and make axis ----
    box(col = call_par$col.axis)
    mtext(call_par$xlab, side=1, line=3, col=call_par$col)
    mtext(call_par$ylab, side=2, line=3, col=call_par$col)
    mtext(call_par$main, side=3, line=1, col=call_par$col)
    
    # add depth lines ----
    if (line_print) {
      for (i in seq(from = 0, to = depth(x), by = 10)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.1)
      }
      for (i in seq(from = 0, to = depth(x), by = 50)) {
        abline(h = i, lty = 2, col = call_par$col, lwd = 0.3)
      }
    }
  }
  
  if (call_par$axes){
    cust_axis(x, call_par$col.axis)
    axis(2, col = call_par$col.axis, col.ticks = call_par$col.axis, 
         col.axis = call_par$col.axis)
  } 
  
  if(dive_print){
    # print the dive line ----
    line_par <- c(list(x = x$dtcurve$times, y = x$dtcurve$depths), call_par)
    line_par$xlim <- NULL ; line_par$ylim <- NULL
    line_par$xlab <- NULL ; line_par$ylab <- NULL
    line_par$main <- NULL ; line_par$axes <- NULL
    line_par$col <- call_par$dive_col
    line_par$dive_col <- NULL
    line_par$lwd <- 0.5
    do.call(lines, line_par)
  }
  
  if(Ltank > 1){
    cols <- viridis(Ltank+1)
  } else {
    cols <- call_par$col
  }
  
  for( i in 3:ncol(x$vcons)){
    
    line_par <- c(list(x = x$vcons$times, y = x$vcons[,i]), call_par)
    line_par$xlim <- NULL ; line_par$ylim <- NULL
    line_par$xlab <- NULL ; line_par$ylab <- NULL
    line_par$main <- NULL ; line_par$axes <- NULL
    line_par$dive_col <- NULL
    line_par$col <- cols[i-2]
    do.call(lines, line_par)
    
    # add rules ablines
    if(rules_print){
      
      if(x$rules[i-2,1] == x$rules[i-2,4]){
        leg <- c(6, 9)
      }else{ 
        leg <- c(3, 6, 9)
      }
      
      for( ii in leg){
        if(!is.na(ii)){
          posx <- c(rep(x$rules[i-2,ii] + x$hour[1], 2), par("usr")[1])
          posy <- c(par("usr")[3], rep(x$rules[i-2,ii-2], 2))
          lines( x = posx, y = posy, lty = 3, col = cols[i-2] )
          legend(posx[2], posy[2], x$rules[i-2,ii-1], text.col = cols[i-2],
                 box.lty = 3, box.col = cols[i-2], bg = par('bg'),
                 xjust = 1, cex = 0.8)
        }
      }
    }
  }
  
  # hour points ----
  points(x = c(x$dtcurve$times[1], tail(x$dtcurve$times, 1)), 
         y = rep(depth(x), 2), pch = c(25, 24), 
         bg = rep(call_par$col, 2))
  if (hour_print){
    text(
      x = x$hour, y = depth(x),
      minute_to_time(x$hour, sec = TRUE, sep = ':'),
      pos = 3, col = call_par$col
    )
  }
  # depth and time info ----
  if(depth_print & dive_print){
    depth_lab <- depths_inf(raw_x, col = call_par$dive_col)
    depth_lab$y <- a * depth_lab$y + b
    do.call(text, depth_lab)
  }
  
  if(time_print & dive_print){
    time_lab <- times_inf(raw_x, col = call_par$dive_col)
    time_lab$y <- a * time_lab$y + b
    do.call(text, time_lab)
  }
  
  if(legend){
    legend('top',
           legend = rownames(x$rules),
           text.col = cols, cex = 0.8, horiz = TRUE)
  }
  
  if (def_cols) {par(bg = tmp_bg)}
}
