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
#' @importFrom viridis viridis
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
  raw_dtcurve <- x$dtcurve
  x$dtcurve$depths <- -a * x$dtcurve$depths + b
  
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
  # if (def_cols) {
  #   call_par$col <- "#fd8f0c" # TODO modify def col
  #   call_par$dive_col <- '#909fa1'# 'peru'
  #   call_par$col.axis <- "#bd971e"
  #   tmp_bg <- par('bg') # save bg for later
  #   par(bg = "#073642")
  # } else {
    call_par$col.axis <- par('col.axis')
  # }
  
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
  
  # # print the pression line 
  if(Ltank > 1){
    cols <- viridis(Ltank+1)
  } else {
    cols <- "black"
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
    depth_lab <- depths_inf(list(dtcurve = raw_dtcurve), col = call_par$dive_col)
    depth_depth <- depths_inf(list(dtcurve = x$dtcurve), col = call_par$dive_col)
    depth_lab$y <- c(0, -depth_depth$y[2])
    do.call(text, depth_lab)
  }

  if(time_print & dive_print){
    time_lab <- times_inf(list(dtcurve = raw_dtcurve), col = call_par$dive_col)
    time_depth <- times_inf(list(dtcurve = x$dtcurve), col = call_par$dive_col)
    time_lab$y <- c(0, -depth_depth$y[2])
    do.call(text, time_lab)
  }
  
  if(legend){
    legend('top',
           legend = rownames(x$rules),
           text.col = cols, cex = 0.8, horiz = TRUE)
  }
  
  if (def_cols) {par(bg = tmp_bg)}
}