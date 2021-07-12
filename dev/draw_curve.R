
library(DiveR)
#' 2 curseurs, temps max et precision
maxt <- 50
prec <- 5
maxd <- 25

curve <- data.frame(depths = 0,times = seq(0, maxt, by = prec), loc = FALSE )
curve$loc[1] <- TRUE
curve

trial <- TRUE
while(trial){
  d <- dive(depth = curve$depths, time = curve$times)
  plot(d, ylim = c(-maxd, 10), depth_print = FALSE, time_print = FALSE)
  # plot(curve$times, -curve$depths, type = 'l', ylim = c(-maxd, 10))
  points(curve$times[curve$loc], -curve$depths[curve$loc],pch  = 3, col = 'red')
  abline(h = 0, col = "green")
  # lines(curve$times, -curve$depths)
  
  p <- locator(1)
  # print(p)
  
  if (p$y < 0){
    prox <- abs(curve$times - p$x)
    pos <- which.min(prox)
    
    curve$depths[pos] <- -p$y
    curve$loc[pos] <- TRUE
    
    # if (pos > 1 & any(curve$depths[1:pos-1] > 0) ){
    #   first <- max(which(curve$depths[1:pos-1] > 0 & curve$loc[1:pos-1]))
    #   round <- (first + 1) : (pos - 1)
    #   reg <- lm(curve$depths[c(first, pos)]~ curve$times[c(first, pos)])
    #   curve$depths[round] <- reg$coefficients[2] * curve$times[round] + reg$coefficients[1]
    # }
    
  } else {
    trial <- FALSE
  }
}





