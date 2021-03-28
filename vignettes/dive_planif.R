## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6.1,
  fig.width = 8.5,
  dpi = 96
)
#library(details)
library(viridisLite)

## ----basic_dive---------------------------------------------------------------
library(DiveR)
dive20_40 <- dive(depth = 20, time = 40)

## ----secu_curve,  dev='png', out.width="100%", echo = FALSE-------------------
nodeco <- time <- depth <- 1:65
for(i in depth){
  time[i] <- max_depth_time(depth[i])
  nodeco[i] <- max_depth_time(depth[i], force = TRUE, no_deco = TRUE)
}

plot(time, -depth, type = "l", xlim = c(0, max(time)), 
     xlab = "Time (min)", ylab = "Depth (meter)", 
     main = "Maximum time in table with desat stop (Deco) and without")
lines(nodeco, -depth, type = "l")
cols = viridis(3)
polygon(c(rep(0, length(nodeco)), rev(nodeco)), c(-depth, -rev(depth)), col = cols[2])
polygon(c(time, rev(nodeco)), c(-depth, -rev(depth)), col = cols[1])

legend("bottomright", legend = c("Deco", "No-deco time"), fill = (cols[-3]))
# df <- rbind(rev(nodeco),rev(time-nodeco))
# colnames(df) <- rev(depth)
# 
# barplot(df, horiz = TRUE, col = rev(viridis(2)))

## ----lim funct, error=TRUE----------------------------------------------------
### check if in table
tablecheck(20, 50)
tablecheck(20, 80)
tablecheck(20, 80, force = TRUE)
### checking time
max_depth_time(depth = 20)
max_depth_time(depth = 20, no_deco = TRUE)

## ----basic_tank---------------------------------------------------------------
library(DiveR)
tank1 <- tank(vol = 12, press = 200)
tank1

