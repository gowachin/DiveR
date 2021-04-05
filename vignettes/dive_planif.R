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

## ----basic_dive,  dev='png', out.width="100%"---------------------------------
library(DiveR)
dive20_40 <- dive(depth = 20, time = 40)
plot(dive20_40, line_print = FALSE)

## ----secu_curve,  dev='png', out.width="100%", echo = FALSE-------------------
nodeco <- time <- depth <- 1:65
for(i in depth){
  time[i] <- max_depth_time(depth[i])
  nodeco[i] <- max_depth_time(depth[i], force = TRUE, no_deco = TRUE)
}
cols = viridis(3)
# plot(time, -depth, type = "l", xlim = c(0, max(time)), 
#      xlab = "Time (min)", ylab = "Depth (meter)", 
#      main = "Maximum time in table with desat stop (Deco) and without")
# lines(nodeco, -depth, type = "l")
# polygon(c(rep(0, length(nodeco)), rev(nodeco)), c(-depth, -rev(depth)), col = cols[2])
# polygon(c(time, rev(nodeco)), c(-depth, -rev(depth)), col = cols[1])

df <- rbind(rev(nodeco),rev(time-nodeco))
colnames(df) <- -rev(depth)
barplot(df, horiz = TRUE, col = rev(cols[-3]), yaxt = "n", 
     xlab = "Time (min)", ylab = "Depth (meter)", 
     main = "Maximum time in table with desat stop (Deco) and without")
axis(2, at = seq(0, par("usr")[4]- 3, length.out = 14), labels = seq(-65, -0, by = 5))
legend("bottomright", legend = c("No-deco time" , "Deco"), fill = rev(cols[-3]))

## ----lim funct, error=TRUE----------------------------------------------------
### check if in table
tablecheck(20, 50)
tablecheck(20, 80)
tablecheck(20, 80, force = TRUE)
### checking time
max_depth_time(depth = 20)
max_depth_time(depth = 20, no_deco = TRUE)

## ----ghost_dive, echo = FALSE-------------------------------------------------
ghost_dive <- dive(depth = 39, time = 22, secu = FALSE)

## ----depth_dive---------------------------------------------------------------
depth(dive20_40)
depth(ghost_dive) # it is important when depth is not in the name !

## ----dtime_dive---------------------------------------------------------------
dtime(dive20_40)
dtime(ghost_dive) # it is important when depth is not in the name !
# Difference with underwater time
diff(dive20_40$hour)
dtime(dive20_40) + dtr(dive20_40) == diff(dive20_40$hour)

## ----depth_time_dive----------------------------------------------------------
depth_at_time(dive20_40, 15)
depth_at_time(dive20_40, 43) # during desat
depth_at_time(dive20_40, 50) # after the dive
depth_at_time(ghost_dive, 15)

## ----summary_dive-------------------------------------------------------------
summary(dive20_40)
summary(ghost_dive)

## ----inde_dive,  dev='png', out.width="100%"----------------------------------
dive1 <- dive(20, 40)
dive2 <- dive(20, 40)

diff_dive <- ndive(dive1, dive2, inter = 721)
plot(diff_dive)

## ----success_dive,  dev='png', out.width="100%", error=TRUE-------------------
succ_dive <- ndive(dive1, dive2, inter = 214.8) # this is just round hours ^^
plot(succ_dive)

## ----no_success_dive,  dev='png', out.width="100%", error=TRUE----------------
no_succ_dive <- ndive(dive1, dive2, inter = 30)
plot(no_succ_dive)

## ----force_success_dive,  dev='png', out.width="100%"-------------------------
maj <- majoration(depth = 20, group = dive1$desat$group, inter = 30)
maj
max_time <- max_depth_time(depth = 20) - maj
max_time
max_succ_dive <- ndive(dive1, dive(20, max_time), inter = 30)
plot(max_succ_dive)

## ----consec_dive,  dev='png', out.width="100%", warning=TRUE------------------
dive1 <- dive(20, 40)
dive2 <- dive(10, 5)
cons_dive <- ndive(dive1, dive2, inter = 10) # this is just round hours ^^
plot(cons_dive)

