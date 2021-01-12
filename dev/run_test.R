
# running tests
library(DiveR)
# plot.dive ####
d <- dive(20, 40, secu = T, hour = 10*60 + 24.5)
plot(d, def_cols = T)
# plot.ndive ####
par(mfrow = c(4, 2))
d <- ndive(dive1 = dive(18,11), dive2 = dive(15,40), inter = 1)
cat(d$inter,'\n')
plot(d, def_cols = T, main = 'consec') # consec

d <- ndive(dive1 = dive(35,11), dive2 = dive(30,20), inter = 4)
plot(d, def_cols = T, xlab = 'hello world', main = 'no_consec') # no_consec # SOLO

d <- ndive(dive1 = dive(18,40), dive2 = dive(15,30), inter = 60)
cat(d$inter,'\n')
plot(d, main = 'success no cut') # succes no cut

d <- ndive(dive1 = dive(20,40), dive2 = dive(20,30), inter = 121)
cat(d$inter,'\n')
plot(d, main = 'success cut') # sucess cut

d <- ndive(dive1 = dive(18,40), dive2 = dive(35,30), inter = 120)
plot(d, def_cols = T, main = 'maj_no_success') # maj_no_success # SOLO

d <- ndive(dive1 = dive(62,11), dive2 = dive(15,30), inter = 120)
plot(d, def_cols = T, main = '60_no_sucess') # 60_no_sucess # SOLO

d <- ndive(dive1 = dive(18,11), dive2 = dive(47,8), inter = 730)
cat(d$inter,'\n')
plot(d, main = 'diff cut', def_cols = T) # diff cut
plot(1, axes = F, type = 'n', xlab = '', ylab = '')
par(mfrow = c(1,1))