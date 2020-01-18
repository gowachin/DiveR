
# Hudson data preparation
library(Template)
#file at : https://github.com/stan-dev/example-models/blob/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv
Hudson <-
  read.csv("raw_data/hudson-bay-lynx-hare.csv",
           comment.char="#")

lynx = population("lynx",4,1,60)
hare = population("hare",30,2,80)
lynx[[2]]=Hudson$Lynx
hare[[2]]=Hudson$Hare
lynx[[3]] = hare[[3]] = Hudson$Year
hudson = community('Hudson',hare,lynx)

usethis::use_data(hudson, overwrite = TRUE)
#data("hudson")
#plot(hudson)

#ou alors utiliser le package IBM pour produire les datas!!!
