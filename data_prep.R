### examples ####
library(mn90)

pa20 <- dive(depth = 19, time = 43, secu = TRUE, vup = 10)
plot(pa20)
usethis::use_data(pa20, overwrite = TRUE)

#### Table preparation ##### 
# pdf at : http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf

# convert hour to minutes
h <- function(x,m = 0){
  return(x*60+m)
}

# possible depth
prof <- c(6,8,10, 
          12,15, 18, 20, 
          22, 25, 28, 30, 
          32, 35, 38, 40, 
          42, 45, 48, 50, 
          52, 55, 58, 60, 
          62, 65)
# possible times per depth
time <- c(15,30,45,h(1,15),h(1,45),h(2,15),h(3), # 6
          15, 30, 45,h(1),h(1,30),h(1,45),h(2,15),h(2,45), # 8
          15, 30, 45, h(1),h(1,15),h(1,45), h(2),h(2,15),h(2,45), # 10
          15, 25, 35, 45, 55, h(1),h(1,5),h(1,20), h(1,30),h(1,45), h(2),h(2,20),h(2,40), # 12
          5, 10,20, 25, 35, 40, 50, h(1),h(1,10),h(1,20), h(1,25), h(1,30), h(1,35), h(1, 40),h(1,45), h(2), # 15
          5, 10, 15,20, 25, 35, 40, 50,55, h(1,5),h(1,10),h(1,15),h(1,20), h(1,25), h(1,30), # 18
          5, 10, 15,20, 25,30,35, 40,45, 50,55,h(1),h(1,5),h(1,10),h(1,15), # 20
          10, 15,20, 25,30,35, 40,45, 50,55,h(1), # 22
          5,10, 15,20, 25,30,35, 40,45, 50, # 25
          5,10, 15,20, 25,30,35, 40,45, # 28
          5,10, 15,20, 25,30,35, 40,45, # 30
          5,10, 15,20, 25,30,35, 40,45, # 32
          5,10, 15,20, 25,30,35, 40, # 35
          5,10, 15,20, 25,30,35,  # 38
          5,10, 15,20, 25,30,35, # 40
          5,10, 15,20, 25,30, # 42
          5,10, 15,20, 25,30, # 45
          5,10, 15,20, 25, # 48
          5,10, 15,20, 25, # 50
          5,10, 15,20, 25, # 52
          5,10, 15,20, 25, # 55
          5,10, 15,20, 25, # 58
          5,10, 15,20, 25, # 60
          5,10, 15, # 62
          5,10, 15 # 65
)
# max time per depth
mt <- c(h(3), # 6
        h(2,45), # 8
        h(2,45), # 10
        h(2,40), # 12
        h(2), # 15
        h(1,30), # 18
        h(1,15), # 20
        h(1), # 22
        50, # 25
        45, # 28
        45, # 30
        45, # 32
        40, # 35
        35,  # 38
        35, # 40
        30, # 42
        30, # 45
        25, # 48
        25, # 50
        25, # 52
        25, # 55
        25, # 58
        25, # 60
        15, # 62
        15 # 65)
)

ut <- sort(unique(time))
table <- matrix(NA, ncol = length(ut), nrow = length(prof),
                dimnames = list(prof,ut))
table

for(i in 1:length(mt)){
  table[i,1:which(colnames(table) == as.character(mt[i]))] = 0
}

write.csv(table, "raw_data/raw_table.csv")

m3 <- read.csv("raw_data/raw_m3.csv")[,-1] ; m3
m6 <- read.csv("raw_data/raw_m6.csv")[,-1] ; m6
m9 <- read.csv("raw_data/raw_m9.csv")[,-1] ; m9
grp <- read.csv("raw_data/raw_grp.csv", stringsAsFactors = F)[,-1] ; grp

table <- array(unlist(list(m3,m6,m9)), dim = c(length(prof), length(ut), 3), dimnames = list(prof,ut,c("m3","m6","m9")))
table
dimnames(grp) <- list(prof,ut)

# azote table
n2 <- matrix(NA,ncol = 13, nrow = 16, 
             dimnames = list(LETTERS[1:16],
                             c(15,30,45,h(1),h(1,30), h(2),h(2,30),h(3),h(4), h(6),h(8),h(10),h(12))))
write.csv(n2, "raw_data/raw_n2.csv")

azote <- read.csv("raw_data/raw_azote.csv")[,-1] ; azote
colnames(azote) <- as.numeric(sub("X","",x =colnames(azote)))
rownames(azote) <- LETTERS[1:nrow(azote)]

maj <- read.csv("raw_data/raw_maj.csv", header = TRUE) ; maj
colnames(maj) <- as.numeric(sub("X","",x =colnames(maj)))
rownames(maj) <- maj[,1] ; maj <- maj[,-1]
maj

# save all 
usethis::use_data(table, overwrite = TRUE)
usethis::use_data(grp, overwrite = TRUE)
usethis::use_data(azote, overwrite = TRUE)
usethis::use_data(maj, overwrite = TRUE)

rm(m3,m6,m9,grp,i, mt, prof, table, time, ut, n2, azote)
