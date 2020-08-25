#' Minesweeper
#' 
#' Try to clear all paths without jumping on a mine.
#' You need to click on the cells to discover the plot round after round.
#'
#' @param N the number of mines. 20 by default.
#' @param X size of the matrix. 20 by default.
#' @param Y sive of the matrix. 20 by default.
#' @param verbose FALSE by default.
#'
#' 
#' @export
minesweeper <- function(N=20,X=20,Y=20, verbose = FALSE){
  
  if(N>(X*Y)){
    stop('There is too many bombs on this map ! Game over')
  }
  
  if(X > 30 | Y > 30){
    stop('The field is too big to be generated whithout a cup of tea')
  }
  
  field <- creation(n=N,x=X,y=Y)
  names(field) <- c("prox","know","reveal",'plot')
  
  if(sum(field$prox == 0)){
    zonal <- expand(field,N,X,Y)
  }
  pits <- unlist(lapply(zonal, '[[',1))
  zon <- Reduce('+',lapply(zonal, '[[', 4))
  
  library(plot.matrix)
  game <- TRUE
  plot(field$reveal,col = c("darkgray","lightgray"), key = NULL,
       axis.col=NULL, axis.row=NULL,
       main = paste("Minesweeper :",N,"mines",sep =" "),
       xlab =" ", ylab = " ")
  if(verbose){
    text(as.numeric(col(field$prox)),rev(as.numeric(row(field$prox))),as.character(field$prox))
  }
  
  while(game != FALSE){
    
    try <- round(unlist(locator(1)))
    try[2] <- abs((Y+1)-try[2])
    value <- field$know[try[2],try[1]] <- field$prox[try[2],try[1]]
    field$reveal[try[2],try[1]] <- TRUE
    
    if(value == -1){
      game <- FALSE
      
      plot(field$reveal,col = c("darkred","lightgray"), key = NULL,
           axis.col=NULL, axis.row=NULL,
           main = "YOU'RE DEAD",
           xlab =" ", ylab = " ")
      text(as.numeric(col(field$prox)),rev(as.numeric(row(field$prox))),as.character(field$prox))
      
      Sys.sleep('1')
      
      graphics.off()
      stop("\n Game over! \n")
    } else if(value %in% c(1:8)){
      # cat("You survived")
    } else if(value == 0){
      if(length(pits)> 1){
        if(zon[try[2],try[1]] %in% pits){
          zones <- zonal[[which(pits==zon[try[2],try[1]])]]$zon != 0 
        } else {
          zones <- field$reveal
        }
      } else {
        zones <- zonal[[1]]$zon != 0
      }
      field$reveal <- matrix(as.logical(field$reveal+zones),ncol = X)
      field$know[field$reveal] <- field$prox[field$reveal]
    }
    
    plot(field$reveal,col = c("darkgray","lightgray"), key = NULL,
         axis.col=NULL, axis.row=NULL,
         main = paste("Minesweeper :",N,"mines",sep =" "),
         xlab =" ", ylab = " ")
    field$plot <- field$know
    field$plot[which(field$plot==0)] <- NA
    
    if(verbose){
      text(as.numeric(col(field$prox)),rev(as.numeric(row(field$prox))),as.character(field$prox))
    } else {
      text(as.numeric(col(field$plot)),rev(as.numeric(row(field$plot))),as.character(field$plot))
    }
    
    if(sum(is.na(field$know)) == N){
      game <- FALSE
      cat('\n You won! The map will close in 3s \n')
      
      plot(field$reveal,col = c("darkgreen","lightgray"), key = NULL,
           axis.col=NULL, axis.row=NULL,
           main = 'YOU WON',
           xlab =" ", ylab = " ")
      text(as.numeric(col(field$prox)),rev(as.numeric(row(field$prox))),as.character(field$prox))
      
      Sys.sleep('2')
      
      graphics.off()
    }
  }
}


creation <- function(n=5,x=5,y=5){
  info  <- matrix(FALSE,ncol = x, nrow = y)
  pres  <- info

  while(sum(info) < n){
    Xpos <- sample(c(1:x),1)
    Ypos <- sample(c(1:y),1)
    if(!info[Ypos,Xpos]) info[Ypos,Xpos]  <- TRUE
  }

  info <- cbind(rep(FALSE,y),
                info,
                rep(FALSE,y))
  info <- rbind(rep(FALSE,x+2),
                info,
                rep(FALSE,x+2))
  repres <- info #  as.numeric(info)

  for(i in 2:(y+1)){
    for(j in 2:(x+1)){
      tmp <- info[c((i-1):(i+1)),c((j-1):(j+1))]
      repres[i,j] <- sum(tmp)
      #print(c(i,j))
    }
  }
  repres[info] <- -1
  repres <- repres[-c(1,x+2),-c(1,y+2)]
  info <- info[-c(1,x+2),-c(1,y+2)]

  know <- repres
  know[] <- NA

  save = list(repres,know,pres,know)
}



expand <- function(field,N,X,Y){
  matrix <- field$prox
  matrix <- cbind(rep(-2,Y),matrix,rep(-2,Y))
  matrix <- rbind(rep(-2,X+2),matrix,rep(-2,X+2))
  empty <- field$reveal
  empty <- cbind(rep(1,Y),empty,rep(1,Y))
  empty <- rbind(rep(1,X+2),empty,rep(1,X+2))
  # plot(matrix)
  # text(as.numeric(col(matrix)),rev(as.numeric(row(matrix))),as.character(matrix))

  coord <- which(matrix == 0, arr.ind=TRUE)
  # system.time({
  # zonal <- list() ; length(zonal) <- sum(matrix==0)
  # })
  zonal <- vector('list',sum(matrix==0))
  k <- 2

  system.time({
    for(i in 1:length(zonal)){
      zonal[[i]] <- list(k,row = coord[i,1],col = coord[i,2],
                         zon = empty)
      zonal[[i]]$zon[zonal[[i]]$row,zonal[[i]]$col] <- k
      k <- k+1
    }

  })

  # system.time({
  for(i in 1:length(zonal)){
    tmp <- zonal[[i]]
    tval <- matrix
    tk <- tmp$zon[tmp$row,tmp$col]

    cy <- tmp$row ; cx <- tmp$col

    secu <- matrix(ncol=2)

    n=1
    while(!is.null(cx) & n < X*Y ){
      n=n+1
      around <- tmp$zon[(cy-1):(cy+1),(cx-1):(cx+1)]
      values <- tval[(cy-1):(cy+1),(cx-1):(cx+1)]
      tmp$zon[cy,cx] <- tk
      values[2,2] <- -2
      ty <- row(tval[(cy-1):(cy+1),(cx-1):(cx+1)])+cy-2
      tx <- col(tval[(cy-1):(cy+1),(cx-1):(cx+1)])+cx-2

      texp <- which(values == 0, arr.ind=TRUE)
      tfix <- which(values > 0, arr.ind=TRUE)

      if(nrow(tfix)>0){
        for(j in 1:nrow(tfix)){
          tmp$zon[ty[tfix[j,1],tfix[j,2]],tx[tfix[j,1],tfix[j,2]]] <- tk
        }
      }
      if(nrow(texp)>0){
        tval[cy,cx] <- -2
        cy <- ty[texp[1,1],texp[1,2]]
        cx <- tx[texp[1,1],texp[1,2]]

        if(nrow(texp)>1){
          transfer <- matrix(c(ty[texp[-1,1],1],tx[1,texp[-1,2]]),ncol =2)
          secu <- rbind(secu,transfer)
        }
      }else{
        secu <- matrix(unique(secu[-1,1:2]),ncol=2)
        cy <- cx <- NULL
        n2 =1
        while(is.null(cy) & nrow(secu) > 0 & n2 < X*Y ){
          n2 <- n2+1
          if(tmp$zon[secu[1,1],secu[1,2]] != tk){
            cy <- secu[1,1]
            cx <- secu[1,2]
            secu <- matrix(secu[-1,],ncol=2)
          } else {
            secu <- matrix(secu[-1,],ncol=2)
            cy <- cx <- NULL
          }
        }
      }
    }
    zonal[[i]]$zon <- tmp$zon
  }
  # })

  recup <- list(zonal[[1]])

  m = 2
  for(i in 2:length(zonal)){
    cy <- zonal[[i]]$row ; cx <- zonal[[i]]$col
    test <- 0 ; try <- 0
    for(l in 1:(m-1)){
      if(recup[[l]]$zon[cy,cx] > 0){
        try = try + 1
      } else {
        try = try + 1
        test <- test + 1
      }
    }
    if(test/try == 1 ){recup[[m]] <- zonal[[i]] ; m <- m +1}
  }
  zonal <- recup

  for(i in 1:length(zonal)){
    table <- zonal[[i]]$zon
    zonal[[i]]$zon <- table[-c(1,nrow(table)),-c(1,ncol(table))]
  }

  zonal
}

# minesweeper(40,20,20, verbose = FALSE)