

#' @param vol tank volume in litre
#' @param press tank pression in bar
#' @param rules tank rules to watch during a dive. A list of two named element :
#' \describe{
#'   \item{"rules"}{vector of 2 named integer indicating a percentage 
#'   or a pression. The named will be used in the plot function later}
#'   \item{"sys"}{character string, either '%' or 'bar'}
#' }
#' @param gas tank gas, by default "Air"
#' @param typ tank type, by default "back"
#' \describe{
#'   \item{"solo"}{single tank}
#'   \item{"bi"}{two separated tanks}
#'   \item{"relay"}{single tank to be dropped at certain time}
#'   \item{"deco"}{single tank to be used in deco ascent}
#' }
#' @param limit a two element vector with times between which the tank 
#' is not used. Used to mimic an accident, or a relay tank.
nbloc <- function(vol, press, rules = list(rules = c('mid' = 50,'res' = 25), 
                                           sys = '%' ), 
                  gas = "Air", typ = "back", limit = NULL){
  
  # modify rules to bar !
  if(rules$sys == "%"){
    rules$rules <- press * rules$rules / 100
  } else {
    # check the values are under press !!
  }
  
  if(is.null(limit)){
    limit <- rep(NA,2)
  }
  
  carac <- c(vol, press, unlist(rules$rules))
  names(carac) <- c('vol', 'press', 'rule1', 'rule2')
  typo <- c(gas, typ, names(rules$rules))
  names(typo) <- c('gas', 'typ', 'rule1', 'rule2')
  
  limit <- c(0,60,limit)
  names(limit) <- c('mind', 'maxd', 't1', 't2')
  
  bloc <- list(carac = carac, typo = typo, limit = limit)
  class(bloc) <- "bloc"
  return(bloc)
}

nbloc(12, 200)
