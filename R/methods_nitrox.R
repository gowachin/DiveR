#' Set a gas for dive saturation
#' 
#' Gas for dive. The values are taken into account for tissue saturation models.
#' 
#' @param ppo2 partial percentage of dioxygen in breathed mixture. in percentage
#' @param ppn2 partial percentage of nitrogen in breathed mixture. in percentage
#' @param name default is empty and will be deducted from the ppo2 as "NXppo2". 
#' This is equivalent to "EANppo2"
#' 
#' @details Sum of ppn2 and ppo2 must be 1. There is two cases :
#' if the sum is superior to 0, the function will trigger an error; 
#' if the sum is inferior to 1, a gas "other" will compensate and the function
#' will only throw a warning.
#' 
#' 
#' @rdname gas
#' @examples
#' gas()
#' gas(0.32, 0.68, name = "NX32")
#' gas(0.32, 0.68)
#' 
#' @export
gas <- function(ppo2 = 0.209, ppn2 = 0.791, name = character(0)){
  
  assertNumber(ppo2, lower = 0, upper = 1)
  assertNumber(ppn2, lower = 0, upper = 1)
  
  all_gas <- ppo2 + ppn2
  oth <- 0
  
  if(all_gas < 1){
    warning(paste(
      "sum of all gases is < 1.",
      "A other gas not included in computation is added to fill the tanks."
    ))
    oth <- 1 - (all_gas)
  } else if (all_gas > 1){
    stop("sum of all gases is > 1.")
  }
  assertCharacter(name, pattern = "^(^AIR)|(((NX)|(EAN))[[:digit:]]{1,3})$",
                  min.len = 0, max.len = 1, any.missing = FALSE)
  
  if(length(name) == 0){
    name <- paste0("NX", ceiling(ppo2 * 100))
  }
  
  res <- data.frame(ppo2 = ppo2, ppn2 = ppn2, oth = oth, name = name, 
                    stringsAsFactors = FALSE)
  class(res) <- c("gas", class(res))
  return(res)
}

#' Coerce to gas
#' 
#' @param x R object in numeric,  character
#' 
#' @details Will call gas function, so names and will be 
#' deducted from the ppo2 as "NXppo2".
#' 
#' @rdname as.gas
#' @examples 
#' as.gas(NULL)
#' 
#' @export
as.gas <- function(x){
  if (is.null(x)){
    return(data.frame(ppo2 = numeric(0), ppn2 = numeric(0), 
                      oth = numeric(0), name = character(0))
    )
  }
  UseMethod("as.gas")
}

# TODO : write as.gas.data.frame
# TODO : write as.gas.list

#' @rdname as.gas
#' @examples
#' as.gas(gas())
#' 
#' @export
as.gas.gas <- function(x){
  return(x)
}

#' @rdname as.gas
#' @examples
#' as.gas(c(ppo2 = 0.3, ppn2 = 0.7))
#' 
#' @export
as.gas.numeric <- function(x){
  
  assertNumeric(x, lower = 0, upper = 1, len = 2)
  if(any(!c("ppo2", "ppn2") %in% names(x))){
    stop("names of x must be at least 'ppo2' and 'ppn2'")
  }
  
  res <- gas(ppo2 = x[["ppo2"]], ppn2 = x[["ppn2"]])
  return(res)
}

#' @rdname as.gas
#' @examples
#' as.gas("AIR")
#' as.gas("EAN32")
#' as.gas("NX36")
#' 
#' @export
as.gas.character <- function(x){
  
  assertCharacter(x, pattern = "^(^AIR)|(((NX)|(EAN))[[:digit:]]{1,3})$",
                  len = 1, any.missing = FALSE)
  
  if(x == "AIR"){
    return(gas(name = x))
    # x <- "NX21
  } 
  
  ppo2 <- as.numeric(
    sub("^([[:alpha:]]{2,3})([[:digit:]]{1,3})$", "\\2", x)
  ) / 100
  
  res <- 
    return(
      gas(ppo2 = ppo2, ppn2 = 1-ppo2, name = x)
    )
}
