#' insertRow
#' 
#' Insert a row at a row number. This an intern function.
#' 
#' @param existingDF The data.frame to insert row in.
#' @param newrow row to insert. Must be of length == ncol(existingDF)
#' @param r place where to insert the row
#' 
#' @return data.frame
#' 
#' @examples 
#' existingDF <- as.data.frame(matrix(seq(20),nrow=5,ncol=4))
#' insertRow(existingDF, newrow = seq(4), r = 3)
#' 
#' @details 
#' copied from SO
#' \url{https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended}
#' 
#' @export
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


#' ceiling_dec
#' 
#' Ceiling at a specific decimal level
#' 
#' @param x values to round. Must be numeric
#' @param level decimal to round to. Default is 1
#' 
#' @return data.frame
#' 
#' @examples 
#' ceiling_dec(1.259, 2)
#' ceiling_dec(1.9, 0)
#' ceiling_dec(29, -1)
#' ceiling_dec(1.251, 2)
#' ceiling_dec(1.2, 0)
#' ceiling_dec(23, -1)
#' 
#' @details 
#' copied from SO
#' \url{https://stackoverflow.com/questions/35807523/r-decimal-ceiling}
#' 
#' @export
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

