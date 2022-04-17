#' # exploratory work for rcpp insertRow ####
#' 
#' #' insertRow
#' #' 
#' #' Insert a row at a row number. This an intern function.
#' #' 
#' #' @param existingDF The data.frame to insert row in.
#' #' @param newrow row to insert. Must be of length == ncol(existingDF)
#' #' @param r place where to insert the row
#' #' 
#' #' @return data.frame
#' #' 
#' #' @examples 
#' #' existingDF <- as.data.frame(matrix(seq(20),nrow=5,ncol=4))
#' #' insertRow(existingDF, newrow = seq(4), r = 3)
#' #' 
#' #' @details 
#' #' copied from SO
#' #' \url{https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended}
#' #' 
#' #' @export
#' insertRow <- function(existingDF, newrow, r) {
#'   existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
#'   existingDF[r,] <- newrow
#'   existingDF
#' }
#' 
#' existingDF <- data.frame(n1 = c(1.0,2.0,3.0),
#'                          i1 = c(1,2,3),
#'                          c1 = c("A", "B", "C"),
#'                          l1 = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
#' newr <- data.frame(n1 = c(42.0),
#'                    i1 = c(42),
#'                    c1 = c("I"),
#'                    l1 = c(TRUE), stringsAsFactors = FALSE)
#' (x <- cpp_insertRow(existingDF, newr, 4))
#' (x <- cpp_insertRow(existingDF, newr, 5))
#' insertRow(existingDF, newrow = newr, r = 3)
#' 
#' # insertRow(existingDF, newrow = seq(4), r = 3)
#' (x <- cpp_insertRow(existingDF, newrow = newr, r = 3))
#' existingDF 
#' 
#' microbenchmark::microbenchmark(
#'   r = insertRow(existingDF, newrow = newr, r = 3),
#'   cpp = cpp_insertRow(existingDF, newrow = newr, r = 3),
#'   times = 100
#' )
#' test_that("insertRow", {
#'   existingDF <- as.data.frame(matrix(as.numeric(1:12),nrow=3,ncol=3))
#'   expect_identical(insertRow(existingDF, newrow = seq(3), r = 3), 
#'                    as.data.frame(matrix(c(1,2,1,3:5,2,6:8,3,9), nrow = 4, ncol = 3)))
#' })