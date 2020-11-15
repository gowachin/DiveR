#' MN90 Tables
#' 
#' A dataset containing an array with the palier time depending on depth 
#' and time and a data.frame with the group.
#'
#' @format  table is an array of dimension 25, 27 and 3:\cr
#' \itemize{
#'  \item{[x,,] - depths (meters).}
#'  \item{[,y,] - times (minutes).}
#'  \item{[,,z] - duration of decompression stop at certain depth. 
#'  For values of z from 1 to 0, there is duration at respectively 3, 
#'  6 and 9 meters stop.}
#' }
#' 
#' @details 
#' As the table is limited in row and column, some values of time and 
#' depth can not return values. See \code{\link[mn90]{tablecheck}} for 
#' limit values of depth and time.
#' Also you can note that part of the table are NA values. 
#' This mean that the couple of dive parameters are not included in this model.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' 
#' @examples 
#' data(table)
#' # here an example where there can be times at differents depths or not. 
#' table[16:19,1:8,]
#' 
#' @source
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"table"

#' Deco Group
#' 
#' A dataset containing an array with the palier time depending on depth 
#' and time and a data.frame with the group.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"grp"

#' MN90 table
#'
#' A dataset containing an array with the palier time depending on depth 
#' and time and a data.frame with the group.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"azote"


#' MN90 table
#'
#' A dataset containing an array with the palier time depending on depth 
#' and time and a data.frame with the group.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"maj"