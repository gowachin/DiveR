#' MN90 Tables
#' 
#' A dataset containing an array with the desaturation stop time depending on depth 
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
#' depth can not return values. See \code{\link[DiveR]{tablecheck}} for 
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
#' A dataset containing an data.frame with the group letter depending on depth 
#' and time.
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
#' A dataset containing an data.frame with the residual nitrogen in blood depending on the interval time and the group.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"nitrogen"


#' MN90 table
#'
#' A dataset containing an data.frame with the time majoration depending on 
#' depth of second dive and residual nitrogen.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' Trucco, Jean-Noël; Biard, Jef; Redureau, Jean-Yves; Fauvel, Yvon (3 May 1999). 
#' pdf in french :
#' \href{http://renaud.helstroffer.free.fr/sub_technique/fiche_initiation/mn90.pdf}{"Table Marine National 90 (MN90): Version du 03/05/1999"}
"maj"


#' Haldane S values
#'
#' A dataset containing different data.frames for Haldane S values.
#' 
#' @format  Each data.frame has a column 'comp' for the half-life of the compartment, 
#' and a column 'Scomp' for the S value of the compartment. For a data.frame
#' of HaldaneN there is N rows.
#' 
#' @details 
#' \itemize{
#'  \item{Haldane5 - Compartments from 5 to 60 minutes}
#'  \item{Haldane12 - Compartments from 5 to 120 minutes}
#' }
#' 
#' @note For the Haldane5 :
#' There is a modification of the last compartment, with an half-life
#' of 60 instead of 75 min. This is because the Sc value was not found for such
#' time.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @source 
#' 
#' @rdname Haldane_Svalues
"Haldane5"


#' @rdname Haldane_Svalues
"Haldane12"