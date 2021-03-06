% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_conso.R
\name{rules}
\alias{rules}
\alias{rules.tank}
\alias{rules.conso}
\title{rules}
\usage{
rules(object, ...)

\method{rules}{tank}(object, ...)

\method{rules}{conso}(object, ..., hour = FALSE)
}
\arguments{
\item{object}{is a DiveR object. There is methods for 
\code{\link[DiveR]{tank}} and \code{\link[DiveR]{conso}} objects.}

\item{...}{other parameters passed to the pressure.conso method.
\describe{
  \item{tankn}{the number of the tank if there is multiple. NULL by default
  will return all the tanks}
  \item{n}{the number of the rule. 0 mean the air-failure, 1 rule1 and 
  2 rule2. NULL by default will get all of them. A vector of values can 
  be provided}
}}

\item{hour}{if the time defined inside the hours of the dive (TRUE) or 
in relative time to dive start (FALSE). Set to FALSE by default.}
}
\value{
\describe{
  \item{for \code{\link[DiveR]{tank}}}{numeric vector for rules in their
   order}
  \item{for \code{\link[DiveR]{conso}}}{data.frame with rules in column and
  tanks in rows}
}
 
 @examples 
 t <- tank(12,200)
 rules(object = t)
 
 d <- dive(20,15)
 c <- conso(d, t, cons = 20)
 rules(object = c)
 
 rules(object = c, n = 0)
}
\description{
\code{rules} retrieve the rules of a tank or
pressure and times at which the pressure of tank(s) met the rules in conso.
}
\author{
Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
}
