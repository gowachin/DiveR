% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conso.R
\name{conso}
\alias{conso}
\title{conso}
\usage{
conso(dive, tank, cons = 20, failure_label = "AF")
}
\arguments{
\item{dive}{\code{\link[DiveR]{dive}} object.}

\item{tank}{\code{\link[DiveR]{tank}} object or a list of tank objects.
Priority of consumption for tanks is set by their order in list.}

\item{cons}{Litre per minute breathed by diver. Single numeric positive value.
20 L/min by default}

\item{failure_label}{Label for when a tank is a empty. Single character
string. 'AF' by default.}
}
\value{
conso, a conso class object.

back <- tank(12, 200, 
             rules = list(rules = c('retour' = 150, 'reserve' = 100),
                          sys = "bar"))
back15 <- tank(15, 200,
               rules = list(rules = c('retour' = 150, 'reserve' = 100),
                            sys = "bar"))
relay <- tank(12, 200,
             rules = list(rules = c('retour' = 120, 'reserve' = 120),
                          sys = "bar"), typ = 'relay')
dive <- dive(20, 40)

# Deadly dive as warnings will tell
conso(dive, back)
# Safe dive here
conso(dive, back15)
# Multiple tank dive
conso(dive, list(relay, back)
}
\description{
conso
}
\author{
Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
}
