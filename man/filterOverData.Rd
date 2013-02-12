\name{filterOverData}
\alias{filterOverData}
\title{filterOverData}
\usage{
  filterOverData(overData, type = "positive")
}
\arguments{
  \item{overData}{Matrix of functions and which species
  affect them from \code{getRedundancy}.}

  \item{type}{Are the kinds of effects we're looking at
  "positive", "negative" or "all".}
}
\value{
  Returns a filtered matrix.
}
\description{
  \code{filterOverData} and \code{filterCoefData} filter
  contributions of species to function by sign.
}
\details{
  Takes a matrix of functions and coefficients for species
  - either 1's and -1's, or just continuous coefficients -
  and filters out only the sign of contributions desired.
  Typically used by other functions in the package.
}
\examples{
data(all_biodepth)
allVars<-qw(biomassY3, root3, N.g.m2,  light3, N.Soil, wood3, cotton3)

germany<-subset(all_biodepth, all_biodepth$location=="Germany")

vars<-whichVars(germany, allVars)

#re-normalize N.Soil so that everything is on the same sign-scale (e.g. the maximum level of a function is the "best" function)
germany$N.Soil<- -1*germany$N.Soil +max(germany$N.Soil, na.rm=T)

res.list<-lapply(vars, function(x) sAICfun(x, species, germany))
names(res.list)<-vars

redund<-getRedundancy(vars, species, germany)

filterOverData(redund, type="positive")
}
\author{
  Jarrett Byrnes.
}

