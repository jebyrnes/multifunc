\name{filterCoefData}
\alias{filterCoefData}
\title{filterCoefData}
\usage{
  filterCoefData(coefData, type = "positive")
}
\arguments{
  \item{coefData}{Matrix of functions and coefficients for
  which species affect them from \code{getRedundancy}.}

  \item{type}{Are the kinds of effects we're looking at
  "positive", "negative" or "all".}
}
\value{
  Returns a filtered matrix.
}
\description{
  \code{filterCoefData} filters contributions of species to
  function by sign.
}
\details{
  Takes a matrix of functions and coefficients for species
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

stdCoefs<-stdEffects(coefs, germany, vars, species)

filterCoefData(stdCoefs)
}
\author{
  Jarrett Byrnes.
}

