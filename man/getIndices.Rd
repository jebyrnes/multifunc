\name{getIndices}
\alias{getIndices}
\title{getIndices}
\usage{
  getIndices(slopedata, threshdata, eqn, fun = glm,
    divvar = "Diversity", groupVar = "thresholds",
    showNfunc = T)
}
\arguments{
  \item{slopedata}{A data frame with slopes of the
  relationship between diversity and number of functions
  greather than or equal to a threshold from
  \code{getCoefTab}.}

  \item{threshdata}{A data frame with the number of
  functions greater than a threshold for each plot at each
  threshold from \code{getFuncsMaxed}.}

  \item{eqn}{The formula used for fitting the models in
  slopedata.}

  \item{fun}{The function used to refit the threshold data
  at key points to get intercepts, etc., that are needed
  for the table.}

  \item{divvar}{The name of the variable that has the
  measure of diversity or other driver in the threshdata
  data frame.}

  \item{groupVar}{The name of a variable by which data is
  grouped in the threshdata data frame. Typically
  "thresholds" from \code{getFuncsMaxed}.}

  \item{showNfunc}{Show the functions at Tmin, Tmax, and
  Tmde. Defaults to TRUE.}
}
\value{
  A data frame of indices
}
\description{
  \code{getIndices} Generates a variety of indices
  describing multifunctionality based on the number of
  functions greater than a threshold for many different
  threshold and coefficients describing the relationship
  between diversity and number of functoins greater than a
  threshold.
}
\details{
  See Byrnes et al. In Review.
}
\examples{
data(all_biodepth)
allVars<-qw(biomassY3, root3, N.g.m2,  light3, N.Soil, wood3, cotton3)

germany<-subset(all_biodepth, all_biodepth$location=="Germany")

vars<-whichVars(germany, allVars)

germanyThresh<-getFuncsMaxed(germany, vars, threshmin=0.05, threshmax=0.99, prepend=c("plot","Diversity"), maxN=7)

germanyLinearSlopes<-getCoefTab(funcMaxed ~ Diversity, data=germanyThresh, coefVar="Diversity", family=quasipoisson(link="identity"))

getIndices(germanyLinearSlopes, germanyThresh, funcMaxed ~ Diversity)
}
\author{
  Jarrett Byrnes.
}

