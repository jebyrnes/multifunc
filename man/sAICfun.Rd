% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sAICfun.R
\name{sAICfun}
\alias{sAICfun}
\title{sAICfun}
\usage{
sAICfun(
  response,
  species,
  data,
  positive.desired = TRUE,
  method = "lm",
  combine = "+",
  ...
)
}
\arguments{
\item{response}{Name of the response column}

\item{species}{Vector of column names of species}

\item{data}{data frame with species presence/abscence of values of functions}

\item{positive.desired}{Is a positive effect the desired sign.  Defaults to TRUE}

\item{method}{Fitting function for statistical models.  Defaults to \code{lm}.}

\item{combine}{How are species combined in the model? Defaults to "+" for additive combinations.}

\item{...}{Other arguments to be supplied to fitting function.}
}
\value{
Returns list of species with positive
negative or neutral contributions, the
relevant coefficient and effect matrices, and response name
}
\description{
\code{sAICfun} examines which species have an effect on which function using a stepwise AIC approach
}
\details{
\code{sAICfun} takes a dataset, response, and function, and then uses a stepAIC approach
to determine the best model.  From that it extracts the species with a positive,
negative, and neutral effect on that function.
}
\examples{
data(all_biodepth)
allVars <- qw(biomassY3, root3, N.g.m2, light3, N.Soil, wood3, cotton3)

germany <- subset(all_biodepth, all_biodepth$location == "Germany")

vars <- whichVars(germany, allVars)
species <- relevantSp(germany, 26:ncol(germany))

# re-normalize N.Soil so that everything is on the same
# sign-scale (e.g. the maximum level of a function is
# the "best" function)
germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)

spList <- sAICfun("biomassY3", species, germany)
# " spList
res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
names(res.list) <- vars



#########
# sAICfun takes a dataset, response, and function, and then uses a stepAIC approach
# to determine the best model.  From that it extracts the species with a positive,
# negative, and neutral effect on that function
#########
}
\author{
Jarrett Byrnes.
}
