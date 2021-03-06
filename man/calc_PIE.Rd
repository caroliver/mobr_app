% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobr_boxplots.R
\name{calc_PIE}
\alias{calc_PIE}
\title{Calculate probability of interspecific encounter (PIE)}
\usage{
calc_PIE(x, ENS = FALSE)
}
\arguments{
\item{x}{can either be a: 1) mob_in object, 2) community matrix-like
object in which rows represent plots and columns represent species, or 3)
a vector which contains the abundance of each species.}

\item{ENS}{boolean that determines if the effective number of species should
be returned or the raw PIE value. Defaults to FALSE}
}
\description{
PIE is also known as Simpson's evenness index and the Gini-Simpson index.
The formula of Hurlbert (1971) is used to calculate PIE:
}
\details{
\eqn{PIE = N /(N - 1) * (1 - p_i^2)}

where N is the total number of individuals and \eqn{p_i} is the relative abundance
of species i. This formulation uses sampling without replacement and it is
sometimes referred to as the bias corrected formulation of PIE.

The code in this function borrows heavily from the function vegan::diversity()
but computes a different quantity. The function vegan::diversity() computes
PIE when sampling with replacement is assumed. The difference between the two 
formulations will decrease as N becomes large. Jari Oksanen and Bob O'Hara are
the original authors of the function vegan::diversity().
}
\examples{
data(inv_comm)
calc_PIE(inv_comm)
calc_PIE(inv_comm, ENS=TRUE)
}
\references{
Hurlbert, S. H. (1971) The nonconcept of species diversity: a critique and
 alternative parameters. Ecology 52, 577–586.
}
\author{
Dan McGlinn
}
