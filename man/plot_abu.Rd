% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobr.R
\name{plot_abu}
\alias{plot_abu}
\title{Plot distributions of species abundance}
\usage{
plot_abu(mob_in, env_var, type = c("sad", "rad"), pooled = FALSE,
  col = NULL, lwd = 3, log = "", leg_loc = "topleft")
}
\arguments{
\item{mob_in}{a 'mob_in' class object produced by 'make_mob_in'}

\item{env_var}{a string that specifies the column name in mob_in$env that
specifies the grouping variable.}

\item{type}{either 'sad' or 'rad' for species abundance vs rank abundance
distribution}

\item{pooled}{boolean specifying if abundances should be pooled at the group
level or not}

\item{col}{optional vector of colors.}

\item{lwd}{a vector of line widths, see \code{\link[graphics]{par}}.}

\item{log}{a character string which contains \code{"x"} if the x axis
    is to be logarithmic, \code{"y"} if the y axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}

\item{leg_loc}{the location of the legend. Defaults to 'topleft', see
\code{\link[graphics]{legend}}. If set to NA then no legend is printed.}
}
\description{
Plot distributions of species abundance
}
\examples{
data(inv_comm)
data(inv_plot_attr)
inv_mob_in = make_mob_in(inv_comm, inv_plot_attr)
plot_abu(inv_mob_in, 'group', type='sad', pooled=FALSE, log='x')
plot_abu(inv_mob_in, 'group', type='rad', pooled=TRUE, log='x')
}
