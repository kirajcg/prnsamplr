\name{prnsamplr-package}
\alias{prnsamplr-package}
\alias{prnsamplr}
\docType{package}
\title{
\packageTitle{prnsamplr}
}
\description{
\packageDescription{prnsamplr}
}
\details{
This package provides two functions for drawing stratified PRN-assisted samples: \code{srs} and \code{pps}. The former -- simple random sampling -- assumes that every unit in a given stratum is equally likely to be sampled. It samples the \eqn{n_h} elements with the smallest PRN's, for each stratum \eqn{h}. The latter -- Pareto \eqn{\pi ps} sampling -- assumes that large units are more likely to be sampled than small units. The function approximates this unknown inclusion probability \eqn{\lambda} and samples the \eqn{n_h} elements with the smallest values of \deqn{Q = \frac{prn(1 - \lambda)}{\lambda(1 - prn)},} for each stratum \eqn{h}.

These two functions can be run standalone or via the wrapper function \code{samp}. Input to the functions is the sampling frame, stratification information and PRN's given as variables on the frame, and in the case for \code{pps} also a size measure given as variable on the frame. Output is a copy of the sampling frame containing sampling information, and in the case for \code{pps} also containing \eqn{\lambda} and \eqn{Q}.

Provided is also a function \code{transformprn} via which it is possible to select where to start counting and in which direction when enumerating the PRN's in the sampling routines. This is done by specifying start and direction to \code{transformprn} and then calling \code{srs} or \code{pps} on its output.

Finally, an example dataset is provided that can be used to illustrate the functionality of the package.
}
\author{
\packageAuthor{prnsamplr}

Maintainer: \packageMaintainer{prnsamplr}
}
\references{
Lindblom, Annika. "On Precision in Estimates of Change over Time where Samples are Positively Coordinated by Permanent Random Numbers" Journal of Official Statistics, vol.30, no.4, 2014, pp.773-785. https://doi.org/10.2478/jos-2014-0047
}
\keyword{ package }
\seealso{
  \code{\link{srs}},
  \code{\link{pps}},
  \code{\link{samp}},
  \code{\link{transformprn}}.
}
\examples{
dfSRS <- srs(df=ExampleData, 
             nsamp="nsample", 
             stratid="stratum", 
             prn="rands")

dfPPS <- pps(df=ExampleData, 
             nsamp="nsample", 
             stratid="stratum", 
             prn="rands", 
             size="sizeM")

dfPRN <- transformprn(df=ExampleData, 
                      prn="rands", 
                      direction="U", 
                      start=0.2)
}
