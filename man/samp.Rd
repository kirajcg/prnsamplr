\name{samp}
\alias{samp}
\title{PRN Sampling}

\description{
  Wrapper for PPS and SRS sampling using permanent random numbers.
}

\usage{
samp(method, df, ...)
}

\arguments{
  \item{method}{pps or srs}
  \item{df}{Data frame containing the elements to sample from.}
  \item{...}{Further method-specific arguments.}
}

\value{
  Copy of the input data frame together with the boolean variable sampled,
  as well as the numeric variable lambda for PPS case.
}

\examples{
  samp(method=pps, df=myDataFrame, )
  samp(method=pps, df=myDataFrame, nsamp="nsamp", stratid="nace", size="turn", prn="prn")
  samp(method=srs, df=myDataFrame, nsamp="nsamp", stratid="nace", prn="prn")
}