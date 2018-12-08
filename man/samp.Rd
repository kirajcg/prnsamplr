\name{samp}
\alias{samp}
\title{PRN Sampling}
\usage{
samp(method, df, ...)
}
\description{
Wrapper for PPS and SRS sampling.

Input:
  method: pps or srs.
  df: Data frame containing the elements to sample from.
  nsamp: Column containing sample sizes.
  stratid: Column containing stratum information.
  size: Size measure, in the PPS case.

Output:
  Copy of the input data frame together with the boolean variable sampled,
  as well as the numeric variable lambda for PPS case.
}
