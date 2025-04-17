# prnsamplr <img src="man/figures/logo.png" align="right" height="138" alt="" /> [![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

The goal of prnsamplr is to coordinate survey samples with the help of permanent random numbers. 

## Installation

You can install the released version of prnsamplr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("prnsamplr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(prnsamplr)

SampledData <- samp(method=pps, df=ExampleData, nsamp=~nsample, stratid=~stratum, prn=~rands, size=~sizeM)
```

## News

In version 1.1.0 a functionality breaking bug in srs() was fixed.

In version 1.0.0 was implemented a major overhaul of how the package is organized, as well as a change in how functions are called. S3 methods were also implemented.

In version 0.3.0 the documentation was better developed and informative error messages were added.

In version 0.2.1 an error in the documentation was fixed, and some code in pps was edited to make the output more consistent.

In version 0.2.0 a compatibility issue with the latest R was fixed. 

In version 0.1.1 an issue was fixed that disallowed generic function parameters. 
