makeData <- function(nobs, nstrat, seed) {
  set.seed(seed)
  stratlen <- c(1, sort(floor(runif(nstrat, 1, nobs))), nobs)
  DataNew <- data.frame(id = 1:nobs)

  for (i in 1:nstrat + 1) {
    DataNew[stratlen[i]:stratlen[i + 1], "stratum"] <- paste("st",
      stringr::str_pad(toString(i), 5,
        side = "left",
        pad = "0"
      ),
      sep = ""
    )
  }

  DataNew[1:stratlen[2] - 1, "stratum"] <- "st00001"
  StratInfo <- as.data.frame(table(DataNew$stratum))
  colnames(StratInfo) <- c("stratum", "npopul")
  StratInfo$nsample <- ceiling(runif(nrow(StratInfo)) * StratInfo$npop)

  outData <- merge(DataNew, StratInfo, by = "stratum")
  outData$rands <- runif(nrow(outData))
  outData$sizeM <- 10 * runif(nrow(outData))
  return(outData)
}

ExampleData <- makeData(40000, 99, 1)

usethis::use_data(ExampleData, overwrite = TRUE)
