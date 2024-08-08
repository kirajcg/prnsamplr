make_data <- function(nobs, nstrat, seed) {
  set.seed(seed)
  strat_len <- c(1, sort(floor(runif(nstrat, 1, nobs))), nobs)
  data_new <- data.frame(id = 1:nobs)

  for (i in 1:nstrat + 1) {
    data_new[strat_len[i]:strat_len[i + 1], "stratum"] <- paste("st",
      stringr::str_pad(toString(i), 5,
        side = "left",
        pad = "0"
      ),
      sep = ""
    )
  }

  data_new[1:strat_len[2] - 1, "stratum"] <- "st00001"
  strat_info <- as.data.frame(table(data_new$stratum))
  colnames(strat_info) <- c("stratum", "npopul")
  strat_info$nsample <- ceiling(runif(nrow(strat_info)) * strat_info$npop)

  out_data <- merge(data_new, strat_info, by = "stratum")
  out_data$rands <- runif(nrow(out_data))
  out_data$sizeM <- 10 * runif(nrow(out_data))
  return(out_data)
}

ExampleData <- make_data(40000, 99, 1)

usethis::use_data(ExampleData, overwrite = TRUE)
