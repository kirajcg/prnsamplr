## code to prepare `ExampleData` dataset goes here


# turn this into a function, move the library line
library(stringr)


nobs <- 40000

nstrat <- 99

set.seed(1)

stratlen <- c(1, sort(floor(runif(nstrat, 1, nobs))), nobs)

DataNew <- data.frame(id=1:nobs)

for (i in 1:nstrat+1){
  DataNew[stratlen[i]:stratlen[i+1], "stratum"] <- paste('st',str_pad(toString(i), 5, side = "left", pad = "0"), sep='')
}

DataNew[1:stratlen[2]-1, "stratum"] = 'st00001'


StratInfo <- as.data.frame(table(DataNew$stratum))


colnames(StratInfo) <- c("stratum", "npopul")


StratInfo$nsample <- ceiling(runif(nrow(StratInfo)) * StratInfo$npop)


ExampleData <- merge(DataNew, StratInfo, by="stratum")

ExampleData$rands <- runif(nrow(ExampleData))

ExampleData$sizeM <- 10*runif(nrow(ExampleData))

usethis::use_data(ExampleData, overwrite = TRUE)
