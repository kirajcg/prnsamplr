test_that("pps samples the right number", {
  pps_frame <- pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  nsamp_list <- aggregate(pps_frame$nsample,
                          by = list(stratum = pps_frame$stratum),
                          FUN = unique)
  sampled_list <- aggregate(pps_frame$sampled,
                            by = list(stratum = pps_frame$stratum),
                            FUN = sum)
  expect_equal(nsamp_list$x, sampled_list$x)
})

test_that("pps calculates lambda correctly", {
  pps_frame <- pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  nsamp_list <- aggregate(pps_frame$nsample,
                          by = list(stratum = pps_frame$stratum),
                          FUN = unique)
  lambda_list <- aggregate(pps_frame$lambda,
                           by = list(stratum = pps_frame$stratum),
                           FUN = sum)
  expect_equal(nsamp_list$x, lambda_list$x)
})

test_that("error when stratid not found", {
  expect_error(pps(ExampleData, nsamp=~nsample, prn=~rands, size=~sizeM))
})

test_that("error when nsamp not found", {
  expect_error(pps(ExampleData, stratid=~stratum, prn=~rands, size=~sizeM))
})

test_that("error when prn not found", {
  expect_error(pps(ExampleData, stratid=~stratum, nsamp=~nsample, size=~sizeM))
})

test_that("error when size not found", {
  expect_error(pps(ExampleData, stratid=~stratum, nsamp=~nsample, prn=~rands))
})

test_that("warning when too many nsamp in one stratid", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- data_copy[1, "nsample"] + 1
  expect_warning(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
})

test_that("data.table input gives data.table output", {
  ExampleTable <- data.table::as.data.table(ExampleData)
  expect_s3_class(pps(ExampleTable, ~stratum, ~nsample, ~rands, ~sizeM),
                  "data.table")
})


test_that("tibble input gives tibble output", {
  ExampleTibble <- tibble::as_tibble(ExampleData)
  expect_s3_class(pps(ExampleTibble, ~stratum, ~nsample, ~rands, ~sizeM),
                  "tbl_df")
})
