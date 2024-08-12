test_that("srs samples the right number", {
  srs_frame <- srs(ExampleData, ~stratum, ~nsample, ~rands)
  nsamp_list <- aggregate(srs_frame$nsample,
                          by = list(stratum = srs_frame$stratum),
                          FUN = unique)
  sampled_list <- aggregate(srs_frame$sampled,
                            by = list(stratum = srs_frame$stratum),
                            FUN = sum)
  expect_equal(nsamp_list$x, sampled_list$x)
})

test_that("error when stratid not found", {
  expect_error(srs(ExampleData, nsamp=~nsample, prn=~rands))
})

test_that("error when nsamp not found", {
  expect_error(srs(ExampleData, stratid=~stratum, prn=~rands))
})

test_that("error when prn not found", {
  expect_error(srs(ExampleData, stratid=~stratum, nsamp=~nsample))
})

test_that("warning when too many nsamp in one stratid", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- data_copy[1, "nsample"] + 1
  expect_warning(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("data.table input gives data.table output", {
  ExampleTable <- data.table::as.data.table(ExampleData)
  expect_s3_class(srs(ExampleTable, ~stratum, ~nsample, ~rands), "data.table")
})


test_that("tibble input gives tibble output", {
  ExampleTibble <- tibble::as_tibble(ExampleData)
  expect_s3_class(srs(ExampleTibble, ~stratum, ~nsample, ~rands), "tbl_df")
})
