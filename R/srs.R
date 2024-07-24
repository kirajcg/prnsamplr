#' Title
#'
#' @param frame
#' @param stratid
#' @param nsamp
#' @param prn
#'
#' @return
#' @export
#'
#' @examples
srs <- function(frame, stratid, nsamp, prn) {
  UseMethod("srs")
}

#' @export
srs.data.frame <- function(frame, stratid, nsamp, prn) {
  # start by casting each argument as strings
  if (is.language(stratid)) {
    stratid <- rlang::f_name(stratid)
  }
  if (is.language(nsamp)) {
    nsamp <- rlang::f_name(nsamp)
  }
  if (is.language(prn)) {
    prn <- rlang::f_name(prn)
  }

  # Check for each variable on frame
  if (!(stratid %in% names(frame))) {
    stop('variable "', stratid, '" not found on ', deparse(substitute(frame)))
  }
  if (!(nsamp %in% names(frame))) {
    stop('variable "', nsamp, '" not found on ', deparse(substitute(frame)))
  }
  if (!(prn %in% names(frame))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(frame)))
  }

  # nsamp and prn numeric variables
  if (!is.numeric(frame[[nsamp]])) {
    stop("sample size variable ", nsamp, " is not numeric")
  }
  if (!is.numeric(frame[[prn]])) {
    stop("PRN variable ", prn, " is not numeric")
  }

  # Only one nsamp for each stratid
  strat_info <- unique(frame[, c(stratid, nsamp)])
  n_strat_nsamp <- data.frame(table(strat_info[, stratid]))
  non_unique_nsamp <- n_strat_nsamp[n_strat_nsamp$Freq > 1, ]
  if (nrow(non_unique_nsamp) > 0) {
    problematic_strata <- non_unique_nsamp$Var1
    warning(
      stratid, " with names ", paste(problematic_strata, collapse = ", "),
      " have more than one corresponding value of ", nsamp
    )
  }

  # Each prn between 0 and 1
  prn_below_zero <- frame[frame[, prn] < 0, ]
  if (nrow(prn_below_zero) > 0) {
    warning(
      prn, " less than 0 found at rows ",
      paste(row.names(prn_below_zero), collapse = ", ")
    )
  }
  prn_above_one <- frame[frame[, prn] > 1, ]
  if (nrow(prn_above_one) > 0) {
    warning(
      prn, " greater than 1 found at rows ",
      paste(row.names(prn_above_one), collapse = ", ")
    )
  }

  # sort the frame along stratum and PRN's
  order_frame <- unname(frame[, c(stratid, prn)])
  out_frame <- frame[do.call(order, order_frame), ]
  # the nsamp first objects in each stratum are marked for sampling
  out_frame$sampled <- sequence(rle(as.character(frame[, stratid]))$lengths) <=
    out_frame[, nsamp]

  return(out_frame)
}

#' @export
srs.data.table <- function(frame, stratid, nsamp, prn) {
  return(NextMethod() |> data.table::as.data.table())
}

#' @export
srs.tbl_df <- function(frame, stratid, nsamp, prn) {
  return(NextMethod() |> tibble::as_tibble())
}
