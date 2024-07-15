#' @param frame
#'
#' @param stratid
#' @param nsamp
#' @param prn
#' @param size
#'
#' @export
pps <- function(frame, stratid, nsamp, prn, size) {
  # start by casting arguments as strings
  if (typeof(stratid) == "language") {
    stratid <- rlang::f_name(stratid)
  }
  if (typeof(nsamp) == "language") {
    nsamp <- rlang::f_name(nsamp)
  }
  if (typeof(prn) == "language") {
    prn <- rlang::f_name(prn)
  }
  if (typeof(size) == "language") {
    size <- rlang::f_name(size)
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
  if (!(size %in% names(frame))) {
    stop('variable "', size, '" not found on ', deparse(substitute(frame)))
  }

  # nsamp, prn, and size numeric variables
  if (mode(frame[, nsamp]) != "numeric") {
    stop("sample size variable ", nsamp, " is not numeric")
  }
  if (mode(frame[, prn]) != "numeric") {
    stop("PRN variable ", prn, " is not numeric")
  }
  if (mode(frame[, size]) != "numeric") {
    stop("size variable ", size, " is not numeric")
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

  # calculate the sum of the size in each stratum
  sum_frame <- setNames(aggregate(frame[size], frame[stratid], sum),
                        c(stratid, "sumsize"))
  frame <- merge(frame, sum_frame, by = stratid)
  # calculate a preliminary lambda for each item
  frame["lambda"] <- frame[nsamp] * frame[size] / frame["sumsize"]

  # if any lambda >= 1:
  if (any(frame["lambda"] >= 1)) {
    # calculate new sample size among units with lambda < 1
    n_frame <- aggregate(list(ntot = frame["lambda"] >= 1),
                         frame[stratid], sum)
    frame["nnew"] <- frame[nsamp] - merge(frame,
                                          n_frame,
                                          by = stratid)["lambda.y"]
    # remove the variable with sum of size
    if ("sumsize" %in% colnames(frame)) {
      frame["sumsize"] <- NULL
    }
    # extract units with lambda >= 1, set their lambda to 1
    # and mark them for sampling
    gtone <- subset(frame, frame$lambda >= 1)
    gtone$lambda <- rep(1, nrow(gtone))
    gtone$Q <- rep(0, nrow(gtone))
    gtone$sampled <- rep(TRUE, nrow(gtone))
    # remove the variable with new sample size from the {lambda >= 1} subset
    gtone["nnew"] <- NULL
    # run the entire function on the {lambda < 1} subset
    # with the new sample sizes
    ltone <- pps(subset(frame, frame$lambda < 1), stratid, ~nnew, prn, size)
    # remove the new sample sizes and return a concatenation of the subsets
    ltone["nnew"] <- NULL
    out_frame <- rbind(gtone, ltone)
    return(out_frame)
    # if no lambda >= 1
  } else {
    # calculate Q and sort along it for each stratum
    frame["Q"] <- frame[prn] * (1 - frame["lambda"]) /
      (frame["lambda"] * (1 - frame[prn]))
    order_frame <- unname(frame[, c(stratid, "Q")])
    frame <- frame[do.call(order, order_frame), ]
    # the nsamp with lowest Q for each stratum are marked for sampling
    frame$sampled <- sequence(rle(as.character(frame[, stratid]))$lengths) <=
      frame[nsamp]
    # remove the sum of the sizes and return the frame
    frame["sumsize"] <- NULL
    return(frame)
  }
}
