#' @param frame
#'
#' @param prn
#' @param direction
#' @param start
#'
#' @export
transformprn <- function(frame, prn, direction, start) {
  # start by casting the prn argument as string
  if (typeof(prn) == "language") {
    prn <- rlang::f_name(prn)
  }

  # Check for PRN's on frame
  if (!(prn %in% names(frame))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(frame)))
  }

  # start point needs to be between 0 and 1
  if (start < 0) {
    stop("start point cannot be less than 0")
  }
  if (start > 1) {
    stop("start point cannot be greater than 1")
  }

  # save the original prn's
  frame[["prn.old"]] <- frame[[prn]]
  # transform the prn's according to one operation if counting should go up,
  # another if counting should go down
  # and not at all in any other case
  if (toupper(substr(direction, 1, 1)) %in% c("U", "R")) {
    frame[[prn]] <- (frame[["prn.old"]] - start + 1) %% 1
  } else if (toupper(substr(direction, 1, 1)) %in% c("D", "L")) {
    frame[[prn]] <- 1 - ((frame[["prn.old"]] - start + 1) %% 1)
  } else {
    warning(
      '"', direction,
      '" is not a valid direction. ',
      'Please specify either "U" or "R" for up, or either "D or "L" for down.'
    )
  }
  return(frame)
}
