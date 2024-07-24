#' Title
#'
#' @param method
#' @param frame
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
samp <- function(method, frame, ...) {
  # Check if a valid method is used
  method_str <- deparse(substitute(method))
  if (!(method_str %in% c("srs", "pps"))) {
    stop("invalid method ", method_str)
  }

  # run the specified method function using provided parameters
  do.call(method, list(frame, ...))
}
