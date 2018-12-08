srs <- function(df, stratid, nsamp, prn) {
  df <- df[with(df, order(df[, stratid], df[, prn])), ]
  df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[, nsamp]
  return(df)
}
