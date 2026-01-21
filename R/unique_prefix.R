min_unique_prefix_length <- function(x) {
  n <- nchar(x)
  for (k in seq_len(max(n))) {
    prefixes <- substr(x, 1, k)
    if (length(unique(prefixes)) == length(x)) {
      return(k)
    }
  }
  NA_integer_
}
