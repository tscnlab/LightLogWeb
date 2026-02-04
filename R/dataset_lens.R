dataset_lens <- function(datasets, key) {
  req(datasets, key())
  list(
    get = reactive(datasets[[key()]]),
    set = function(value) { datasets[[key()]] <- value }
  )
}
