to_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE)
}

from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
}
