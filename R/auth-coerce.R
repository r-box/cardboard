#' Coerce to an auth function
#'
#' @param auth can be:
#'   - `function` with S3 class `cardboard_auth`: uses `auth`.
#'   - `character`: uses auth function using name of stored in keyring,
#'      see `bx_keyring_list()`.
#'   - `NULL`: uses default auth-function.
#'
#' @return `function` with S3 class `cardboard_auth`, fully operational
#' drop-in replacement for `httr2::req_oauth_auth_code()` or
#' `httr2::req_oauth_auth_credentials()`.
#'
#' @export
#'
bx_auth <- function(auth) {
  UseMethod("bx_auth")
}

#' @export
#'
bx_auth.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
bx_auth.cardbord_auth <- function(auth) {
  # no-op
  auth
}
