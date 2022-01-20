#' Get auth function
#'
#' @param auth can be:
#'   - `function` with S3 class `cardboard_auth`: uses `auth`.
#'   - `character`: uses auth function using name of stored in keyring,
#'      see `bx_keyring_list()`.
#'   - `list`: uses [bx_auth_parse()].
#'   - `NULL`: if `bx_keyring_list()` has exactly one entry, uses it.
#'
#' @return `function` with S3 class `cardboard_auth`, fully operational
#' drop-in replacement for `httr2::req_oauth_auth_code()` or
#' `httr2::req_oauth_auth_credentials()`.
#'
#' @export
#'
bx_auth_get <- function(auth) {
  UseMethod("bx_auth_get")
}

#' @export
#'
bx_auth_get.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
bx_auth_get.cardbord_auth <- function(auth) {
  # no-op
  auth
}

#' @export
#'
bx_auth_get.list <- function(auth) {
  rlang::exec(bx_auth_parse, !!!auth)
}

#' Coerce to auth function
#'
#' @param auth Object coerced to auth function.
#'   - If `NULL`, returns default auth-function returned by `bx_auth()`.
#'   - If `bx_auth()` returns `NULL`, tries [bx_auth_get()].
#'
#' @keywords internal
#'
as_auth <- function(auth) {
  auth <- auth %||% bx_auth()

  if (is.null(auth)) {
    message("Default auth not set, bx_auth_use()")
  }

  auth <- bx_auth_get(auth)

  auth
}

