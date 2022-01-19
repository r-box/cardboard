#' Serialize an auth function
#'
#' @inheritParams bx_auth
#'
#' @return Named `list` with elements `type`, `params`.
#' @export
#'
bx_auth_serialize <- function(auth) {
  UseMethod("bx_auth_serialize")
}

#' @export
#'
bx_auth_serialize.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
bx_auth_serialize.cardbord_auth_interactive <- function(auth) {

  type <- "interactive"
  param_names <- c("id" , "secret", "cache_disk")

  serialize(auth, type, param_names)
}

#' @export
#'
bx_auth_serialize.cardbord_auth_credentials <- function(auth) {

  type <- "credentials"
  param_names <- c("id" , "secret", "subject_type", "subject_id", "cache_disk")

  serialize(auth, type, param_names)
}

#' Inspect an auth function
#'
#' @inheritParams bx_auth
#'
#' @return Invisible `auth`, called for side effects.
#' @export
#'
bx_auth_inspect <- function(auth) {
  ser <- bx_auth_serialize(auth)
  ser$params$secret <- "<REDACTED>"

  print(glue("type: {ser[['type']]}"))
  print(glue("{names(ser[['params']])}: {unlist(ser[['params']])}"))

  invisible(auth)
}

serialize <- function(auth, type, param_names) {

  list_auth <- as.list(environment(auth))
  params <- list_auth[param_names]

  invisible(
    list(type = type, params = params)
  )
}
