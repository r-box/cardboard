#' Serialize an auth function
#'
#' @inheritParams as_auth
#'
#' @return Named `list` with elements `type`, `params`.
#'   Returned invisibly because it contains secret information.
#' @export
#'
bx_auth_serialize <- function(auth) {
  auth <- as_auth(auth)
}

#' Serialize an auth function
#'
#' @inheritParams bx_auth_get
#'
#' @return Named `list` with elements `type`, `params`.
#'   Returned invisibly because it contains secret information.
#' @keywords internal
#' @export
#'
auth_serialize <- function(auth) {
  UseMethod("auth_serialize")
}

#' @export
#'
auth_serialize.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
auth_serialize.cardbord_auth_interactive <- function(auth) {

  type <- "interactive"
  param_names <- c("id" , "secret", "cache_disk")

  serialize(auth, type, param_names)
}

#' @export
#'
auth_serialize.cardbord_auth_credentials <- function(auth) {

  type <- "credentials"
  param_names <- c("id" , "secret", "subject_type", "subject_id", "cache_disk")

  serialize(auth, type, param_names)
}

serialize <- function(auth, type, param_names) {

  list_auth <- as.list(environment(auth))
  params <- list_auth[param_names]

  invisible(
    list(type = type, params = params)
  )
}


#' Create an auth functiuon
#'
#' @param type `character`
#' @param params `list`
#' @inherit bx_auth_create_interactive params return
#'
#' @export
#'
bx_auth_parse <- function(type = c("interactive", "credentials"), params,
                         .test = FALSE) {

  type <- match.arg(type)
  params[[".test"]] <- .test

  fn <- switch(
    type,
    interactive = bx_auth_create_interactive,
    credentials = bx_auth_create_credentials
  )

  auth <- rlang::exec(fn, !!!params)

  return(auth)
}
