#' to_list an auth function
#'
#' @inheritParams as_auth
#'
#' @return Named `list` with elements `type`, `params`.
#'   Returned invisibly because it contains secret information.
#' @keywords internal
#' @export
#'
bx_auth_to_list <- function(auth) {
  auth <- as_auth(auth)
  list_auth <- auth_to_list(auth)

  invisible(list_auth)
}

#' to_list an auth function
#'
#' @inheritParams bx_auth_get
#'
#' @return Named `list` with elements `type`, `params`.
#'   Returned invisibly because it contains secret information.
#' @keywords internal
#' @export
#'
auth_to_list <- function(auth) {
  UseMethod("auth_to_list")
}

#' @export
#'
auth_to_list.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
auth_to_list.cardbord_auth_interactive <- function(auth) {

  type <- "interactive"
  param_names <- c("id" , "secret", "cache_disk")

  to_list(auth, type, param_names)
}

#' @export
#'
auth_to_list.cardbord_auth_credentials <- function(auth) {

  type <- "credentials"
  param_names <- c("id" , "secret", "subject_type", "subject_id", "cache_disk")

  to_list(auth, type, param_names)
}

to_list <- function(auth, type, param_names) {

  list_auth <- as.list(environment(auth))
  params <- list_auth[param_names]

  invisible(
    list(type = type, params = params)
  )
}

#' Create an auth functiuon
#'
#'
#' @param list_auth `list`
#' @inherit bx_auth_create_interactive params return
#'
#' @keywords internal
#' @export
#'
bx_auth_from_list <- function(list_auth, .test = FALSE) {

  stopifnot(
    "list_auth is not a list" = is.list(list_auth),
    "'type' not in list_auth" = "type" %in% names(list_auth),
    "'params' not in list_auth" = "params" %in% names(list_auth),
    "'type' must be one of: 'interactive', 'credentials'" =
      list_auth[["type"]] %in% c("interactive", "credentials")
  )

  list_auth[["params"]][[".test"]] <- .test

  fn <- switch(
    list_auth[["type"]],
    interactive = bx_auth_create_interactive,
    credentials = bx_auth_create_credentials
  )

  auth <- rlang::exec(fn, !!!list_auth[["params"]])

  invisible(auth)
}
