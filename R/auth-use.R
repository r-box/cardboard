#' Default auth function
#'
#' @description
#' This package lets you set a *default* auth function.
#' Use these functions to accesss this function:
#'
#'  - `bx_auth_use()`: to set the default function
#'  - `bx_auth()`: to get the default function
#'
#' @inherit bx_auth_get params return
#'
#' @export
#'
bx_auth_use <- function(auth) {

  auth <- bx_auth_get(auth)
  bx_env[["auth"]] <- auth

  invisible(auth)
}

#' @rdname bx_auth_use
#' @export
#'
bx_auth <- function() {
  bx_env[["auth"]]
}
