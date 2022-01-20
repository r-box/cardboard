#' Default auth function
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
