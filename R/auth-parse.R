#' Create an auth functiuon
#'
#' @param type `character`
#' @param params `list`
#' @inherit bx_auth_create_interactive params return
#'
#' @export
#'
bx_auth_parse <-function(type = c("interactive", "credentials"), params,
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
