#' Test an auth function
#'
#' @inheritParams as_auth
#'
#' @return Invisible `logical`, indicating success
#' @export
#'
bx_auth_test <- function(auth = NULL) {

  auth <- as_auth(auth)

  me <-
    httr2::request(url_box_api("users/me")) %>% # replace with bx_req
    auth() %>%
    httr2::req_perform() %>% # replace with bx_req_run
    httr2::resp_body_json()

  name <- me[["name"]] %||% "<not available>"
  id <- me[["id"]] %||% "<not available>"
  login <- me[["login"]] %||% "<not available>"
  message(
    glue("{name} (id: {id}, login: {login}) authorized to Box API.")
  )

  invisible(TRUE)
}

#' Inspect an auth function
#'
#' @inheritParams as_auth
#' @inherit bx_auth_get params return
#'
#' @export
#'
bx_auth_inspect <- function(auth = NULL) {
  auth <- as_auth(auth)
  auth_inspect(auth)
}

#' Inspect an auth function
#'
#' @inherit bx_auth_get params return
#'
#' @keywords internal
#' @export
#'
auth_inspect <- function(auth) {
  UseMethod("auth_inspect")
}

#' @export
#'
auth_inspect.default <- function(auth) {
  stop(
    glue(
      "No method for class: {glue_collapse(class(auth), sep = ' ')}."
    ),
    call. = FALSE
  )
}

#' @export
#'
auth_inspect.cardbord_auth_interactive <- function(auth) {

  env <- environment(auth)
  env_params <- as.list(env)[c("auth_url", "cache_disk")]

  cat("client:\n")
  print(env[["client"]])
  cat(
    "",
    glue("{names(env_params)}: {env_params}"),
    sep = "\n"
  )

  invisible(auth)
}

#' @export
#'
auth_inspect.cardbord_auth_credentials <- function(auth, ...) {

  env <- environment(auth)

  token_params <- env[["token_params"]]

  cat("client:\n")
  print(env[["client"]])
  cat(
    "",
    "token_params:",
    glue("  {names(token_params)}: {token_params}"),
    glue("cache_disk: {env[['cache_disk']]}"),
    sep = "\n"
  )

  invisible(auth)
}
