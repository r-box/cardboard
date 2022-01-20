#' Make an auth function
#'
#' - RMarkdown files are generally rendered in their own R session. By caching
#' the token, you could first authenticate to Box interactively.
#' - if you use the default (`NULL`) for `id` and `secret`, you will be asked
#' interactively. This may be preferable because the `id` and `secret` will not
#' be saved to your command history.
#' - `"user"` does not seem to work for credentials-based auth.
#'
#' @param id `character` client id, taken from Box app.
#' @param secret `character` client secret, taken from Box app.
#' @param cache_disk `logical` for code-based auth, indicates if the token
#'   should be cached on disk for use in other R sessions.
#' @param subject_type `character` for credentials-based auth, the type of
#'   account to use.
#' @param subject_id `character` for credentials-based auth, the id of the
#'   account to use.
#' @param .test `logical` indicates to test by calling the `users/me` resource.
#'
#' @return `function` that takes only an `httr2::request` object and returns a
#'   `httr2::request` object. This function can be used place of
#'   `httr2::req_oauth_auth_code()` or `httr2::req_oauth_auth_credentials()`.
#' @export
#'
bx_auth_create_interactive <- function(id = NULL, secret = NULL,
                                       cache_disk = FALSE, .test = TRUE) {

  # create client
  client <- make_client(id, secret)

  auth_url = "https://app.box.com/api/oauth2/authorize"

  # create auth function
  req_auth <- function(req) {
    httr2::req_oauth_auth_code(
      req,
      client,
      auth_url = auth_url,
      cache_disk = cache_disk
    )
  }

  if (.test) {
    bx_auth_test(req_auth)
  }

  class(req_auth) <- c("cardbord_auth_interactive", "cardbord_auth")
  req_auth
}

#' @rdname bx_auth_create_interactive
#' @export
#'
bx_auth_create_credentials <- function(id = NULL, secret = NULL,
                                       subject_type = c("enterprise", "user"),
                                       subject_id = NULL,
                                       cache_disk = FALSE, .test = TRUE) {

  # create client
  client <- make_client(id, secret)

  subject_type <- match.arg(subject_type)
  subject_id <-
    subject_id %||%
    askpass::askpass(
      glue("Please enter Box app subject_id ({subject_type} id):")
    )

  token_params <- list(
    box_subject_type = subject_type,
    box_subject_id = subject_id
  )

  # create auth function
  req_auth <- function(req) {
    httr2::req_oauth_client_credentials(
      req,
      client,
      token_params = token_params
    )
  }

  if (.test) {
    bx_auth_test(req_auth)
  }

  class(req_auth) <- c("cardbord_auth_credentials", "cardbord_auth")
  req_auth
}

make_client <- function(id, secret) {

  id <- id %||% askpass::askpass("Please enter Box app client_id:")
  secret <- secret %||% askpass::askpass("Please enter Box app client_secret:")

  httr2::oauth_client(
    id = id,
    secret = secret,
    token_url = url_box("oauth2/token")
  )
}

