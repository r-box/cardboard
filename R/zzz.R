# environment, private to the package, where we can store
# stuff, like the default auth-function
bx_env <- NULL

.onLoad <- function(...) {
  bx_env <<- new.env()
  bx_env[["auth"]] <- NULL
}
