has_sodium <- requireNamespace("sodium", quietly = TRUE)
key <- Sys.getenv("CARDBOARD_AUTH_IJL")

if (has_sodium && nchar(key) > 0) {
  file <- fs::path_package("cardboard", "secret", "auth")
  auth <- bx_auth_decrypt_file(file, key = key)
  bx_auth_use(auth)
}
