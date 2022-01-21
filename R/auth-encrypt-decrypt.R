# ref: https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html

#' Auth file encryprition
#'
#' @inheritParams bx_auth
#' @inheritParams bx_auth_create_interactive
#' @param file `character`
#' @param key `character`
#' @param nonce `character`
#'
#' @return Invisible `auth`, called for side effects.
#' @export
#'
bx_auth_encrypt_file <- function(auth, file, key = NULL, nonce = NULL) {

  key <- key %||% key_gen()
  key_raw <- as_key_raw(key)
  nonce_raw <- as_nonce_raw(nonce)

  file_abs <- fs::path_abs(file)
  if (fs::file_exists(file_abs)) {
    if (!interactive()) {
      stop(glue("Not interactive. File: {file_abs} exists. Aborting."))
    }

    proceed <- utils::askYesNo(glue("File: {file_abs} exists. Overwrite?"))
    if (!proceed) {
      stop("File exists. Aborting.")
    }
  }

  list_auth <- bx_auth_to_list(auth)
  json_auth <- to_json(list_auth)
  raw_auth <- charToRaw(json_auth)

  encrypt_auth <-
    sodium::data_encrypt(raw_auth, key = key_raw, nonce = nonce_raw)
  attr(encrypt_auth, "nonce") <- NULL

  writeBin(encrypt_auth, file)

  cli::cat_bullet(
    glue("auth written to encrypted file: {file_abs}"),
    bullet = "tick",
    bullet_col = "green"
  )

  invisible(auth)
}

#' @rdname bx_auth_encrypt_file
#' @export
#'
bx_auth_decrypt_file <- function(file, key, nonce = NULL, .test = TRUE) {

  key_raw <- as_key_raw(key)
  nonce_raw <- as_nonce_raw(nonce)

  encrypt_auth <- readBin(file, "raw", file.size(file))
  raw_auth <-
    sodium::data_decrypt(encrypt_auth, key = key_raw, nonce = nonce_raw)
  json_auth <- rawToChar(raw_auth)
  list_auth <- from_json(json_auth)

  auth <- bx_auth_from_list(list_auth)

  if (.test) {
    bx_auth_test(auth)
  }

  invisible(auth)
}


as_key_raw <- function(key) {
  sodium::sha256(charToRaw(key))
}

# TODO: credit gargle
key_gen <- function() {

  # uncouple from seed
  withr::local_preserve_seed()
  set.seed(NULL)

  key <- sample(c(letters, LETTERS, 0:9), 50, replace = TRUE)
  key <- paste0(key, collapse = "")

  cli::cat_bullet(
    glue("generated key: {key}"),
    bullet = "tick",
    bullet_col = "green"
  )

  key
}

as_nonce_raw <- function(nonce) {
  nonce <- nonce %||% "4XQ5PHbHeoFZrqiPR1Lp4m3b"
  charToRaw(nonce)
}

