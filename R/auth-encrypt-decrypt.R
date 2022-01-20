# ref: https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html

bx_auth_encrypt_file <- function(auth, file, key = NULL, nonce = NULL) {

  key <- as_key(key)
  nonce <- as_nonce(nonce)

}

bx_auth_decrypt_file <- function(file, key, nonce = NULL, .test = TRUE) {

  nonce <- as_nonce(nonce)

}


as_key <- function(key) {
  key <- key %||% key_gen()
  invisible(key)
}

# TODO: credit gargle
key_gen <- function() {

  # uncouple from seed
  withr::local_preserve_seed()
  set.seed(NULL)

  x <- sample(c(letters, LETTERS, 0:9), 50, replace = TRUE)
  paste0(x, collapse = "")
}

as_nonce <- function(nonce) {
  nonce <- nonce %||% "d642b7509df52965ccfe8dcf99f94b91301b78579d9ceab2"
  sodium::hex2bin(nonce)
}
