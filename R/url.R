url_box <- function(x) {

  x <- remove_leading_slash(x)

  glue("https://api.box.com/{x}")
}

url_box_api <- function(x) {

  x <- remove_leading_slash(x)

  url_box(glue("2.0/{x}"))
}

remove_leading_slash <- function(x) {
  sub("^/(.*)", "\\1", x)
}
