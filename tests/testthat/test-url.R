test_that("url_box() works", {
  expect_identical(url_box("foo"), "https://api.box.com/foo")
  expect_identical(url_box("/foo"), "https://api.box.com/foo")
  expect_identical(url_box("/foo/bar"), "https://api.box.com/foo/bar")
})

test_that("url_box_api() works", {
  expect_identical(url_box_api("foo"), "https://api.box.com/2.0/foo")
  expect_identical(url_box_api("/foo"), "https://api.box.com/2.0/foo")
  expect_identical(url_box_api("/foo/bar"), "https://api.box.com/2.0/foo/bar")
})

test_that("auth works", {
  expect_true(bx_auth_test(bx_auth()))
})
