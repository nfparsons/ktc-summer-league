# tests/testthat/test-db-connect.R
# Unit tests for db_connect()'s retry + status-return logic (persist.R).
# Uses injected fake connectors via `connect_fn`, so no live database is
# required. Assumes persist.R is loaded (via the package, or source()'d in
# tests/testthat/setup.R alongside dice.R, explore.R, etc.).
#
# The only behaviour these tests do NOT cover is the real wake latency
# against the live Neon instance, which is verified separately.

test_that("a connector that succeeds first try reports ok with one attempt", {
  res <- db_connect(connect_fn = function() "FAKE_CON", retries = 3L, wait = 0)
  expect_true(res$ok)
  expect_identical(res$con, "FAKE_CON")
  expect_equal(res$attempts, 1L)
  expect_equal(res$db_response, "")
})

test_that("a connector that wakes on the 3rd try succeeds and counts attempts", {
  attempt <- 0L
  waker <- function() {
    attempt <<- attempt + 1L
    if (attempt < 3L) stop("the database system is starting up")
    "FAKE_CON"
  }
  res <- db_connect(connect_fn = waker, retries = 5L, wait = 0)
  expect_true(res$ok)
  expect_equal(res$attempts, 3L)
  expect_equal(res$db_response, "")
})

test_that("a connector that always fails returns the DB's own message", {
  res <- db_connect(
    connect_fn = function() stop("could not connect to server: Connection refused"),
    retries = 2L, wait = 0
  )
  expect_false(res$ok)
  expect_null(res$con)
  expect_equal(res$attempts, 2L)
  expect_match(res$db_response, "Connection refused")
  expect_match(res$log, "after 2 attempts")
})
