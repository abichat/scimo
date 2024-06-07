test_that("check_in works", {
  expect_invisible(check_in("a", letters))
  expect_error(check_in("a", LETTERS), "must be one of")
  expect_error(check_in(c("a", "b"), letters), "must be a length-one vector")


  expect_error(check_in(c("FDR"), p.adjust.methods, name_x = "correction"),
               "correction")
  expect_error(check_in(c("FDR"), p.adjust.methods, name_x = "correction"),
               "fdr")
})

test_that("check_binary works", {
  expect_invisible(check_binary(letters[1:2]))

  expect_error(check_binary(letters), "must be a binary vector")
  expect_error(check_binary(iris$Species, name_x = "outcome"),
               "outcome")
})

test_that("check_not_null works", {
  expect_invisible(check_not_null(letters))

  expect_error(check_not_null(NULL), "must be specified and can't be `NULL`.")
  expect_error(check_binary(NULL, name_x = "rank"),
               "rank")
})
