
test_that("var_to_keep() keep the correct values", {

  expect_equal(var_to_keep(1:5, n_kept = 3, maximize = TRUE),
               c(rep(FALSE, 2), rep(TRUE, 3)))
  expect_equal(var_to_keep(1:10, prop_kept = 0.4, maximize = FALSE),
               c(rep(TRUE, 4), rep(FALSE, 6)))
  expect_equal(var_to_keep(1:10, cutoff = 8, maximize = FALSE),
               c(rep(TRUE, 8), rep(FALSE, 2)))
  expect_equal(var_to_keep(1:10, prop_kept = 0.8, cutoff = 7),
               c(rep(FALSE, 6), rep(TRUE, 4)))

  N <- 1234
  x1 <- rnorm(N, sd = 10)

  k11 <- var_to_keep(x1, n_kept = 40, maximize = FALSE)
  expect_equal(sum(k11), 40)
  expect_true(max(x1[k11]) < min(x1[!k11]))

  k12 <- var_to_keep(x1, n_kept = 40, maximize = TRUE)
  expect_equal(sum(k12), 40)
  expect_true(min(x1[k12]) > max(x1[!k12]))

  k13 <- var_to_keep(x1, prop_kept = 0.6, maximize = FALSE)
  expect_equal(mean(k13), 0.6, tolerance = 1 / N)
  expect_true(max(x1[k13]) < min(x1[!k13]))

  k14 <- var_to_keep(x1, prop_kept = 0.6, maximize = TRUE)
  expect_equal(mean(k14), 0.6, tolerance = 1 / N)
  expect_true(min(x1[k14]) > max(x1[!k14]))

  k15 <- var_to_keep(x1, cutoff = 0, maximize = FALSE)
  expect_true(max(x1[k15]) <= 0)
  expect_true(min(x1[!k15]) > 0)

  k16 <- var_to_keep(x1, cutoff = 0, maximize = TRUE)
  expect_true(min(x1[k16]) >= 0)
  expect_true(max(x1[!k16]) < 0)

  k17 <- var_to_keep(x1, prop_kept = 0.5, cutoff = 0, maximize = FALSE)
  expect_true(max(x1[k17]) <= 0)
  expect_true(mean(k17) <= N/2)

  k18 <- var_to_keep(x1, prop_kept = 0.5, cutoff = 0, maximize = TRUE)
  expect_true(max(x1[k18]) >= 0)
  expect_true(mean(k18) <= N/2)

  k19 <- var_to_keep(x1)
  expect_true(all(k19))


  # with ties
  x2 <- sample(1:100, N, replace = TRUE)

  k21 <- var_to_keep(x2, n_kept = 40, maximize = TRUE)
  expect_true(sum(k21) >= 40)
  expect_true(max(rank(-x2, ties.method = "min")[k21]) <= 40)
  expect_true(min(x2[k21]) > max(x2[!k21]))

  k22 <- var_to_keep(x2, prop = 0.3, maximize = FALSE)
  expect_true(mean(k22) >= 0.3 - 1/N)
  expect_true(max(rank(x2, ties.method = "min")[k22]) <= 0.30 * N)
  expect_true(max(x2[k22]) < min(x2[!k22]))

})



test_that("var_to_keep() throw error when needed", {

  expect_error(var_to_keep(1:5, n_kept = 2, prop_kept = 0.2),
               regexp = "mutually exclusive")

})
