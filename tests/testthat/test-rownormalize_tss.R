data("cheese_abundance")

test_that("step_rownormalize_tss() works", {
  rec <-
    recipe(~ ., data = cheese_abundance) %>%
    step_rownormalize_tss(all_numeric_predictors())

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  tss_tidy <- tidy(prepped, 1)

  expect_equal(nrow(tss_tidy), ncol(cheese_abundance) - 3)

  baked <- bake(prepped, new_data = NULL)

  expect_equal(colnames(baked), colnames(cheese_abundance))
  expect_equal(rowSums(baked[, -c(1:3)]), rep(1, nrow(cheese_abundance)))


  expect_invisible(recipes_pkg_check(required_pkgs.step_rownormalize_tss()))

})
