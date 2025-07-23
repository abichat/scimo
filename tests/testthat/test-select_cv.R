library(dplyr)

data(pedcan_expression)

df_expr <- select(pedcan_expression, starts_with("A"))

test_that("step_select_cv works", {
  means <- apply(df_expr, 2, mean)
  sds <- apply(df_expr, 2, sd)
  cvs <- sds / means

  rec <-
    recipe(~., data = df_expr) %>%
    step_select_cv(all_numeric_predictors(), cutoff = 1)

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  cv_tidy <- tidy(prepped, 1)

  expect_equal(nrow(cv_tidy), ncol(df_expr))
  expect_equal(pull(cv_tidy, cv, terms), cvs)
  expect_true(all(cv_tidy[cv_tidy$kept, ]$cv >= 1))
  expect_true(all(cv_tidy[!cv_tidy$kept, ]$cv < 1))

  baked <- bake(prepped, new_data = NULL)

  expect_setequal(colnames(baked), names(cvs[cvs > 1]))

  expect_invisible(recipes_pkg_check(required_pkgs.step_select_cv()))
})
