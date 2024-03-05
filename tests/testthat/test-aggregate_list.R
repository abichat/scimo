library(dplyr)

data("cheese_abundance")
data("cheese_taxonomy")

rk <- sample(colnames(cheese_taxonomy)[-c(1:2)], size = 1)

rk_list <- split(cheese_taxonomy$asv, cheese_taxonomy[[rk]])

test_that("step_aggregate_list() works", {
  rec <-
    recipe(~ ., data = cheese_abundance) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = rk_list, fun_agg = sum)

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  agglist_tidy <- tidy(prepped, 1)

  expect_equal(nrow(agglist_tidy), ncol(cheese_abundance) - 3)
  expect_equal(agglist_tidy[-3],
               select(cheese_taxonomy, terms = asv, aggregate = all_of(rk)))

  expect_setequal(summary(prepped)$role, "predictor")

  baked <- bake(prepped, new_data = NULL)

  expect_equal(colnames(baked),
               c("sample", "cheese", "rind_type", names(rk_list)))
  expect_equal(rowSums(baked[, -c(1:3)]), rowSums(cheese_abundance[, -c(1:3)]))

  cl <- sample(names(rk_list), size = 1)


  expect_equal(baked[[cl]],
               cheese_abundance %>%
                 select(all_of(rk_list[[cl]])) %>%
                 rowSums())

  ## keep_original_cols

  baked2 <-
    recipe(~ ., data = cheese_abundance) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = rk_list, fun_agg = sum,
                        keep_original_cols = TRUE) %>%
    prep() %>%
    juice()

  expect_equal(colnames(baked2),
               c(colnames(cheese_abundance), names(rk_list)))

})
