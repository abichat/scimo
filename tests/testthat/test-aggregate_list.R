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
    bake(new_data = NULL)

  expect_equal(colnames(baked2),
               c(colnames(cheese_abundance), names(rk_list)))


  expect_invisible(recipes_pkg_check(required_pkgs.step_aggregate_list()))

})


test_that("`others` argument works", {

  ### All variables are present in list_agg
  l1 <- list(petal.size = c("Petal.Width", "Petal.Length"),
             sepal.size = c("Sepal.Length", "Sepal.Width"))


  # Discard original columns
  rec1_discori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l1, fun_agg = prod,
                        others = "discard") %>%
    prep()

  expect_equal(colnames(bake(rec1_discori, new_data = NULL)),
               c("Species", names(l1)))

  expect_equal(tidy(rec1_discori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec1_discori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", "petal.size"))


  # Keep original columns
  rec1_keepori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l1, fun_agg = prod,
                        others = "discard",
                        keep_original_cols = TRUE) %>%
    prep()

  expect_equal(colnames(bake(rec1_keepori, new_data = NULL)),
               c(colnames(iris), names(l1)))

  expect_equal(tidy(rec1_keepori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec1_keepori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", "petal.size"))

  ### One variable is missing in list_agg
  l2 <- list(petal.size = c("Petal.Length"),
             sepal.size = c("Sepal.Width", "Sepal.Length"))


  # Discard other columns and discard original columns
  rec2_discoth_disori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        others = "discard",
                        list_agg = l2, fun_agg = prod) %>%
    prep()

  expect_equal(colnames(bake(rec2_discoth_disori, new_data = NULL)),
               c("Species", names(l2)))

  expect_equal(tidy(rec2_discoth_disori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec2_discoth_disori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", NA))


  # Discard other columns and keep original columns
  rec2_discoth_keepori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l2, fun_agg = prod,
                        others = "discard",
                        keep_original_cols = TRUE) %>%
    prep()

  expect_equal(colnames(bake(rec2_discoth_keepori, new_data = NULL)),
               c(colnames(iris), names(l2)))

  expect_equal(tidy(rec2_discoth_keepori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec2_discoth_keepori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", NA))


  # Keep other columns and discard original columns
  rec2_keepoth_disori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l2, fun_agg = prod,
                        others = "asis",
                        keep_original_cols = FALSE) %>%
    prep()

  expect_equal(colnames(bake(rec2_keepoth_disori, new_data = NULL)),
               c("Petal.Width", "Species", names(l2)))

  expect_equal(tidy(rec2_keepoth_disori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec2_keepoth_disori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", "Petal.Width"))


  # Keep other columns and keep original columns
  rec2_keepoth_keepori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l2, fun_agg = prod,
                        others = "asis",
                        keep_original_cols = TRUE) %>%
    prep()

  expect_equal(colnames(bake(rec2_keepoth_keepori, new_data = NULL)),
               c(names(iris), names(l2)))

  expect_equal(tidy(rec2_keepoth_keepori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec2_keepoth_keepori, 1)$aggregate,
               c("sepal.size", "sepal.size", "petal.size", "Petal.Width"))

  ### Some variables are missing in list_agg
  l3 <- list(sepal.size = c("Sepal.Width", "Sepal.Length"))


  # Aggregate other columns and discard original columns
  rec3_aggoth_disori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        others = "aggregate", name_others = "Oth",
                        list_agg = l3, fun_agg = prod) %>%
    prep()

  expect_equal(colnames(bake(rec3_aggoth_disori, new_data = NULL)),
               c("Species", names(l3), "Oth"))

  expect_equal(tidy(rec3_aggoth_disori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec3_aggoth_disori, 1)$aggregate,
               c("sepal.size", "sepal.size", "Oth", "Oth"))

  expect_equal(bake(rec3_aggoth_disori, new_data = NULL)$Oth,
               iris$Petal.Length * iris$Petal.Width)


  # Aggregate other columns and keep original columns
  rec3_aggcoth_keepori <-
    iris %>%
    recipe(formula = Species ~ .) %>%
    step_aggregate_list(all_numeric_predictors(),
                        list_agg = l3, fun_agg = prod,
                        others = "aggregate", name_others = "Oth",
                        keep_original_cols = TRUE) %>%
    prep()

  expect_equal(colnames(bake(rec3_aggcoth_keepori, new_data = NULL)),
               c(colnames(iris), names(l3), "Oth"))

  expect_equal(tidy(rec3_aggcoth_keepori, 1)$terms,
               colnames(iris)[-5])
  expect_equal(tidy(rec3_aggcoth_keepori, 1)$aggregate,
               c("sepal.size", "sepal.size", "Oth", "Oth"))

  expect_equal(bake(rec3_aggcoth_keepori, new_data = NULL)$Oth,
               iris$Petal.Length * iris$Petal.Width)

})
