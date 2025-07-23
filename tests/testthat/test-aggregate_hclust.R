library(dplyr)

data("cheese_abundance")

nc <- sample(2:9, size = 1)
new_names <- paste0("hc_", 1:nc)


clustering <-
  cheese_abundance %>%
  select(where(is.numeric)) %>%
  as.matrix() %>%
  t() %>%
  dist(method = "manhattan") %>%
  hclust(method = "ward.D2") %>%
  cutree(k = nc)

rnd_asv <- sample(names(clustering), size = 1)
rnd_asv_group <- names(clustering[clustering == clustering[rnd_asv]])


test_that("step_aggregate_hclust works", {
  rec <-
    recipe(~., data = cheese_abundance) %>%
    step_aggregate_hclust(
      all_numeric_predictors(),
      n_clusters = nc,
      fun_agg = sum,
      prefix = "hc_",
      dist_metric = "manhattan",
      linkage_method = "ward.D2"
    )

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  hclust_tidy <- tidy(prepped, 1)

  expect_equal(nrow(hclust_tidy), ncol(cheese_abundance) - 3)

  # check set equality on random cluster
  expect_setequal(
    hclust_tidy %>%
      filter(
        aggregate %in%
          filter(hclust_tidy, terms == rnd_asv)$aggregate
      ) %>%
      pull(terms),
    rnd_asv_group
  )

  # check equality on groups cardinal
  expect_setequal(
    hclust_tidy %>%
      count(aggregate) %>%
      pull(n) %>%
      sort(),
    clustering %>%
      table() %>%
      sort() %>%
      unname()
  )

  expect_setequal(summary(prepped)$role, "predictor")

  baked <- bake(prepped, new_data = NULL)

  expect_equal(colnames(baked), c("sample", "cheese", "rind_type", new_names))
  expect_equal(rowSums(baked[, -c(1:3)]), rowSums(cheese_abundance[, -c(1:3)]))

  cl <- sample(new_names, size = 1)
  asv_in_cl <-
    hclust_tidy %>%
    filter(aggregate == cl) %>%
    pull(terms)

  expect_equal(
    baked[[cl]],
    cheese_abundance %>%
      select(all_of(asv_in_cl)) %>%
      rowSums()
  )

  ## keep_original_cols

  baked2 <-
    recipe(~., data = cheese_abundance) %>%
    step_aggregate_hclust(
      all_numeric_predictors(),
      n_clusters = nc,
      fun_agg = sum,
      prefix = "hc_",
      dist_metric = "manhattan",
      linkage_method = "ward.D2",
      keep_original_cols = TRUE
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(colnames(baked2), c(colnames(cheese_abundance), new_names))

  expect_invisible(recipes_pkg_check(required_pkgs.step_aggregate_hclust()))
})


test_that("step_aggregate_hclust throws errors", {
  expect_error(
    recipe(~., data = cheese_abundance) %>%
      step_aggregate_hclust(
        all_numeric_predictors(),
        n_clusters = nc,
        fun_agg = sum,
        dist_metric = "hello"
      ) %>%
      prep(),
    '`dist_metric` must be one of "euclidean",'
  )

  expect_error(
    recipe(~., data = cheese_abundance) %>%
      step_aggregate_hclust(
        all_numeric_predictors(),
        n_clusters = nc,
        fun_agg = sum,
        linkage_method = "hello"
      ) %>%
      prep(),
    '`linkage_method` must be one of "ward.D",'
  )
})
