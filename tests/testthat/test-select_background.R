library(dplyr)

data(pedcan_expression)

df_expr <-
  pedcan_expression %>%
  select(event, disease, starts_with("A"))


test_that("step_select_background works", {
  level <- runif(1, min = 1, max = 6)
  prop <- runif(1)
  n <- sample(1:20, size = 1)

  quants <-
    df_expr %>%
    summarize(across(
      where(is.numeric),
      ~ unname(quantile(., probs = 1 - prop, type = 1))
    )) %>%
    unlist()

  rec <-
    recipe(disease ~ ., data = df_expr) %>%
    step_select_background(
      all_numeric_predictors(),
      background_level = level,
      prop_samples = prop
    )

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  bkg_tidy <- tidy(prepped, 1)

  expect_equal(nrow(bkg_tidy), ncol(df_expr) - 2)
  expect_equal(
    bkg_tidy[bkg_tidy$kept, "terms", drop = TRUE],
    names(quants[quants >= level])
  )

  baked <- bake(prepped, new_data = NULL)

  expect_setequal(
    colnames(baked),
    c(bkg_tidy[bkg_tidy$kept, ]$terms, "event", "disease")
  )

  kepts <-
    df_expr %>%
    summarize(across(where(is.numeric), ~ sum(. >= level) >= n)) %>%
    unlist()

  bkg_tidy2 <-
    recipe(disease ~ ., data = df_expr) %>%
    step_select_background(
      all_numeric_predictors(),
      background_level = level,
      n_samples = n
    ) %>%
    prep() %>%
    tidy(1)

  expect_equal(
    bkg_tidy2[bkg_tidy2$kept, "terms", drop = TRUE],
    names(kepts[kepts])
  )

  expect_invisible(recipes_pkg_check(required_pkgs.step_select_background()))
})


test_that("step_select_background throw errors", {
  rec <-
    recipe(disease ~ ., data = df_expr)

  expect_error(
    rec %>%
      step_select_background(all_numeric_predictors(), n_samples = 10) %>%
      prep(),
    "background_level"
  )

  expect_error(
    rec %>%
      step_select_background(all_numeric_predictors(), background_level = 2) %>%
      prep(),
    "n_samples.*prop_samples"
  )

  expect_error(
    rec %>%
      step_select_background(
        all_numeric_predictors(),
        background_level = 2,
        n_samples = 10,
        prop_samples = 0.10
      ) %>%
      prep(),
    "are mutually exclusive"
  )

  expect_error(
    rec %>%
      step_select_background(
        all_predictors(),
        background_level = 2,
        n_samples = 10,
        prop_samples = 0.10
      ) %>%
      prep(),
    "numeric"
  )
})
