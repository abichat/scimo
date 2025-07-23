library(dplyr)

data(pedcan_expression)

df_expr <-
  pedcan_expression %>%
  select(event, disease, starts_with("A")) %>%
  filter(event != "Unknown")

gene <- sample(colnames(df_expr)[-c(1:2)], size = 1)
gx <- df_expr[df_expr$event == "Primary", gene, drop = TRUE]
gy <- df_expr[df_expr$event == "Metastatic", gene, drop = TRUE]
gwt <- wilcox.test(x = gx, y = gy)

test_that("step_select_wilcoxon works", {
  my_cutoff <- 0.05
  my_correction <- "BH"

  rec <-
    recipe(event ~ ., data = df_expr) %>%
    step_select_wilcoxon(
      all_numeric_predictors(),
      outcome = "event",
      correction = my_correction,
      cutoff = my_cutoff
    )

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  wt_tidy <- tidy(prepped, 1)

  expect_equal(nrow(wt_tidy), ncol(df_expr) - 2)
  expect_equal(wt_tidy[wt_tidy$terms == gene, ]$pv, gwt$p.value)
  expect_true(all(wt_tidy[wt_tidy$kept, ]$qv <= my_cutoff))
  expect_true(all(wt_tidy[!wt_tidy$kept, ]$qv > my_cutoff))
  expect_equal(p.adjust(wt_tidy$pv, method = my_correction), wt_tidy$qv)

  baked <- bake(prepped, new_data = NULL)

  expect_setequal(
    colnames(baked),
    c(wt_tidy[wt_tidy$kept, ]$terms, "event", "disease")
  )

  expect_invisible(recipes_pkg_check(required_pkgs.step_select_wilcoxon()))
})

test_that("step_select_wilcoxon throw errors", {
  rec <-
    recipe(disease ~ ., data = df_expr)

  expect_error(
    rec %>%
      step_select_wilcoxon(
        all_numeric_predictors(),
        outcome = "disease",
        cutoff = 0.05
      ) %>%
      prep(),
    "`outcome` must be a binary vector."
  )

  expect_error(
    rec %>%
      step_select_wilcoxon(
        all_numeric_predictors(),
        outcome = "event",
        correction = c("BH", "fdr"),
        cutoff = 0.05
      ) %>%
      prep(),
    "`correction` must be a length-one vector."
  )

  expect_error(
    rec %>%
      step_select_wilcoxon(
        all_numeric_predictors(),
        outcome = "event",
        correction = "FDR",
        cutoff = 0.05
      ) %>%
      prep(),
    '`correction` must be one of "holm", "hochberg", '
  )
})
