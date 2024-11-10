
test_that("step_mfa works", {
  skip_if_not_installed("FactoMineR")

  mtcars2 <-
    mtcars %>%
    transform(cyl = as.factor(cyl), vs = as.factor(vs),
              am = as.factor(am), gear = as.factor(gear),
              carb = as.factor(carb)) %>%
    as_tibble(rownames = "car")

  g_1 <- colnames(mtcars2)[c(2, 4:8)]
  g_2 <- colnames(mtcars2)[c(3, 9:12)]
  groups <- list(g1 = g_1, g2 = g_2)
  nc <- 4

  rec <-
    recipe(car ~ ., data = mtcars2) %>%
    step_mfa(all_predictors(),
             list_groups = groups,
             type_var = c("c", "n"),
             num_comp = nc, prefix = "MFA")

  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  mfa_tidy <- tidy(prepped, 1)

  expect_equal(pull(mfa_tidy, terms), c(g_1, g_2))
  expect_equal(pull(mfa_tidy, group), c(rep("g1", length(groups[[1]])),
                                        rep("g2", length(groups[[2]]))))

  baked <- bake(prepped, new_data = NULL)

  expect_equal(ncol(baked), nc + 1)
  expect_equal(colnames(baked), c("car", paste0("MFA", seq_len(nc))))

  fmr_call <- call2("MFA", .ns = "FactoMineR",
                    base = mtcars2[, c(g_1, g_2)],
                    group = c(length(g_1), length(g_2)),
                    type = c("c", "n"), ncp = nc, graph = FALSE)

  res_mfa <- eval_tidy(fmr_call)

  expect_equal(baked$MFA1, unname(res_mfa$ind$coord[, 1]))
  expect_equal(baked$MFA2, unname(res_mfa$ind$coord[, 2]))
  expect_equal(baked$MFA3, unname(res_mfa$ind$coord[, 3]))
  expect_equal(baked$MFA4, unname(res_mfa$ind$coord[, 4]))

  baked2 <- bake(prepped, new_data = head(mtcars2))
  expect_equal(baked2$MFA1, head(baked$MFA1))


})
