test_that("aggregate_var works", {
  list_iris <-
    list(
      sepal = c("Sepal.Length", "Sepal.Width"),
      petal = c("Petal.Length", "Petal.Width"),
      c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
    )

  agg1 <- aggregate_var(
    iris,
    list_agg = list_iris,
    fun_agg = prod,
    prefix = "group",
    keep_original_cols = TRUE
  )

  expect_equal(colnames(agg1), c(names(iris), names(list_iris[1:2]), "group3"))
  expect_equal(agg1[, 1:5], iris)
  expect_equal(agg1$sepal, iris$Sepal.Length * iris$Sepal.Width)
  expect_equal(
    agg1$group3,
    iris$Sepal.Length * iris$Sepal.Width * iris$Petal.Length * iris$Petal.Width
  )

  agg2 <- aggregate_var(iris, list_agg = list_iris, fun_agg = prod)

  expect_equal(colnames(agg2), c("Species", names(list_iris[1:2]), "agg_3"))
  expect_equal(agg2$sepal, agg1$sepal)
})
