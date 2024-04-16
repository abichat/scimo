data("cheese_taxonomy")


test_that("step_taxonomy works", {
  skip_if_not_installed("yatah", minimum_version = "1.0.0")

  rks <- c("kingdom", "phylum", "class", "order",
           "family", "genus", "species")

  rec <-
    cheese_taxonomy %>%
    recipe(~ .) %>%
    step_taxonomy(lineage, rank = rks)


  expect_equal(nrow(tidy(rec, 1)), 1)

  prepped <- prep(rec)
  txn_tidy <- tidy(prepped, 1)

  expect_equal(pull(txn_tidy, terms), rep("lineage", length(rks)))
  expect_equal(pull(txn_tidy, rank), rks)

  baked <- bake(prepped, new_data = NULL)

  expect_equal(ncol(baked), ncol(cheese_taxonomy) - 1 + length(rks))
  expect_equal(baked$lineage_kingdom, cheese_taxonomy$kingdom)
  expect_equal(baked$lineage_phylum, cheese_taxonomy$phylum)
  expect_equal(baked$lineage_class, cheese_taxonomy$class)
  expect_equal(baked$lineage_order, cheese_taxonomy$order)
  expect_equal(gsub("_", " ", baked$lineage_family), cheese_taxonomy$family)
  expect_equal(baked$lineage_genus, cheese_taxonomy$genus)
  expect_equal(gsub("_", " ", baked$lineage_species), cheese_taxonomy$species)


  rks2 <- c("genus", "order", "phylum")

  prepped2 <-
    cheese_taxonomy %>%
    select(asv, lineage) %>%
    mutate(lin2 = sample(lineage)) %>%
    recipe(~ .) %>%
    step_taxonomy(starts_with("lin"), rank = rks2,
                  keep_original_cols = TRUE) %>%
    prep()

  txn_tidy2 <- tidy(prepped2, 1)

  expect_equal(pull(txn_tidy2, terms),
               rep(c("lineage", "lin2"), each = length(rks2)))
  expect_equal(pull(txn_tidy2, rank), rep(rks2, times = 2))

  baked2 <- bake(prepped2, new_data = NULL)

  expect_equal(colnames(baked2), c("asv", "lineage", "lin2",
                                   paste0("lineage_", rks2),
                                   paste0("lin2_", rks2)))


  rec_error <-
    cheese_taxonomy %>%
    recipe(~ .) %>%
    step_taxonomy(lineage)

  expect_error(prep(rec_error), "`rank` must")

  expect_invisible(recipes_pkg_check(required_pkgs.step_taxonomy()))

})


test_that("step_taxonomy fails witout yatah", {
  skip_if(rlang::is_installed("yatah"))

  suppressMessages(
    expect_message(recipes::recipes_pkg_check(required_pkgs.step_taxonomy()),
                   ".*yatah.*"))
})
