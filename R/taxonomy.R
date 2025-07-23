#' Taxonomic clades feature generator
#'
#' Extract clades from a lineage, as defined in the `{yatah}` package.
#'
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [recipes::selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the new columns created by this step from
#'  the original variables will be used as `predictors` in a model.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
#' @param rank The desired ranks, a combinaison of `"kingdom"`, `"phylum"`,
#' `"class"`, `"order"`, `"family"`, `"genus"`, `"species"`, or `"strain"`. See
#' [yatah::get_clade()] for more details.
#' @param res This parameter is only produced after the recipe has been trained.
#' @param keep_original_cols A logical to keep the original variables in
#' the output. Defaults to `FALSE`.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake()]? While all operations are baked
#'  when [recipes::prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of recipe with the new step added to the
#' sequence of any existing operations.
#'
#' @export
#'
#' @importFrom recipes add_step rand_id
#' @importFrom rlang enquos
#'
#' @author Antoine Bichat
#'
#' @examplesIf rlang::is_installed("yatah")
#' data("cheese_taxonomy")
#' rec <-
#'   cheese_taxonomy %>%
#'   select(asv, lineage) %>%
#'   recipe(~ .) %>%
#'   step_taxonomy(lineage, rank = c("order", "genus")) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' bake(rec, new_data = NULL)
step_taxonomy <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  rank = NULL,
  res = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("taxonomy")
) {
  add_step(
    recipe,
    step_taxonomy_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      rank = rank,
      res = res,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_taxonomy_new <- function(
  terms,
  role,
  trained,
  rank,
  res,
  keep_original_cols,
  skip,
  id
) {
  step(
    subclass = "taxonomy",
    terms = terms,
    role = role,
    trained = trained,
    rank = rank,
    res = res,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
#' @importFrom recipes check_type recipes_eval_select
#' @importFrom tidyr expand_grid
prep.step_taxonomy <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = FALSE)
  check_not_null(x$rank, "rank")

  res_txn <- expand_grid(terms = unname(col_names), rank = x$rank)

  step_taxonomy_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    rank = x$rank,
    res = res_txn,
    keep_original_cols = x$keep_original_cols,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
#' @importFrom rlang eval_tidy call2
bake.step_taxonomy <- function(object, new_data, ...) {
  col_names <- object$res$terms
  check_new_data(col_names, object, new_data)

  for (i in seq_len(nrow(object$res))) {
    new_col <- paste0(object$res$terms[i], "_", object$res$rank[i])
    yatah_call <- call2(
      "get_clade",
      .ns = "yatah",
      lineage = new_data[[object$res$terms[i]]],
      rank = object$res$rank[i],
      same = TRUE
    )
    new_data[[new_col]] <- eval_tidy(yatah_call)
  }

  if (!object$keep_original_cols) {
    new_data[, unique(col_names)] <- NULL
  }

  new_data
}

#' @export
#' @importFrom recipes print_step
print.step_taxonomy <- function(x, width = max(20, options()$width - 35), ...) {
  title <- "Taxonomy features from "

  print_step(
    tr_obj = unique(x$res$terms),
    untr_obj = x$terms,
    trained = x$trained,
    title = title,
    width = width
  )
  invisible(x)
}

#' @rdname step_taxonomy
#' @param x A `step_taxonomy` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_taxonomy <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        rank = rlang::na_chr
      )
  }

  res$id <- x$id
  res
}


#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.scimo
#' @keywords internal
#' @export
required_pkgs.step_taxonomy <- function(x, ...) {
  c("yatah", "scimo")
}
