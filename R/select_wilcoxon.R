#' Feature selection step using Wilcoxon test
#'
#' Select variables with the lowest (adjusted) p-value of a
#' Wilcoxon-Mann-Whitney test against an outcome.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
#' @param outcome Name of the variable to perform the test against.
#' @inheritParams var_to_keep
#' @param correction Multiple testing correction method. One of
#' `p.adjust.methods`. Default to `"none"`.
#' @param res This parameter is only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
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
#' @examples
#' rec <-
#'   iris %>%
#'   dplyr::filter(Species != "virginica") %>%
#'   recipe(formula = Species ~ .) %>%
#'   step_select_wilcoxon(all_numeric_predictors(), outcome = "Species",
#'                        correction = "fdr", prop_kept = 0.5) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' juice(rec)
step_select_wilcoxon <- function(recipe, ..., role = NA, trained = FALSE,
                                 outcome = NULL,
                                 n_kept = NULL,
                                 prop_kept = NULL,
                                 cutoff = NULL,
                                 correction = "none",
                                 res = NULL,
                                 skip = FALSE,
                                 id = rand_id("select_wilcoxon")) {

  add_step(
    recipe,
    step_select_wilcoxon_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      outcome = outcome,
      n_kept = n_kept,
      prop_kept = prop_kept,
      cutoff = cutoff,
      correction = correction,
      res = res,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_select_wilcoxon_new <- function(terms, role, trained, outcome,
                                     n_kept, prop_kept, cutoff, correction,
                                     res, skip, id) {

  step(subclass = "select_wilcoxon",
       terms = terms,
       role = role,
       trained = trained,
       outcome = outcome,
       n_kept = n_kept,
       prop_kept = prop_kept,
       cutoff = cutoff,
       correction = correction,
       res = res,
       skip = skip,
       id = id)
}

#' @export
#' @importFrom dplyr mutate
#' @importFrom recipes check_type recipes_eval_select
#' @importFrom rlang .data
#' @importFrom stats as.formula p.adjust wilcox.test
#' @importFrom tibble tibble
prep.step_select_wilcoxon <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)
  check_type(training[, x$outcome], quant = FALSE)
  check_binary(training[[x$outcome]], name_x = "outcome")
  check_in(x$correction, stats::p.adjust.methods, name_x = "correction")

  pvs <- rep(NA, length(col_names))

  for (i in seq_along(col_names)) {
    frml <- as.formula(paste0("`", col_names[i], "`", " ~ ", x$outcome))
    pvs[i] <- wilcox.test(formula = frml, data = training)$p.value
  }

  res_wlcx <- tibble(terms = unname(col_names),
                     pv = pvs)

  if (x$correction == "none") {
    res_wlcx <-
      res_wlcx %>%
      mutate(kept = var_to_keep(.data$pv, x$n_kept, x$prop_kept, x$cutoff,
                                maximize = FALSE))
  } else {
    res_wlcx <-
      res_wlcx %>%
      mutate(qv = p.adjust(.data$pv, method = x$correction),
             kept = var_to_keep(.data$qv, x$n_kept, x$prop_kept, x$cutoff,
                                maximize = FALSE))
  }

  step_select_wilcoxon_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    n_kept = x$n_kept,
    prop_kept = x$prop_kept,
    cutoff = x$cutoff,
    correction = x$correction,
    res = res_wlcx,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
bake.step_select_wilcoxon <- function(object, new_data, ...) {
  col_names <- object$res$terms
  check_new_data(col_names, object, new_data)

  col_to_remove <-
    object$res %>%
    filter(!.data$kept) %>%
    pull(.data$terms)

  new_data[col_to_remove] <- NULL

  new_data
}

#' @export
#' @importFrom recipes print_step
print.step_select_wilcoxon <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- paste("Wilcoxon filtering against", x$outcome, "on ")

    print_step(
      tr_obj = x$res$terms,
      untr_obj = x$terms,
      trained = x$trained,
      title = title,
      width = width
    )
    invisible(x)
  }


#' @rdname step_select_wilcoxon
#' @param x A `step_select_wilcoxon` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_select_wilcoxon <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        pv = rlang::na_dbl,
        kept = rlang::na_lgl
      )
  }
  # Always return the step id:
  res$id <- x$id
  res
}
