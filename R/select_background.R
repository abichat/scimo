#' Feature selection step using background level
#'
#' Select features that exceed a background level in at least a defined
#' number of samples.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
#' @param background_level Background level to exceed.
#' @param n_samples,prop_samples Count or proportion of samples in which a
#' feature exceeds `background_level` to be retained.
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
#'   recipe(formula = Species ~ .) %>%
#'   step_select_background(all_numeric_predictors(),
#'                          background_level = 4, prop_samples = 0.5) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' bake(rec, new_data = NULL)
step_select_background <- function(recipe, ..., role = NA, trained = FALSE,
                                   background_level = NULL,
                                   n_samples = NULL,
                                   prop_samples = NULL,
                                   res = NULL,
                                   skip = FALSE,
                                   id = rand_id("select_background")) {

  add_step(
    recipe,
    step_select_background_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      background_level = background_level,
      n_samples = n_samples,
      prop_samples = prop_samples,
      res = res,
      skip = skip,
      id = id
    )
  )
}


#' @importFrom recipes step
step_select_background_new <- function(terms, role, trained,
                                       background_level,
                                       n_samples, prop_samples,
                                       res, skip, id) {

  step(subclass = "select_background",
       terms = terms,
       role = role,
       trained = trained,
       background_level = background_level,
       n_samples = n_samples,
       prop_samples = prop_samples,
       res = res,
       skip = skip,
       id = id)
}

#' @export
#' @importFrom recipes check_type recipes_eval_select
#' @importFrom rlang  abort
#' @importFrom tibble enframe
prep.step_select_background <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)

  if (is.null(x$background_level)) {
    abort("You must specify `background_level`.")
  }

  if (!is.null(x$n_samples) & !is.null(x$prop_samples)) {
    abort("`n_samples` and `prop_samples` are mutually exclusive.")
  } else if (!is.null(x$n_samples)) {
    n_row <- x$n_samples
  } else if (!is.null(x$prop_samples)) {
    n_row <- x$prop_samples * nrow(training)
  } else {
    abort("You must specify `n_samples` or `prop_samples`.")
  }

  is_above_bkg <- function(vec) {
    sum(vec >= x$background_level) >= n_row
  }

  res_bkg <-
    training[, col_names] %>%
    apply(2, is_above_bkg) %>%
    enframe(name = "terms", value = "kept")

  step_select_background_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    background_level = x$background_level,
    n_samples = x$n_samples,
    prop_samples = x$prop_samples,
    res = res_bkg,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
bake.step_select_background <- function(object, new_data, ...) {
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
print.step_select_background <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Background filtering on "

    print_step(
      tr_obj = x$res$terms,
      untr_obj = x$terms,
      trained = x$trained,
      title = title,
      width = width
    )
    invisible(x)
  }

#' @rdname step_select_background
#' @param x A `step_select_background` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_select_background <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        kept = rlang::na_lgl
      )
  }

  res$id <- x$id
  res
}


#' @rdname required_pkgs.scimo
#' @export
required_pkgs.step_select_background <- function(x, ...) {
  c("scimo")
}
