#' Coefficient of variation
#'
#' @param x A numeric vector.
#' @param na.rm Logical indicating whether NA values should be stripped
#' before the computation proceeds. Default to `TRUE`.
#'
#' @return The coefficient of variation of `x`.
#' @export
#'
#' @importFrom stats sd
#'
#' @author Antoine Bichat
#'
#' @examples
#' cv(1:10)
cv <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm))
}




#' Feature selection step using the coefficient of variation
#'
#' Select variables with highest coefficient of variation.
#'
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
#' @inheritParams var_to_keep
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
#'   recipe(Species ~ ., data = iris) %>%
#'   step_select_cv(all_numeric_predictors(), n_kept = 2) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' juice(rec)
step_select_cv <- function(recipe, ..., role = NA, trained = FALSE,
                           n_kept = NULL,
                           prop_kept = NULL,
                           cutoff = NULL,
                           res = NULL,
                           skip = FALSE, id = rand_id("select_cv")) {

  add_step(
    recipe,
    step_select_cv_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      n_kept = n_kept,
      prop_kept = prop_kept,
      cutoff = cutoff,
      res = res,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_select_cv_new <- function(terms, role, trained,
                               n_kept, prop_kept, cutoff,
                               res, skip, id) {

  step(subclass = "select_cv",
       terms = terms,
       role = role,
       trained = trained,
       n_kept = n_kept,
       prop_kept = prop_kept,
       cutoff = cutoff,
       res = res,
       skip = skip,
       id = id)
}

#' @export
#' @importFrom dplyr mutate
#' @importFrom recipes check_type recipes_eval_select
#' @importFrom tibble enframe
prep.step_select_cv <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)


  res_cv <-
    training[, col_names] %>%
    apply(2, cv) %>%
    enframe(name = "terms", value = "cv") %>%
    mutate(kept = var_to_keep(.data$cv, x$n_kept, x$prop_kept, x$cutoff,
                              maximize = TRUE))

  step_select_cv_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    n_kept = x$n_kept,
    prop_kept = x$prop_kept,
    cutoff = x$cutoff,
    res = res_cv,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
bake.step_select_cv <- function(object, new_data, ...) {
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
print.step_select_cv <- function(x,
                                 width = max(20, options()$width - 35), ...) {
  title <- "Top CV filtering on "

  print_step(
    tr_obj = x$res$terms,
    untr_obj = x$terms,
    trained = x$trained,
    title = title,
    width = width
  )
  invisible(x)
}

#' @rdname step_select_cv
#' @param x A `step_select_cv` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_select_cv <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        cv = rlang::na_dbl,
        rank = rlang::na_dbl,
        kept = rlang::na_lgl
      )
  }

  res$id <- x$id
  res
}

