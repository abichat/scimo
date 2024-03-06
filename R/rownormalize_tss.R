#' Feature normalization step using total sum scaling
#'
#' Normalize a set of variables by converting them to proportion, making
#' them sum to 1. Also known as simplex projection.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
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
#'   step_rownormalize_tss(all_numeric_predictors()) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' juice(rec)
step_rownormalize_tss <- function(recipe, ..., role = NA, trained = FALSE,
                                    res = NULL, skip = FALSE,
                                    id = rand_id("rownormalize_tss")) {

  add_step(
    recipe,
    step_normalize_tss_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      res = res,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_normalize_tss_new <- function(terms, role, trained,
                                   n_kept, prop_kept, cutoff,
                                   res, skip, id) {

  step(subclass = "rownormalize_tss",
       terms = terms,
       role = role,
       trained = trained,
       res = res,
       skip = skip,
       id = id)
}

#' @export
#' @importFrom recipes check_type recipes_eval_select
#' @importFrom tibble tibble
prep.step_rownormalize_tss <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)

  res_normalize_tss <- tibble(terms = unname(col_names))

  step_normalize_tss_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = res_normalize_tss,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
bake.step_rownormalize_tss <- function(object, new_data, ...) {
  col_names <- object$res$terms
  check_new_data(col_names, object, new_data)

  new_data[col_names] <- new_data[col_names] / rowSums(new_data[col_names])

  new_data
}

#' @export
#' @importFrom recipes print_step
print.step_rownormalize_tss <- function(x,
                                 width = max(20, options()$width - 35), ...) {
  title <- "TSS normalization on "

  print_step(
    tr_obj = x$res$terms,
    untr_obj = x$terms,
    trained = x$trained,
    title = title,
    width = width
  )
  invisible(x)
}

#' @rdname step_rownormalize_tss
#' @param x A `step_rownormalize_tss` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_rownormalize_tss <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names
      )
  }

  res$id <- x$id
  res
}

