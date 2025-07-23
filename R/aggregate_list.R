#' Feature aggregation step based on a defined list
#'
#' Aggregate variables according to prior knowledge.
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
#' @param list_agg Named list of aggregated variables.
#' @param fun_agg  Aggregation function like `sum` or `mean`.
#' @param others Behavior for the selected variables in `...` that are not
#' present in `list_agg`. If `discard` (the default), they are not kept.
#' If `asis`, they are kept without modification. If `aggregate`, they are
#' aggregated in a new variable.
#' @param name_others If `others` is set to `aggregate`, name of the
#' aggregated variable. Not used otherwise.
#' @param res This parameter is only produced after the recipe has been trained.
#' @param prefix A character string for the prefix of the resulting new
#' variables that are not named in `list_agg`.
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
#' @examples
#' list_iris <- list(sepal.size = c("Sepal.Length", "Sepal.Width"),
#'                   petal.size = c("Petal.Length", "Petal.Width"))
#' rec <-
#'   iris %>%
#'   recipe(formula = Species ~ .) %>%
#'   step_aggregate_list(all_numeric_predictors(),
#'                       list_agg = list_iris, fun_agg = prod) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' bake(rec, new_data = NULL)
step_aggregate_list <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  list_agg = NULL,
  fun_agg = NULL,
  others = "discard",
  name_others = "others",
  res = NULL,
  prefix = "agg_",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("aggregate_list")
) {
  add_step(
    recipe,
    step_aggregate_list_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      list_agg = list_agg,
      fun_agg = fun_agg,
      others = others,
      name_others = name_others,
      res = res,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_aggregate_list_new <- function(
  terms,
  role,
  trained,
  list_agg,
  fun_agg,
  others,
  name_others,
  res,
  prefix,
  keep_original_cols,
  skip,
  id
) {
  step(
    subclass = "aggregate_list",
    terms = terms,
    role = role,
    trained = trained,
    list_agg = list_agg,
    fun_agg = fun_agg,
    others = others,
    name_others = name_others,
    res = res,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
#' @importFrom dplyr if_else left_join
#' @importFrom recipes recipes_eval_select
#' @importFrom rlang .data .env
#' @importFrom tibble enframe tibble
#' @importFrom tidyr unnest_longer
prep.step_aggregate_list <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_in(
    x$others,
    name_x = "others",
    values = c("discard", "asis", "aggregate")
  )

  updt_list_agg <- x$list_agg

  if (x$others == "aggregate") {
    other_cols <- setdiff(col_names, unlist(x$list_agg))
    if (length(other_cols) != 0) {
      updt_list_agg[[x$name_others]] <- other_cols
    }
  }

  df_agg <-
    updt_list_agg %>%
    fill_name(prefix = x$prefix) %>%
    enframe(name = "aggregate", value = "terms") %>%
    unnest_longer("terms")

  res_agg_list <-
    tibble(terms = unname(col_names)) %>%
    left_join(df_agg, by = "terms")

  if (x$others == "asis") {
    asis <- setdiff(col_names, unlist(updt_list_agg))
    res_agg_list <-
      res_agg_list %>%
      mutate(
        aggregate = if_else(
          .data$terms %in% .env$asis,
          .data$terms,
          .data$aggregate
        )
      )
  }

  step_aggregate_list_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    list_agg = updt_list_agg,
    fun_agg = x$fun_agg,
    others = x$others,
    name_others = x$name_others,
    res = res_agg_list,
    prefix = x$prefix,
    keep_original_cols = x$keep_original_cols,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
bake.step_aggregate_list <- function(object, new_data, ...) {
  col_names <- object$res$terms
  check_new_data(col_names, object, new_data)

  new_df <-
    aggregate_var(
      new_data,
      list_agg = object$list_agg,
      fun_agg = object$fun_agg,
      prefix = object$prefix,
      keep_original_cols = object$keep_original_cols
    )

  if (object$others == "discard" && !object$keep_original_cols) {
    to_discard <-
      col_names %>%
      setdiff(unlist(object$list_agg)) %>%
      setdiff(names(object$list_agg))

    new_df[, to_discard] <- NULL
  }

  new_df
}

#' @export
#' @importFrom recipes print_step
print.step_aggregate_list <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- paste("Aggregation of ")

    print_step(
      tr_obj = x$res$terms,
      untr_obj = x$terms,
      trained = x$trained,
      title = title,
      width = width
    )
    invisible(x)
  }


#' @rdname step_aggregate_list
#' @param x A `step_aggregate_list` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_aggregate_list <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$res
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        aggregate = rlang::na_chr
      )
  }
  # Always return the step id:
  res$id <- x$id
  res
}


#' @rdname required_pkgs.scimo
#' @export
required_pkgs.step_aggregate_list <- function(x, ...) {
  c("scimo")
}
