#' Feature aggregation step based on a hierarchical clustering
#'
#' Aggregate variables according to hierarchical clustering.
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
#' @param n_clusters Number of cluster to create.
#' @param fun_agg Aggregation function like `sum` or `mean`.
#' @param dist_metric Default to `euclidean`. See [stats::dist()] for more
#' details.
#' @param linkage_method Default to `complete`. See [stats::hclust()] for more
#' details.
#' @param res This parameter is only produced after the recipe has been trained.
#' @param prefix A character string for the prefix of the resulting new
#' variables.
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
#' rec <-
#'   iris %>%
#'   recipe(formula = Species ~ .) %>%
#'   step_aggregate_hclust(all_numeric_predictors(),
#'                         n_clusters = 2, fun_agg = sum) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' bake(rec, new_data = NULL)
step_aggregate_hclust <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  n_clusters,
  fun_agg,
  dist_metric = "euclidean",
  linkage_method = "complete",
  res = NULL,
  prefix = "cl_",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("aggregate_hclust")
) {
  add_step(
    recipe,
    step_aggregate_hclust_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      n_clusters = n_clusters,
      fun_agg = fun_agg,
      dist_metric = dist_metric,
      linkage_method = linkage_method,
      res = res,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_aggregate_hclust_new <- function(
  terms,
  role,
  trained,
  n_clusters,
  fun_agg,
  dist_metric,
  linkage_method,
  res,
  prefix,
  keep_original_cols,
  skip,
  id
) {
  step(
    subclass = "aggregate_hclust",
    terms = terms,
    role = role,
    trained = trained,
    n_clusters = n_clusters,
    fun_agg = fun_agg,
    dist_metric = dist_metric,
    linkage_method = linkage_method,
    res = res,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
#' @importFrom dplyr mutate
#' @importFrom recipes recipes_eval_select
#' @importFrom rlang .data
#' @importFrom stats cutree dist hclust
#' @importFrom tibble enframe
prep.step_aggregate_hclust <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)
  check_in(
    x$dist_metric,
    name_x = "dist_metric",
    values = c(
      "euclidean",
      "maximum",
      "manhattan",
      "canberra",
      "binary",
      "minkowski"
    )
  )
  check_in(
    x$linkage_method,
    name_x = "linkage_method",
    values = c(
      "ward.D",
      "ward.D2",
      "single",
      "complete",
      "average",
      "mcquitty",
      "median",
      "centroid"
    )
  )

  ct <-
    training[, col_names] %>%
    as.matrix() %>%
    t() %>%
    dist(method = x$dist_metric) %>%
    hclust(method = x$linkage_method) %>%
    cutree(k = x$n_clusters)

  res_agg_hclust <-
    ct %>%
    enframe(name = "terms", value = "aggregate") %>%
    mutate(aggregate = paste0(x$prefix, .data$aggregate))

  step_aggregate_hclust_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    n_clusters = x$n_clusters,
    fun_agg = x$fun_agg,
    dist_metric = x$dist_metric,
    linkage_method = x$linkage_method,
    res = res_agg_hclust,
    prefix = x$prefix,
    keep_original_cols = x$keep_original_cols,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
bake.step_aggregate_hclust <- function(object, new_data, ...) {
  col_names <- object$res$terms
  check_new_data(col_names, object, new_data)

  list_agg_hc <- split(object$res$terms, object$res$aggregate)

  aggregate_var(
    new_data,
    list_agg = list_agg_hc,
    fun_agg = object$fun_agg,
    prefix = object$prefix,
    keep_original_cols = object$keep_original_cols
  )
}

#' @export
#' @importFrom recipes print_step
print.step_aggregate_hclust <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- paste("`hclust` aggregation of ")

    print_step(
      tr_obj = x$res$terms,
      untr_obj = x$terms,
      trained = x$trained,
      title = title,
      width = width
    )
    invisible(x)
  }


#' @rdname step_aggregate_hclust
#' @param x A `step_aggregate_hclust` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_aggregate_hclust <- function(x, ...) {
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
required_pkgs.step_aggregate_hclust <- function(x, ...) {
  c("scimo")
}
