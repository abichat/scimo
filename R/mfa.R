#' Dimension reduction step using multiple factor analysis
#'
#' Project variables into a smaller space.
#'
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the new columns created by this step from
#'  the original variables will be used as `predictors` in a model.
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
#' @param list_groups Named list specifying the groups of variables.
#' @param type_var A vector indicating the type of each group. `"c"` and `"s"`
#' are for quantitative variables (the difference is that for `"s"` variables
#' are scaled to unit variance), `"n"` for categorical variables `"m"` for
#' group of mixed variables and `"f"`for frequencies (from a contingency
#' table). By default, all variable are assumed `"c"`.
#' @param num_comp The number of components to retain as new predictors.
#' @param res This parameter is only produced after the recipe has been trained.
#' @param prefix A character string for the prefix of the resulting
#' new variables.
#' @param keep_original_cols A logical to keep the original variables in
#' the output. Defaults to `FALSE`.
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
#' @examplesIf rlang::is_installed("FactoMineR")
#' rec <-
#'   mtcars %>%
#'   transform(cyl = as.factor(cyl), vs = as.factor(vs),
#'             am = as.factor(am), gear = as.factor(gear),
#'             carb = as.factor(carb)) %>%
#'   tibble::as_tibble(rownames = "car") %>%
#'   recipe(car ~ ., data = .) %>%
#'   step_mfa(all_predictors(),
#'            list_groups = list(g1 = colnames(mtcars)[c(1, 3:7)],
#'                               g2 = colnames(mtcars)[c(2, 8:11)]),
#'            type_var = c("c", "n")) %>%
#'   prep()
#' rec
#' tidy(rec, 1)
#' bake(rec, new_data = NULL)
step_mfa <- function(recipe, ..., role = "predictor", trained = FALSE,
                     list_groups = NULL,
                     type_var = rep("c", length(list_groups)),
                     num_comp = 5,
                     res = NULL,
                     prefix = "mfa_",
                     keep_original_cols = FALSE,
                     skip = FALSE, id = rand_id("mfa")) {

  add_step(
    recipe,
    step_mfa_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      list_groups = list_groups,
      type_var = type_var,
      num_comp = num_comp,
      res = res,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_mfa_new <- function(terms, role, trained,
                         list_groups, type_var, num_comp,
                         res, prefix, keep_original_cols,
                         skip, id) {

  step(subclass = "mfa",
       terms = terms,
       role = role,
       trained = trained,
       list_groups = list_groups,
       type_var = type_var,
       num_comp = num_comp,
       res = res,
       prefix = prefix,
       keep_original_cols = keep_original_cols,
       skip = skip,
       id = id)
}

#' @export
#' @importFrom recipes recipes_eval_select
#' @importFrom rlang eval_tidy call2
prep.step_mfa <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  all_vars <- unlist(x$list_groups)
  n_vars <- vapply(x$list_groups, length, FUN.VALUE = numeric(1))

  fmr_call <- call2("MFA", .ns = "FactoMineR",
                    base = training[, all_vars],
                    group = n_vars, type = x$type_var, graph = FALSE)

  res_mfa <- eval_tidy(fmr_call)

  step_mfa_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    list_groups = x$list_groups,
    type_var = x$type_var,
    num_comp = x$num_comp,
    res = res_mfa,
    prefix = x$prefix,
    keep_original_cols = x$keep_original_cols,
    skip = x$skip,
    id = x$id
  )
}


#' @export
#' @importFrom recipes check_new_data
#' @importFrom dplyr bind_cols
bake.step_mfa <- function(object, new_data, ...) {
  col_names <- unique(c(object$res$summary.quanti$variable,
                        object$res$summary.quali$variable))
  check_new_data(col_names, object, new_data)

  ## Works only for training data (new_data = NULL)
  ## Need to do some linear algebra

  df_mfa <- object$res$ind$coord
  colnames(df_mfa) <- paste0(object$prefix, seq_len(object$num_comp))
  rownames(df_mfa) <- NULL

  new_data <- bind_cols(new_data, df_mfa)

  if (!object$keep_original_cols) {
    new_data[, unique(col_names)] <- NULL
  }

  new_data
}

#' @export
#' @importFrom recipes print_step
print.step_mfa <- function(x, width = max(20, options()$width - 35), ...) {
  title <- "MFA on "

  tr_terms <- unique(c(x$res$summary.quanti$variable,
                       x$res$summary.quali$variable))

  print_step(
    tr_obj = tr_terms,
    untr_obj = x$terms,
    trained = x$trained,
    title = title,
    width = width
  )
  invisible(x)
}

#' @rdname step_mfa
#' @param x A `step_mfa` object.
#' @export
#' @importFrom recipes is_trained sel2char
#' @importFrom tibble tibble
tidy.step_mfa <- function(x, ...) {
  if (is_trained(x)) {

    res <- tibble(terms = unlist(x$list_groups),
                  group = rep(names(x$list_groups),
                              vapply(x$list_groups, length, numeric(1))))

  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        group = rlang::na_chr
      )
  }

  res$id <- x$id
  res
}


#' @rdname required_pkgs.scimo
#' @export
required_pkgs.step_mfa <- function(x, ...) {
  c("FactoMineR", "scimo")
}
