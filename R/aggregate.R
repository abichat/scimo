aggregate_var <- function(
  data,
  list_agg,
  fun_agg,
  prefix = "agg_",
  keep_original_cols = FALSE
) {
  list_agg <- fill_name(list_agg, prefix)

  for (i in seq_along(list_agg)) {
    data[[names(list_agg[i])]] <- apply(
      data[, list_agg[[i]]],
      MARGIN = 1,
      FUN = fun_agg
    )
  }

  if (!keep_original_cols) {
    data[, unique(unlist(list_agg))] <- NULL
  }

  data
}


fill_name <- function(list_agg, prefix) {
  default_names <- paste0(prefix, seq_along(list_agg))

  if (is.null(names(list_agg))) {
    names(list_agg) <- default_names
  } else {
    names(list_agg)[names(list_agg) == ""] <-
      default_names[names(list_agg) == ""]
  }

  list_agg
}
