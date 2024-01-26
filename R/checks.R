check_in <- function(x, values, name_x = "x") {

  if (length(x) != 1) {
    rlang::abort(paste0("`", name_x, "` must be a length-one vector."))
  }


  if (!x %in% values) {
    all_values <- paste0(paste0('"', values, '"'), collapse = ", ")
    rlang::abort(paste0("`", name_x, "` must be one of ",
                        all_values, '; not "', x, '".'))
  }

  invisible(x)
}


check_binary <- function(x, name_x = "x") {
  if (length(unique(x)) != 2) {
    rlang::abort(paste0("`", name_x, "` must be a binary vector."))
  }

  invisible(x)
}
