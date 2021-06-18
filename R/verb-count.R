#' Count observations by group
#'
#' These are methods for the dplyr [count()] and [tally()] generics. They
#' wrap up [group_by.tbl_lazy()], [summarise.tbl_lazy()] and, optionally,
#' [arrange.tbl_lazy()].
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::count
#' @param .drop Not supported for lazy tables.
#' @importFrom dplyr count
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(g = c(1, 1, 1, 2, 2), x = c(4, 3, 6, 9, 2))
#' db %>% count(g) %>% show_query()
#' db %>% count(g, wt = x) %>% show_query()
#' db %>% count(g, wt = x, sort = TRUE) %>% show_query()
count.tbl_lazy <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
  } else {
    out <- x
  }
  out <- tally(out, wt = {{ wt }}, sort = sort, name = name)
  out <- group_by(out, !!!syms(group_vars(x)))
  out
}

#' @rdname count.tbl_lazy
#' @importFrom dplyr add_count
#' @importFrom lifecycle deprecated
#' @export
add_count.tbl_lazy <- function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated()) {
  if (!missing(.drop)) {
    abort("`.drop` argument not supported for lazy tables.")
  }
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
  }
  else {
    out <- x
  }
  out <- dplyr::add_tally(out, wt = !!enquo(wt), sort = sort, name = name)
  group_by(out, !!!syms(group_vars(x)))
}

#' @rdname count.tbl_lazy
#' @importFrom dplyr tally
#' @export
tally.tbl_lazy <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  wt <- enquo(wt)
  if (quo_is_null(wt)) {
    n <- expr(n())
  } else {
    n <- expr(sum(!!wt, na.rm = TRUE))
  }

  name <- check_name(name, group_vars(x))

  out <- summarise(x, !!name := !!n, .groups = "drop")

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

check_name <- function(name, vars) {
  name <- name %||% "n"

  if (!name %in% vars) {
    return(name)
  }

  abort(c(
    glue("'{name}' already present in output"),
    i = "Use `name = \"new_name\"` to pick a new name."
  ))
}
