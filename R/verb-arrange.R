#' Arrange rows by column values
#'
#' @description
#' This is an method for the dplyr [arrange()] generic. It generates
#' the `ORDER BY` clause of the SQL query. It also affects the
#' [window_order()] of windowed expressions in [mutate.tbl_lazy()].
#'
#' Note that `ORDER BY` clauses can not generally appear in subqueries, which
#' means that you should `arrange()` as late as possible in your pipelines.
#'
#' @section Missing values:
#' Unlike R, most databases sorts `NA` (`NULL`s) at the front. You can
#' can override this behaviour by explicitly sorting on `is.na(x)`.
#'
#' @param .data A lazy data frame backed by a database query.
#' @inheritParams dplyr::arrange
#' @return Another `tbl_lazy`. Use [show_query()] to see the generated
#'   query, and use [`collect()`][collect.tbl_sql] to execute the query
#'   and return data to R.
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db %>% arrange(a) %>% show_query()
#'
#' # Note that NAs are sorted first
#' db %>% arrange(b)
#' # override by sorting on is.na() first
#' db %>% arrange(is.na(b), b)
#' @export
#' @importFrom dplyr arrange
arrange.tbl_lazy <- function(.data, ..., .by_group = FALSE) {
  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single(
    "arrange",
    .data,
    dots = dots,
    args = list(.by_group = .by_group)
  )
}

#' @export
op_sort.op_arrange <- function(op) {
  op$dots
}

#' @export
op_desc.op_arrange <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
sql_build.op_arrange <- function(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con, context = list(clause = "ORDER"))

  if (op$args$.by_group) {
    order_vars <- c.sql(ident(op_grps(op$x)), order_vars, con = con)
  }

  select_query(
    sql_build(op$x, con),
    order_by = order_vars
  )
}
