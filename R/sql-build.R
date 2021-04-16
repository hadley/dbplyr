#' Build and render SQL from a sequence of lazy operations
#'
#' `sql_build()` creates a `select_query` S3 object, that is rendered
#' to a SQL string by `sql_render()`. The output from `sql_build()` is
#' designed to be easy to test, as it's database agnostic, and has
#' a hierarchical structure. Outside of testing, however, you should
#' always call `sql_render()`.
#'
#' `sql_build()` is generic over the lazy operations, \link{lazy_ops},
#' and generates an S3 object that represents the query. `sql_render()`
#' takes a query object and then calls a function that is generic
#' over the database. For example, `sql_build.op_mutate()` generates
#' a `select_query`, and `sql_render.select_query()` calls
#' `sql_select()`, which has different methods for different databases.
#' The default methods should generate ANSI 92 SQL where possible, so you
#' backends only need to override the methods if the backend is not ANSI
#' compliant.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default `NULL` uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con = NULL, ...) {
  unique_subquery_name_reset()
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, con = op$src$con, ...) {
  # only used for testing
  qry <- sql_build(op$ops, con = con, ...)
  sql_optimise(qry, con = con, ...)
}

#' @export
sql_build.ident <- function(op, con = NULL, ...) {
  op
}


# Render ------------------------------------------------------------------

#' @export
#' @rdname sql_build
#' @param subquery Is this SQL going to be used in a subquery?
#'   This is important because you can place a bare table name in a subquery
#'   and  ORDER BY does not work in subqueries.
sql_render <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  UseMethod("sql_render")
}

#' @export
sql_render.tbl_lazy <- function(query, con = query$src$con, ..., subquery = FALSE, lvl = 0) {
  sql_render(query$ops, con = con, ..., subquery = subquery, lvl = lvl)
}

#' @export
sql_render.op <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  qry <- sql_build(query, con = con, ...)
  qry <- sql_optimise(qry, con = con, ..., subquery = subquery)
  sql_render(qry, con = con, ..., subquery = subquery, lvl = lvl)
}

#' @export
sql_render.sql <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  query
}

#' @export
sql_render.ident <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  if (subquery) {
    query
  } else {
    dbplyr_query_select(con, sql("*"), query, lvl = lvl)
  }
}

# Optimise ----------------------------------------------------------------

#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ..., subquery = FALSE) {
  UseMethod("sql_optimise")
}

#' @export
sql_optimise.sql <- function(x, con = NULL, ...) {
  # Can't optimise raw SQL
  x
}

#' @export
sql_optimise.ident <- function(x, con = NULL, ...) {
  x
}
