#' Metadata about a remote table
#'
#' `remote_name()` gives the name remote table, or `NULL` if it's a query.
#' `remote_query()` gives the text of the query, and `remote_query_plan()`
#' the query plan (as computed by the remote database). `remote_src()` and
#' `remote_con()` give the dplyr source and DBI connection respectively.
#'
#' @param x Remote table, currently must be a [tbl_sql].
#' @return The value, or `NULL` if not remote table, or not applicable.
#'    For example, computed queries do not have a "name"
#' @export
#' @examples
#' mf <- memdb_frame(x = 1:5, y = 5:1, .name = "blorp")
#' remote_name(mf)
#' remote_src(mf)
#' remote_con(mf)
#' remote_query(mf)
#'
#' mf2 <- dplyr::filter(mf, x > 3)
#' remote_name(mf2)
#' remote_src(mf2)
#' remote_con(mf2)
#' remote_query(mf2)
remote_name <- function(x) {
  UseMethod("remote_name")
}

#' @export
remote_name.tbl_lazy <- function(x) {
  remote_name(x$ops)
}

#' @export
remote_name.op_base <- function(x) {
  x$x
}

#' @export
remote_name.op_group_by <- function(x) {
  remote_name(x$x)
}

#' @export
remote_name.op_ungroup <- function(x) {
  remote_name(x$x)
}

#' @export
remote_name.default <- function(x) {
  return()
}

#' @export
#' @rdname remote_name
remote_src <- function(x) {
  x$src
}

#' @export
#' @rdname remote_name
remote_con <- function(x) {
  x$src$con
}

#' @export
#' @rdname remote_name
remote_query <- function(x) {
  db_sql_render(remote_con(x), x)
}

#' @export
#' @rdname remote_name
remote_query_plan <- function(x) {
  dbplyr_explain(remote_con(x), db_sql_render(remote_con(x), x$ops))
}
