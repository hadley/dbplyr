#' @importFrom dplyr show_query
#' @export
show_query.tbl_lazy <- function(x, ..., cte = TRUE) {
  sql <- remote_query(x, cte = cte)
  cat_line("<SQL>")
  cat_line(sql)
  invisible(x)
}

#' @importFrom dplyr explain
#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x))

  invisible(x)
}
