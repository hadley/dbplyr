#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type = type, all = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      type = type,
      all = all
    ),
    class = c("set_op_query", "query")
  )
}

#' @export
print.set_op_query <- function(x, ...) {
  cat_line("<SQL ", toupper(x$type), ">")

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.set_op_query <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  from_x_query_list <- sql_render(query$x, con, ..., subquery = FALSE, query_list = query_list)
  from_x <- query_list_from(from_x_query_list, con)
  from_y_query_list <- sql_render(query$y, con, ..., subquery = FALSE, query_list = from_x_query_list)
  from_y <- query_list_from(from_y_query_list, con)

  from_x <- dbplyr_query_select(con, sql("*"), from_x)
  from_y <- dbplyr_query_select(con, sql("*"), from_y)

  if (dbplyr_edition(con) >= 2) {
    set_op_sql <- sql_query_set_op(con, from_x, from_y, method = query$type, all = query$all)
  } else {
    if (isTRUE(query$all)) {
      abort("`all` argument not supported by this backend")
    }
    set_op_sql <- dbplyr_query_set_op(con, from_x, from_y, method = query$type)
  }

  cte_wrap(from_y_query_list, set_op_sql, render = TRUE)
}
