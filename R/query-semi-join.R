#' @export
#' @rdname sql_build
semi_join_query <- function(x, y, anti = FALSE, by = NULL, na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by,
      na_matches = na_matches
    ),
    class = c("semi_join_query", "query")
  )
}

#' @export
print.semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.semi_join_query <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  from_x_query_list <- sql_render(query$x, con, ..., subquery = TRUE, query_list = query_list)
  from_x <- query_list_from(from_x_query_list, con, "LHS")
  from_y_query_list <- sql_render(query$y, con, ..., subquery = TRUE, query_list = from_x_query_list)
  from_y <- query_list_from(from_y_query_list, con, "RHS")

  join_sql <- dbplyr_query_semi_join(con, from_x, from_y, anti = query$anti, by = query$by)

  cte_wrap(from_y_query_list, join_sql, render = TRUE)
}
