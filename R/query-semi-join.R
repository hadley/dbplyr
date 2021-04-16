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
sql_render.semi_join_query <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  from_x <- dbplyr_sql_subquery(
    con,
    sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1),
    name = "LHS",
    lvl = lvl
  )
  from_y <- dbplyr_sql_subquery(
    con,
    sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1),
    name = "RHS",
    lvl = lvl
  )

  dbplyr_query_semi_join(con, from_x, from_y, anti = query$anti, by = query$by, lvl = lvl)
}
