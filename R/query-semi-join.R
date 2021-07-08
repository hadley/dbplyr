#' @export
#' @rdname sql_build
semi_join_query <- function(x, y, anti = FALSE, by = NULL, na_matches = FALSE, lhs_as = "LHS", rhs_as = "RHS") {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by,
      na_matches = na_matches,
      lhs_as = lhs_as,
      rhs_as = rhs_as
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
sql_render.semi_join_query <- function(query, con = NULL, ..., subquery = FALSE) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE)

  dbplyr_query_semi_join(con, from_x, from_y,
    anti = query$anti,
    by = query$by,
    lhs_as = query$lhs_as,
    rhs_as = query$rhs_as
  )
}
