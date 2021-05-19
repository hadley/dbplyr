#' @export
#' @rdname sql_build
join_query <- function(x, y, vars, type = "inner", by = NULL, suffix = c(".x", ".y"), na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by,
      na_matches = na_matches
    ),
    class = c("join_query", "query")
  )
}

#' @export
print.join_query <- function(x, ...) {
  cat_line("<SQL JOIN (", toupper(x$type), ")>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.join_query <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  from_x_query_list <- sql_render(query$x, con, ..., subquery = TRUE, query_list = query_list)
  from_x <- query_list_from(from_x_query_list, con, "LHS")
  from_y_query_list <- sql_render(query$y, con, ..., subquery = TRUE, query_list = from_x_query_list)
  from_y <- query_list_from(from_y_query_list, con, "RHS")

  join_sql <- dbplyr_query_join(con, from_x, from_y,
    vars = query$vars,
    type = query$type,
    by = query$by,
    na_matches = query$na_matches
  )

  cte_wrap(from_y_query_list, join_sql, render = TRUE)
}

# SQL generation ----------------------------------------------------------


sql_join_vars <- function(con, vars) {
  sql_vector(
    mapply(
      FUN = sql_join_var,
      alias = vars$alias,
      x = vars$x,
      y = vars$y,
      MoreArgs = list(con = con, all_x = vars$all_x, all_y = vars$all_y),
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    ),
    parens = FALSE,
    collapse = ", ",
    con = con
  )
}

sql_join_var <- function(con, alias, x, y, all_x, all_y) {
  if (!is.na(x) && !is.na(y)) {
    sql_expr(
      COALESCE(
        !!sql_table_prefix(con, x, table = "LHS"),
        !!sql_table_prefix(con, y, table = "RHS")
      ),
      con = con
    )
  } else if (!is.na(x)) {
    sql_table_prefix(con, x, table = if (x %in% all_y) "LHS")
  } else if (!is.na(y)) {
    sql_table_prefix(con, y, table = if (y %in% all_x) "RHS")
  } else {
    stop("No source for join column ", alias, call. = FALSE)
  }
}

sql_join_tbls <- function(con, by, na_matches = "never") {
  na_matches <- arg_match(na_matches, c("na", "never"))

  on <- NULL
  if (na_matches == "na" || length(by$x) + length(by$y) > 0) {
    lhs <- sql_table_prefix(con, by$x, "LHS")
    rhs <- sql_table_prefix(con, by$y, "RHS")

    if (na_matches == "na") {
      compare <- purrr::map_chr(seq_along(lhs), function(i) {
        sql_expr_matches(sql(lhs[[i]]), sql(rhs[[i]]), con = con)
      })
    } else {
      compare <- paste0(lhs, " = ", rhs)
    }

    on <- sql_vector(compare, collapse = " AND ", parens = TRUE, con = con)
  } else if (length(by$on) > 0) {
    on <- build_sql("(", by$on, ")", con = con)
  }

  on
}

sql_table_prefix <- function(con, var, table = NULL) {
  var <- sql_escape_ident(con, var)

  if (!is.null(table)) {
    table <- sql_escape_ident(con, table)
    sql(paste0(table, ".", var))
  } else {
    var
  }
}

utils::globalVariables("COALESCE")
