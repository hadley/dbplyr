sql_select_clauses <- function(con,
                               select,
                               from,
                               where,
                               group_by,
                               having,
                               window,
                               order_by,
                               limit = NULL) {
  out <- list(
    select = select,
    from = from,
    where = where,
    group_by = group_by,
    having = having,
    window = window,
    order_by = order_by,
    limit = limit
  )
  out <- purrr::compact(out)
  escape(unname(out), collapse = "\n", parens = FALSE, con = con)
}

sql_clause_select <- function(con, select, distinct = FALSE, top = NULL) {
  assert_that(is.character(select))
  if (is_empty(select)) {
    abort("Query contains no columns")
  }

  build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    if (!is.null(top)) build_sql("TOP ", as.integer(top), " ", con = con),
    escape(select, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_from  <- function(con, from) {
  sql_clause_generic(con, "FROM", from)
}

sql_clause_where <- function(con, where) {
  if (length(where) == 0L) {
    return()
  }

  assert_that(is.character(where))
  where_paren <- escape(where, parens = TRUE, con = con)
  build_sql(
    "WHERE ", sql_vector(where_paren, collapse = " AND ", con = con),
    con = con
  )
}

sql_clause_group_by <- function(con, group_by) {
  sql_clause_generic(con, "GROUP BY", group_by)
}

sql_clause_having <- function(con, having) {
  sql_clause_generic(con, "HAVING", having)
}

sql_clause_window <- function(con, window) {
  sql_clause_generic(con, "WINDOW", window)
}

sql_clause_order_by <- function(con, order_by, subquery = FALSE, limit = NULL) {
  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause_generic(con, "ORDER BY", order_by)
  }
}

sql_clause_limit <- function(con, limit){
  if (!is.null(limit) && !identical(limit, Inf)) {
    build_sql(
      "LIMIT ", sql(format(limit, scientific = FALSE)),
      con = con
    )
  }
}

# helpers -----------------------------------------------------------------

sql_clause_generic <- function(con, clause, fields) {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  build_sql(
    sql(clause), " ",
    escape(fields, collapse = ", ", con = con),
    con = con
  )
}
