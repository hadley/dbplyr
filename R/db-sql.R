#' SQL generation generics
#'
#' @description
#'
#' SQL translation:
#'
#' * `sql_expr_matches(con, x, y)` generates an alternative to `x = y` when a
#'   pair of `NULL`s should match. The default translation uses a `CASE WHEN`
#'   as described in <https://modern-sql.com/feature/is-distinct-from>.
#'
#' * `sql_translation(con)` generates a SQL translation environment.
#'
#' Tables:
#'
#' * `sql_table_analyze(con, table)` generates SQL that "analyzes" the table,
#'   ensuring that the database has up-to-date statistics for use in the query
#'   planner. It called from [copy_to()] when `analyze = TRUE`.
#'
#' * `sql_table_index()` generates SQL for adding an index to table. The
#'
#' Query manipulation:
#'
#' * `sql_query_explain(con, sql)` generates SQL that "explains" a query,
#'   i.e. generates a query plan describing what indexes etc that the
#'   database will use.
#'
#' * `sql_query_fields()` generates SQL for a 0-row result that is used to
#'   capture field names in [tbl_sql()]
#'
#' * `sql_query_save(con, sql)` generates SQL for saving a query into a
#'   (temporary) table.
#'
#' * `sql_query_wrap(con, from)` generates SQL for wrapping a query into a
#'   subquery.
#'
#' Query generation:
#'
#' * `sql_query_select()` generate SQL for a `SELECT` query
#' * `sql_query_join()` generate SQL for joins
#' * `sql_query_semi_join()` generate SQL for semi- and anti-joins
#' * `sql_query_set_op()` generate SQL for `UNION`, `INTERSECT`, and `EXCEPT`
#'   queries.
#'
#' @section dbplyr 2.0.0:
#'
#' Many `dplyr::db_*` generics have been replaced by `dbplyr::sql_*` generics.
#' To update your backend, you'll need to extract the SQL generation out of your
#' existing code, and place it in a new method for a dbplyr `sql_` generic.
#'
#' * `dplyr::db_analyze()` is replaced by `dbplyr::sql_table_analyze()`
#' * `dplyr::db_explain()` is replaced by `dbplyr::sql_query_explain()`
#' * `dplyr::db_create_index()` is replaced by `dbplyr::sql_table_index()`
#' * `dplyr::db_query_fields()` is replaced by `dbplyr::sql_query_fields()`
#' * `dplyr::db_query_rows()` is no longer used; you can delete it
#' * `dplyr::db_save_query()` is replaced by `dbplyr::sql_query_save()`
#'
#' The query generating functions have also changed names. Their behaviour is
#' unchanged, so you just need to rename the generic and import from dbplyr
#' instead of dplyr.
#'
#' * `dplyr::sql_select()` is replaced by `dbplyr::sql_query_select()`
#' * `dplyr::sql_join()` is replaced by `dbplyr::sql_query_join()`
#' * `dplyr::sql_semi_join()` is replaced by `dbplyr::sql_query_semi_join()`
#' * `dplyr::sql_set_op()` is replaced by `dbplyr::sql_query_set_op()`
#' * `dplyr::sql_subquery()` is replaced by `dbplyr::sql_query_wrap()`
#'
#' Learn more in `vignette("backend-2.0")`
#'
#' @keywords internal
#' @family generic
#' @name db-sql
NULL

#' @export
#' @rdname db-sql
sql_expr_matches <- function(con, x, y) {
  UseMethod("sql_expr_matches")
}
# https://modern-sql.com/feature/is-distinct-from
#' @export
sql_expr_matches.DBIConnection <- function(con, x, y) {
  build_sql(
    "CASE WHEN (", x, " = ", y, ") OR (", x, " IS NULL AND ", y, " IS NULL) ",
    "THEN 0 ",
    "ELSE 1 = 0",
    con = con
  )
}

#' @export
#' @rdname db-sql
sql_translation <- function(con) {
  UseMethod("sql_translation")
}
# sql_translation.DBIConnection lives in backend-.R
dbplyr_sql_translation <- function(con) {
  dbplyr_fallback(con, "sql_translate_env")
}
#' @importFrom dplyr sql_translate_env
#' @export
sql_translate_env.DBIConnection <- function(con) {
  sql_translation(con)
}


# Tables ------------------------------------------------------------------

#' @rdname db-sql
#' @export
sql_table_analyze <- function(con, table, ...) {
  UseMethod("sql_table_analyze")
}
#' @export
sql_table_analyze.DBIConnection <- function(con, table, ...) {
  build_sql("ANALYZE ", as.sql(table, con = con), con = con)
}

#' @rdname db-sql
#' @export
sql_table_index <- function(con, table, columns, name = NULL, unique = FALSE, ...) {
  UseMethod("sql_table_index")
}
#' @export
sql_table_index.DBIConnection <- function(con, table, columns, name = NULL,
                                           unique = FALSE, ...) {
  assert_that(is_string(table) | is.schema(table), is.character(columns))

  name <- name %||% paste0(c(unclass(table), columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  build_sql(
    "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", as.sql(name, con = con),
    " ON ", as.sql(table, con = con), " ", fields,
    con = con
  )
}

# Query manipulation ------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_explain <- function(con, sql, ...) {
  UseMethod("sql_query_explain")
}
#' @export
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  build_sql("EXPLAIN ", sql, con = con)
}

#' @rdname db-sql
#' @export
sql_query_fields <- function(con, sql, ...) {
  UseMethod("sql_query_fields")
}
#' @export
sql_query_fields.DBIConnection <- function(con, sql, ...) {
  dbplyr_query_select(con, sql("*"), dbplyr_sql_subquery(con, sql), where = sql("0 = 1"))
}

#' @rdname db-sql
#' @export
sql_query_save <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("sql_query_save")
}
#' @export
sql_query_save.DBIConnection <- function(con, sql, name, temporary = TRUE, ...) {
  build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "), "TABLE \n",
    as.sql(name, con), " AS ", sql,
    con = con
  )
}
#' @export
#' @rdname db-sql
sql_query_wrap <- function(con, from, name = unique_subquery_name(), ...) {
  UseMethod("sql_query_wrap")
}
#' @export
sql_query_wrap.DBIConnection <- function(con, from, name = unique_subquery_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else if (is.schema(from)) {
    setNames(as.sql(from, con), name)
  } else {
    build_sql("(", from, ") ", ident(name %||% unique_subquery_name()), con = con)
  }
}

#' @rdname db-sql
#' @export
sql_query_rows <- function(con, sql, ...) {
  UseMethod("sql_query_rows")
}
#' @export
sql_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- dbplyr_sql_subquery(con, sql, "master")
  build_sql("SELECT COUNT(*) FROM ", from, con = con)
}


# Query generation --------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_select <- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             window = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...,
                             subquery = FALSE) {
  UseMethod("sql_query_select")
}

#' @export
sql_query_select.DBIConnection <- function(con, select, from, where = NULL,
                               group_by = NULL, having = NULL,
                               window = NULL,
                               order_by = NULL,
                               limit = NULL,
                               distinct = FALSE,
                               ...,
                               subquery = FALSE) {
  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct),
    from      = sql_clause_from(con, from),
    where     = sql_clause_where(con, where),
    group_by  = sql_clause_group_by(con, group_by),
    having    = sql_clause_having(con, having),
    window    = sql_clause_window(con, window),
    order_by  = sql_clause_order_by(con, order_by, subquery, limit),
    limit     = sql_clause_limit(con, limit)
  )
}
dbplyr_query_select <- function(con, ...) {
  dbplyr_fallback(con, "sql_select", ...)
}
#' @importFrom dplyr sql_select
#' @export
sql_select.DBIConnection <- function(con, select, from, where = NULL,
                                     group_by = NULL, having = NULL,
                                     window = NULL,
                                     order_by = NULL,
                                     limit = NULL,
                                     distinct = FALSE,
                                     ...,
                                     subquery = FALSE) {
  sql_query_select(
    con, select, from,
    where = where,
    group_by = group_by,
    having = having,
    window = window,
    order_by = order_by,
    limit = limit,
    distinct = distinct,
    ...,
    subquery = subquery
  )
}

#' @rdname db-sql
#' @export
sql_query_join <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ...) {
  UseMethod("sql_query_join")
}
#' @export
sql_query_join.DBIConnection <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ...) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    cross = sql("CROSS JOIN"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  select <- sql_join_vars(con, vars)
  on <- sql_join_tbls(con, by, na_matches = na_matches)

  # Wrap with SELECT since callers assume a valid query is returned
  build_sql(
    "SELECT ", select, "\n",
    "FROM ", x, "\n",
    JOIN, " ", y, "\n",
    if (!is.null(on)) build_sql("ON ", on, "\n", con = con) else NULL,
    con = con
  )
}
dbplyr_query_join <- function(con, ...) {
  dbplyr_fallback(con, "sql_join", ...)
}
#' @export
#' @importFrom dplyr sql_join
sql_join.DBIConnection <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ...) {
  sql_query_join(
    con, x, y, vars,
    type = type,
    by = by,
    na_matches = na_matches,
    ...
  )
}

#' @rdname db-sql
#' @export
sql_query_semi_join <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  UseMethod("sql_query_semi_join")
}
#' @export
sql_query_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  lhs <- escape(ident("LHS"), con = con)
  rhs <- escape(ident("RHS"), con = con)

  on <- sql_join_tbls(con, by)

  build_sql(
    "SELECT * FROM ", x, "\n",
    "WHERE ", if (anti) sql("NOT "), "EXISTS (\n",
    "  SELECT 1 FROM ", y, "\n",
    "  WHERE ", on, "\n",
    ")",
    con = con
  )
}
dbplyr_query_semi_join <- function(con, ...) {
  dbplyr_fallback(con, "sql_semi_join", ...)
}
#' @export
#' @importFrom dplyr sql_semi_join
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  sql_query_semi_join(con, x, y, anti = anti, by = by, ...)
}

#' @rdname db-sql
#' @export
sql_query_set_op <- function(con, x, y, method, ..., all = FALSE) {
  UseMethod("sql_query_set_op")
}
#' @export
sql_query_set_op.DBIConnection <- function(con, x, y, method, ..., all = FALSE) {
  build_sql(
    "(", x, ")",
    "\n", sql(method), if (all) sql(" ALL"), "\n",
    "(", y, ")",
    con = con
  )
}
dbplyr_query_set_op <- function(con, ...) {
  dbplyr_fallback(con, "sql_set_op", ...)
}
#' @importFrom dplyr sql_set_op
#' @export
sql_set_op.DBIConnection <- function(con, x, y, method) {
  # dplyr::sql_set_op() doesn't have ...
  sql_query_set_op(con, x, y, method)
}


# dplyr fallbacks ---------------------------------------------------------

dbplyr_analyze <- function(con, ...) {
  dbplyr_fallback(con, "db_analyze", ...)
}
#' @export
#' @importFrom dplyr db_analyze
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return()
  }
  dbExecute(con, sql)
}

dbplyr_create_index <- function(con, ...) {
  dbplyr_fallback(con, "db_create_index", ...)
}
#' @export
#' @importFrom dplyr db_create_index
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  sql <- sql_table_index(con, table, columns, name = name, unique = unique, ...)
  dbExecute(con, sql)
}

dbplyr_explain <- function(con, ...) {
  dbplyr_fallback(con, "db_explain", ...)
}
#' @export
#' @importFrom dplyr db_explain
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  expl <- dbGetQuery(con, sql)
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

dbplyr_query_fields <- function(con, ...) {
  dbplyr_fallback(con, "db_query_fields", ...)
}
#' @export
#' @importFrom dplyr db_query_fields
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_fields(con, sql, ...)
  names(dbGetQuery(con, sql))
}

dbplyr_save_query <- function(con, ...) {
  dbplyr_fallback(con, "db_save_query", ...)
}
#' @export
#' @importFrom dplyr db_save_query
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE, ...) {
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)
  dbExecute(con, sql, immediate = TRUE)
  name
}

dbplyr_sql_subquery <- function(con, ...) {
  dbplyr_fallback(con, "sql_subquery", ...)
}
#' @export
#' @importFrom dplyr sql_subquery
sql_subquery.DBIConnection <- function(con, from, name = unique_subquery_name(), ...) {
  sql_query_wrap(con, from = from, name = name, ...)
}
