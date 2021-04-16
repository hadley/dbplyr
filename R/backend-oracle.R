#' Backend: Oracle
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Use `FETCH FIRST` instead of `LIMIT`
#' * Custom types
#' * `paste()` uses `||`
#' * Custom subquery generation (no `AS`)
#' * `setdiff()` uses `MINUS` instead of `EXCEPT`
#'
#' Use `simulate_oracle()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-oracle
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_oracle())
#' lf %>% transmute(x = paste0(c, " times"))
#' lf %>% setdiff(lf)
NULL

#' @export
#' @rdname backend-oracle
simulate_oracle <- function() simulate_dbi("Oracle")

#' @export
dbplyr_edition.Oracle <- function(con) {
  2L
}

#' @export
sql_query_select.Oracle <- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...,
                             subquery = FALSE,
                             lvl = 0) {

  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct, lvl = lvl),
    from      = sql_clause_from(con, from, lvl = lvl),
    where     = sql_clause_where(con, where, lvl = lvl),
    group_by  = sql_clause_group_by(con, group_by, lvl = lvl),
    having    = sql_clause_having(con, having, lvl = lvl),
    order_by  = sql_clause_order_by(con, order_by, subquery, limit, lvl = lvl),
    # Requires Oracle 12c, released in 2013
    limit =   if (!is.null(limit)) {
      sql <- build_sql("FETCH FIRST ", as.integer(limit), " ROWS ONLY", con = con)
      indent_lvl(sql, lvl)
    }
  )
}

#' @export
sql_translation.Oracle <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      # Data type conversions are mostly based on this article
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements001.htm

      # https://stackoverflow.com/questions/1171196
      as.character  = sql_cast("VARCHAR2(255)"),
      # https://docs.oracle.com/cd/E17952_01/mysql-5.7-en/date-and-time-functions.html#function_date
      as.Date = function(x) sql_expr(DATE(!!x)),
      # bit64::as.integer64 can translate to BIGINT for some
      # vendors, which is equivalent to NUMBER(19) in Oracle
      # https://docs.oracle.com/cd/B19306_01/gateways.102/b14270/apa.htm
      as.integer64  = sql_cast("NUMBER(19)"),
      as.numeric    = sql_cast("NUMBER"),
      as.double     = sql_cast("NUMBER"),


      # string -----------------------------------------------------------------
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/operators003.htm#i997789
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),

      # lubridate --------------------------------------------------------------
      today = function() sql_expr(TRUNC(CURRENT_TIMESTAMP)),
      now = function() sql_expr(CURRENT_TIMESTAMP)
    ),
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
sql_query_explain.Oracle <- function(con, sql, ...) {
  build_sql(
    "EXPLAIN PLAN FOR ", sql, ";\n",
    "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));",
    con = con
  )
}

#' @export
sql_table_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  build_sql("ANALYZE TABLE ", as.sql(table, con = con), " COMPUTE STATISTICS", con = con)
}

#' @export
sql_query_wrap.Oracle <- function(con, from, name = unique_subquery_name(), ..., lvl = 0) {
  # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
  if (is.ident(from)) {
    build_sql("(", from, ") ", if (!is.null(name)) ident(name), con = con)
  } else {
    build_sql(ident_subquery(from, con, lvl), " ", ident(name %||% unique_subquery_name()), con = con)
  }
}

# registered onLoad located in the zzz.R script
setdiff.tbl_Oracle <- function(x, y, copy = FALSE, ...) {
  # Oracle uses MINUS instead of EXCEPT for this operation:
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/queries004.htm
  add_op_set_op(x, y, "MINUS", copy = copy, ...)
}

#' @export
sql_expr_matches.Oracle <- function(con, x, y) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions040.htm
  build_sql("decode(", x, ", ", y, ", 0, 1) = 0", con = con)
}


# roacle package ----------------------------------------------------------

#' @export
dbplyr_edition.OraConnection <- dbplyr_edition.Oracle

#' @export
sql_translation.OraConnection <- sql_translation.Oracle

#' @export
sql_query_select.OraConnection <- sql_query_select.Oracle

#' @export
sql_table_analyze.OraConnection <- sql_table_analyze.Oracle

#' @export
sql_query_wrap.OraConnection <- sql_query_wrap.Oracle

# registered onLoad located in the zzz.R script
setdiff.OraConnection <- setdiff.tbl_Oracle

#' @export
sql_expr_matches.OraConnection <- sql_expr_matches.Oracle

globalVariables(c("DATE", "CURRENT_TIMESTAMP", "TRUNC"))
