#' Backend: Teradata
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Uses `TOP` instead of `LIMIT`
#' * Selection of user supplied translations
#'
#' Use `simulate_teradata()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-teradata
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_teradata())
#' lf %>% head()
NULL

#' @export
#' @rdname backend-teradata
simulate_teradata <- function() simulate_dbi("Teradata")

#' @export
dbplyr_edition.Teradata <- function(con) {
  2L
}

#' @export
sql_query_select.Teradata <- function(con, select, from, where = NULL,
                                             group_by = NULL, having = NULL,
                                             order_by = NULL,
                                             limit = NULL,
                                             distinct = FALSE,
                                             ...,
                                             subquery = FALSE,
                                             lvl = 0) {

  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct, top = limit, lvl = lvl),
    from      = sql_clause_from(con, from, lvl = lvl),
    where     = sql_clause_where(con, where, lvl = lvl),
    group_by  = sql_clause_group_by(con, group_by, lvl = lvl),
    having    = sql_clause_having(con, having, lvl = lvl),
    order_by  = sql_clause_order_by(con, order_by, subquery, limit, lvl = lvl)
  )
}

#' @export
sql_translation.Teradata <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      `!=`          = sql_infix("<>"),
      bitwNot       = sql_prefix("BITNOT", 1),
      bitwAnd       = sql_prefix("BITAND", 2),
      bitwOr        = sql_prefix("BITOR", 2),
      bitwXor       = sql_prefix("BITXOR", 2),
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),
      as.numeric    = sql_cast("NUMERIC"),
      as.double     = sql_cast("NUMERIC"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      log10         = sql_prefix("LOG"),
      log           = sql_log(),
      cot           = sql_cot(),
      quantile = sql_quantile("APPROX_PERCENTILE"),
      median = sql_median("APPROX_PERCENTILE"),
      nchar         = sql_prefix("CHARACTER_LENGTH"),
      ceil          = sql_prefix("CEILING"),
      ceiling       = sql_prefix("CEILING"),
      atan2         = function(x, y) {
                        sql_expr(ATAN2(!!y, !!x))
                      },
      substr        = function(x, start, stop) {
                        len <- stop - start + 1
                        sql_expr(SUBSTR(!!x, !!start, !!len))
                      },
      paste         =  function(...) {
                        stop(
                          "`paste()`` is not supported in this SQL variant, try `paste0()` instead",
                          call. = FALSE
                        )
                      }
    ),
    sql_translator(.parent = base_odbc_agg,
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()"),
      var           = sql_aggregate("VAR_SAMP", "var"),
    ),
    sql_translator(.parent = base_odbc_win,
      cor           = win_absent("cor"),
      cov           = win_absent("cov"),
      var           = win_recycled("VAR_SAMP")
    )

  )}

#' @export
sql_table_analyze.Teradata <- function(con, table, ...) {
  # https://www.tutorialspoint.com/teradata/teradata_statistics.htm
  build_sql("COLLECT STATISTICS ", as.sql(table, con = con) , con = con)
}

utils::globalVariables(c("ATAN2", "SUBSTR"))
