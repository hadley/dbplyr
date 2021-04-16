#' Backend: MS Access
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * `SELECT` uses `TOP`, not `LIMIT`
#' * Non-standard types and mathematical functions
#' * String concatenation uses `&`
#' * No `ANALYZE` equivalent
#' * `TRUE` and `FALSE` converted to 1 and 0
#'
#' Use `simulate_access()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-access
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' lf <- lazy_frame(x = 1, y = 2, z = "a", con = simulate_access())
#'
#' lf %>% head()
#' lf %>% mutate(y = as.numeric(y), z = sqrt(x^2 + 10))
#' lf %>% mutate(a = paste0(z, " times"))
NULL

#' @export
#' @rdname backend-access
simulate_access <- function() simulate_dbi("ACCESS")

#' @export
dbplyr_edition.ACCESS <- function(con) {
  2L
}

# sql_ generics --------------------------------------------

#' @export
sql_query_select.ACCESS <- function(con, select, from,
                              where = NULL,  group_by = NULL,
                              having = NULL, order_by = NULL,
                              limit = NULL,  distinct = FALSE, ...,
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
sql_translation.ACCESS <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      # Much of this translation comes from: https://www.techonthenet.com/access/functions/

      # Conversion
      as.numeric    = sql_prefix("CDBL"),
      as.double     = sql_prefix("CDBL"),
      # as.integer() always rounds down. CInt does not, but Int does
      as.integer    = sql_prefix("INT"),
      as.logical    = sql_prefix("CBOOL"),
      as.character  = sql_prefix("CSTR"),
      as.Date       = sql_prefix("CDATE"),

      # Math
      exp           = sql_prefix("EXP"),
      log           = sql_prefix("LOG"),
      log10         = function(x) {
       sql_expr(log(!!x) / log(10L))
      },
      sqrt          = sql_prefix("SQR"),
      sign          = sql_prefix("SGN"),
      floor         = sql_prefix("INT"),
      # Nearly add 1, then drop off the decimal. This results in the equivalent to ceiling()
      ceiling       = function(x) {
       sql_expr(int(!!x + .9999999999))
      },
      ceil          = function(x) {
       sql_expr(int(!!x + .9999999999))
      },
      # There is no POWER function in Access. It uses ^ instead
      `^`           = function(x, y) {
       sql_expr((!!x) ^ (!!y))
      },

      # Strings
      nchar         = sql_prefix("LEN"),
      tolower       = sql_prefix("LCASE"),
      toupper       = sql_prefix("UCASE"),
      # Pull `left` chars from the left, then `right` chars from the right to replicate substr
      substr        = function(x, start, stop){
       right  <- stop - start + 1
       left   <- stop
       sql_expr(right(left(!!x, !!left), !!right))
      },
      trimws        = sql_prefix("TRIM"),
      # No support for CONCAT in Access
      paste         = sql_paste_infix(" ", "&", function(x) sql_expr(CStr(!!x))),
      paste0        = sql_paste_infix("", "&", function(x) sql_expr(CStr(!!x))),

      # Logic
      # Access always returns -1 for True and 0 for False
      is.null       = sql_prefix("ISNULL"),
      is.na         = sql_prefix("ISNULL"),
      # IIF() is like ifelse()
      ifelse        = function(test, yes, no){
       sql_expr(iif(!!test, !!yes, !!no))
      },

      # Coalesce doesn't exist in Access.
      # NZ() only works while in Access, not with the Access driver
      # IIF(ISNULL()) is the best way to construct this
      coalesce      = function(x, y) {
       sql_expr(iif(isnull(!!x), !!y, !!x))
      },

      # pmin/pmax for 2 columns
      pmin          = function(x, y) {
       sql_expr(iif(!!x <= !!y, !!x, !!y))
      },

      pmax          = function(x, y) {
       sql_expr(iif(!!x <= !!y, !!y, !!x))
      },

      # Dates
      Sys.Date      = sql_prefix("DATE")
    ),

    sql_translator(.parent = base_agg,
      sd         = sql_aggregate("STDEV"),
      var        = sql_aggregate("VAR"),
      cor        = sql_not_supported("cor"),
      cov        = sql_not_supported("cov"),

      # Count(Distinct *) does not work in Access
      # This would work but we don't know the table name when translating:
      # SELECT Count(*) FROM (SELECT DISTINCT * FROM table_name) AS T
      n_distinct    = sql_not_supported("n_distinct"),
    ),

    # Window functions not supported in Access
    sql_translator(.parent = base_no_win)
  )
}

# db_ generics -----------------------------------

#' @export
sql_table_analyze.ACCESS <- function(con, table, ...) {
  # Do nothing. Access doesn't support an analyze / update statistics function
  NULL
}

# Util -------------------------------------------

#' @export
sql_escape_logical.ACCESS <- function(con, x) {
  # Access uses a convention of -1 as True and 0 as False
  y <- ifelse(x, -1, 0)
  y[is.na(x)] <- "NULL"
  y
}
                   
#' @export                   
sql_escape_date.ACCESS <-  function(con, x) {
  # Access delimits dates using octothorpes, and uses YYYY-MM-DD
  y <- format(x, "#%Y-%m-%d#")
  y[is.na(x)] <- "NULL"
  y
}

#' @export
sql_escape_datetime.ACCESS <-  function(con, x) {
  # Access delimits datetimes using octothorpes, and uses YYYY-MM-DD HH:MM:SS
  # Timezones are not supported in Access
  y <- format(x, "#%Y-%m-%d %H:%M:%S#")
  y[is.na(x)] <- "NULL"
  y
}                   

globalVariables(c("CStr", "iif", "isnull", "text"))
