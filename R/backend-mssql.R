#' Backend: SQL server
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * `SELECT` uses `TOP` not `LIMIT`
#' * Automatically prefixes `#` to create temporary tables. Add the prefix
#'   yourself to avoid the message.
#' * String basics: `paste()`, `substr()`, `nchar()`
#' * Custom types for `as.*` functions
#' * Lubridate extraction functions, `year()`, `month()`, `day()` etc
#' * Semi-automated bit <-> boolean translation (see below)
#'
#' Use `simulate_mssql()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @section Bit vs boolean:
#' SQL server uses two incompatible types to represent `TRUE` and `FALSE`
#' values:
#'
#' * The `BOOLEAN` type is the result of logical comparisons (e.g. `x > y`)
#'   and can be used `WHERE` but not to create new columns in `SELECT`.
#'   <https://docs.microsoft.com/en-us/sql/t-sql/language-elements/comparison-operators-transact-sql>
#'
#' * The `BIT` type is a special type of numeric column used to store
#'   `TRUE` and `FALSE` values, but can't be used in `WHERE` clauses.
#'   <https://docs.microsoft.com/en-us/sql/t-sql/data-types/bit-transact-sql?view=sql-server-ver15>
#'
#' dbplyr does its best to automatically create the correct type when needed,
#' but can't do it 100% correctly because it does not have a full type
#' inference system. This means that you many need to manually do conversions
#' from time to time.
#'
#' * To convert from bit to boolean use `x == 1`
#' * To convert from boolean to bit use `as.logical(if(x, 0, 1))`
#'
#' @param version Version of MS SQL to simulate. Currently only, difference is
#'   that 15.0 and above will use `TRY_CAST()` instead of `CAST()`.
#' @name backend-mssql
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_mssql())
#' lf %>% head()
#' lf %>% transmute(x = paste(b, c, d))
#'
#' # Can use boolean as is:
#' lf %>% filter(c > d)
#' # Need to convert from boolean to bit:
#' lf %>% transmute(x = c > d)
#' # Can use boolean as is:
#' lf %>% transmute(x = ifelse(c > d, "c", "d"))
NULL

#' @export
#' @rdname simulate_dbi
simulate_mssql <- function(version = "15.0") {
  simulate_dbi("Microsoft SQL Server",
    version = numeric_version(version)
  )
}

#' @export
`dbplyr_edition.Microsoft SQL Server` <- function(con) {
  2L
}

#' @export
`sql_query_select.Microsoft SQL Server` <- function(con, select, from, where = NULL,
                                             group_by = NULL, having = NULL,
                                             order_by = NULL,
                                             limit = NULL,
                                             distinct = FALSE,
                                             ...,
                                             subquery = FALSE) {
  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct, top = limit),
    from      = sql_clause_from(con, from),
    where     = sql_clause_where(con, where),
    group_by  = sql_clause_group_by(con, group_by),
    having    = sql_clause_having(con, having),
    order_by  = sql_clause_order_by(con, order_by, subquery, limit)
  )
}

#' @export
`sql_translation.Microsoft SQL Server` <- function(con) {
  mssql_scalar <-
    sql_translator(.parent = base_odbc_scalar,

      `!`           = function(x) {
                        if (mssql_needs_bit()) {
                          x <- with_mssql_bool(x)
                          mssql_as_bit(sql_expr(~ !!x))
                        } else {
                          sql_expr(NOT(!!x))
                        }
                      },

      `!=`           = mssql_infix_comparison("!="),
      `==`           = mssql_infix_comparison("="),
      `<`            = mssql_infix_comparison("<"),
      `<=`           = mssql_infix_comparison("<="),
      `>`            = mssql_infix_comparison(">"),
      `>=`           = mssql_infix_comparison(">="),

      `&`            = mssql_infix_boolean("&", "%AND%"),
      `&&`           = mssql_infix_boolean("&", "%AND%"),
      `|`            = mssql_infix_boolean("|", "%OR%"),
      `||`           = mssql_infix_boolean("|", "%OR%"),

      `[` = function(x, i) {
        i <- with_mssql_bool(i)
        build_sql("CASE WHEN (", i, ") THEN (", x, ") END")
      },

      bitwShiftL     = sql_not_supported("bitwShiftL"),
      bitwShiftR     = sql_not_supported("bitwShiftR"),

      `if`           = mssql_sql_if,
      if_else        = function(condition, true, false, missing = NULL) mssql_sql_if(condition, true, false, missing),
      ifelse         = function(test, yes, no) mssql_sql_if(test, yes, no),
      case_when      = mssql_case_when,

      as.logical    = sql_cast("BIT"),

      as.Date       = sql_cast("DATE"),
      as.numeric    = sql_cast("FLOAT"),
      as.double     = sql_cast("FLOAT"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      log           = sql_prefix("LOG"),
      atan2         = sql_prefix("ATN2"),
      ceil          = sql_prefix("CEILING"),
      ceiling       = sql_prefix("CEILING"),

      # https://dba.stackexchange.com/questions/187090
      pmin          = sql_not_supported("pmin()"),
      pmax          = sql_not_supported("pmax()"),

      is.null       = mssql_is_null,
      is.na         = mssql_is_null,

      # string functions ------------------------------------------------
      nchar = sql_prefix("LEN"),
      paste = sql_paste_infix(" ", "+", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "+", function(x) sql_expr(cast(!!x %as% text))),
      substr = sql_substr("SUBSTRING"),
      substring = sql_substr("SUBSTRING"),

      # stringr functions
      str_length = sql_prefix("LEN"),
      str_c = sql_paste_infix("", "+", function(x) sql_expr(cast(!!x %as% text))),
      # no built in function: https://stackoverflow.com/questions/230138
      str_to_title = sql_not_supported("str_to_title()"),
      # https://docs.microsoft.com/en-us/sql/t-sql/functions/substring-transact-sql?view=sql-server-ver15
      str_sub = sql_str_sub("SUBSTRING", "LEN", optional_length = FALSE),

      # lubridate ---------------------------------------------------------------
      # https://en.wikibooks.org/wiki/SQL_Dialects_Reference/Functions_and_expressions/Date_and_time_functions
      as_date = sql_cast("DATE"),

      # Using DATETIME2 as it complies with ANSI and ISO.
      # MS recommends DATETIME2 for new work:
      # https://docs.microsoft.com/en-us/sql/t-sql/data-types/datetime-transact-sql?view=sql-server-2017
      as_datetime = sql_cast("DATETIME2"),

      today = function() sql_expr(CAST(SYSDATETIME() %AS% DATE)),

      # https://docs.microsoft.com/en-us/sql/t-sql/functions/datepart-transact-sql?view=sql-server-2017
      year = function(x) sql_expr(DATEPART(YEAR, !!x)),
      day = function(x) sql_expr(DATEPART(DAY, !!x)),
      mday = function(x) sql_expr(DATEPART(DAY, !!x)),
      yday = function(x) sql_expr(DATEPART(DAYOFYEAR, !!x)),
      hour = function(x) sql_expr(DATEPART(HOUR, !!x)),
      minute = function(x) sql_expr(DATEPART(MINUTE, !!x)),
      second = function(x) sql_expr(DATEPART(SECOND, !!x)),

      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr(DATEPART(MONTH, !!x))
        } else {
          if (!abbr) {
            sql_expr(DATENAME(MONTH, !!x))
          } else {
            stop("`abbr` is not supported in SQL Server translation", call. = FALSE)          }
        }
      },

      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        if (fiscal_start != 1) {
          stop("`fiscal_start` is not supported in SQL Server translation. Must be 1.", call. = FALSE)
        }

        if (with_year) {
          sql_expr((DATENAME(YEAR, !!x) + '.' + DATENAME(QUARTER, !!x)))
        } else {
          sql_expr(DATEPART(QUARTER, !!x))
        }
      },
    )

  if (mssql_version(con) >= "11.0") { # MSSQL 2012
    mssql_scalar <- sql_translator(
      .parent = mssql_scalar,
      as.logical = sql_try_cast("BIT"),
      as.Date = sql_try_cast("DATE"),
      as.POSIXct = sql_try_cast("TIMESTAMP"),
      as.numeric = sql_try_cast("FLOAT"),
      as.double = sql_try_cast("FLOAT"),

      # In SQL server, CAST (even with TRY) of INTEGER and BIGINT appears
      # fill entire columns with NULL if parsing single value fails:
      # https://gist.github.com/DavidPatShuiFong/7b47a9804a497b605e477f1bf6c38b37
      # So we parse to NUMERIC (which doesn't have this problem), then to the
      # target type
      as.integer = function(x) {
        sql_expr(try_cast(try_cast(!!x %as% NUMERIC) %as% INT))
      },
      as.integer64 = function(x) {
        sql_expr(try_cast(try_cast(!!x %as% NUMERIC(38L, 0L)) %as% BIGINT))
      },
      as.character = sql_try_cast("VARCHAR(MAX)"),
      as_date = sql_try_cast("DATE"),
      as_datetime = sql_try_cast("DATETIME2")
    )
  }

  sql_variant(
    mssql_scalar,
    sql_translator(.parent = base_odbc_agg,
      sd            = sql_aggregate("STDEV", "sd"),
      var           = sql_aggregate("VAR", "var"),
                      # MSSQL does not have function for: cor and cov
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()"),
      str_flatten = function(x, collapse = "") sql_expr(string_agg(!!x, !!collapse))

    ),
    sql_translator(.parent = base_odbc_win,
      sd            = win_aggregate("STDEV"),
      var           = win_aggregate("VAR"),
      # MSSQL does not have function for: cor and cov
      cor           = win_absent("cor"),
      cov           = win_absent("cov"),
      str_flatten = function(x, collapse = "") {
        win_over(
          sql_expr(string_agg(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )

  )}

mssql_version <- function(con) {
  if (inherits(con, "TestConnection")) {
    attr(con, "version")
  } else {
    numeric_version(DBI::dbGetInfo(con)$db.version)
  }
}

#' @export
`sql_escape_raw.Microsoft SQL Server` <- function(con, x) {
  # SQL Server binary constants should be prefixed with 0x
  # https://docs.microsoft.com/en-us/sql/t-sql/data-types/constants-transact-sql?view=sql-server-ver15#binary-constants
  paste0(c("0x", format(x)), collapse = "")
}

#' @export
`sql_table_analyze.Microsoft SQL Server` <- function(con, table, ...) {
  # https://docs.microsoft.com/en-us/sql/t-sql/statements/update-statistics-transact-sql
  build_sql("UPDATE STATISTICS ", as.sql(table, con = con), con = con)
}

# SQL server does not use CREATE TEMPORARY TABLE and instead prefixes
# temporary table names with #
# <https://docs.microsoft.com/en-us/previous-versions/sql/sql-server-2008-r2/ms177399%28v%3dsql.105%29#temporary-tables>
#' @export
`db_table_temporary.Microsoft SQL Server` <- function(con, table, temporary) {
  if (temporary && substr(table, 1, 1) != "#") {
    table <- hash_temp(table)
  }

  list(
    table = table,
    temporary = FALSE
  )
}

#' @export
`sql_query_save.Microsoft SQL Server` <- function(con, sql, name,
                                                  temporary = TRUE, ...){

  # https://stackoverflow.com/q/16683758/946850
  build_sql(
    "SELECT * INTO ", as.sql(name, con), " ",
    "FROM (", sql, ") AS temp",
    con = con
  )
}

# Bit vs boolean ----------------------------------------------------------

mssql_needs_bit <- function() {
  context <- sql_current_context()
  identical(context$clause, "SELECT") || identical(context$clause, "ORDER")
}

with_mssql_bool <- function(code) {
  local_context(list(clause = ""))
  code
}

mssql_as_bit <- function(x) {
  if (mssql_needs_bit()) {
    sql_expr(cast(iif(!!x, 1L, 0L) %as% BIT))
  } else {
    x
  }
}

mssql_is_null <- function(x) {
  mssql_as_bit(sql_is_null({{x}}))
}

mssql_infix_comparison <- function(f) {
  assert_that(is_string(f))
  f <- toupper(f)
  function(x, y) {
    mssql_as_bit(build_sql(x, " ", sql(f), " ", y))
  }
}

mssql_infix_boolean <- function(if_bit, if_bool) {
  force(if_bit)
  force(if_bool)

  function(x, y) {
    if (mssql_needs_bit()) {
      x <- with_mssql_bool(x)
      y <- with_mssql_bool(y)
      mssql_as_bit(sql_call2(if_bool, x, y))
    } else {
      sql_call2(if_bool, x, y)
    }
  }
}

mssql_sql_if <- function(cond, if_true, if_false = NULL, missing = NULL) {
  cond <- with_mssql_bool(cond)
  if (is.null(missing)) {
    sql_expr(IIF(!!cond, !!if_true, !!if_false))
  } else {
    sql_if(con, if_true, if_false, missing)
  }
}

mssql_case_when <- function(...) {
  with_mssql_bool(sql_case_when(...))
}

#' @export
`sql_escape_logical.Microsoft SQL Server` <- function(con, x) {
  if (mssql_needs_bit()) {
    y <- ifelse(x, "1", "0")
  } else {
    y <- as.character(x)
  }
  y[is.na(x)] <- "NULL"
  y
}

globalVariables(c("BIT", "CAST", "%AS%", "%is%", "convert", "DATE", "DATENAME", "DATEPART", "IIF", "NOT", "SUBSTRING", "LTRIM", "RTRIM", "CHARINDEX", "SYSDATETIME", "SECOND", "MINUTE", "HOUR", "DAY", "DAYOFWEEK", "DAYOFYEAR", "MONTH", "QUARTER", "YEAR", "BIGINT", "INT"))
