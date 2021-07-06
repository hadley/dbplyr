#' Backend: PostgreSQL
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Many stringr functions
#' * lubridate date-time extraction functions
#' * More standard statistical summaries
#'
#' Use `simulate_postgres()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-postgres
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_postgres())
#' lf %>% summarise(x = sd(b, na.rm = TRUE))
#' lf %>% summarise(y = cor(b, c), y = cov(b, c))
NULL

#' @export
#' @rdname backend-postgres
simulate_postgres <- function() simulate_dbi("PqConnection")

#' @export
dbplyr_edition.PostgreSQL <- function(con) {
  2L
}
#' @export
dbplyr_edition.PqConnection <- dbplyr_edition.PostgreSQL

#' @export
db_connection_describe.PqConnection <- function(con) {
  info <- dbGetInfo(con)
  host <- if (info$host == "") "localhost" else info$host

  paste0("postgres ", info$serverVersion, " [", info$username, "@",
    host, ":", info$port, "/", info$dbname, "]")
}
#' @export
db_connection_describe.PostgreSQL <- db_connection_describe.PqConnection

postgres_grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  # https://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-TABLE
  if (any(c(perl, fixed, useBytes))) {
    abort("`perl`, `fixed` and `useBytes` parameters are unsupported")
  }

  if (ignore.case) {
    sql_expr(((!!x)) %~*% ((!!pattern)))
  } else {
    sql_expr(((!!x)) %~% ((!!pattern)))
  }
}
postgres_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_expr(round(((!!x)) %::% numeric, !!digits))
}

#' @export
sql_translation.PqConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      bitwXor = sql_infix("#"),
      log10  = function(x) sql_expr(log(!!x)),
      log    = sql_log(),
      cot    = sql_cot(),
      round  = postgres_round,
      grepl  = postgres_grepl,

      paste  = sql_paste(" "),
      paste0 = sql_paste(""),

      # stringr functions
      # https://www.postgresql.org/docs/9.1/functions-string.html
      # https://www.postgresql.org/docs/9.1/functions-matching.html#FUNCTIONS-POSIX-REGEXP
      str_c = sql_paste(""),

      str_locate  = function(string, pattern) {
        sql_expr(strpos(!!string, !!pattern))
      },
      str_detect = function(string, pattern, negate = FALSE) {
        if (isTRUE(negate)) {
          sql_expr(!(!!string ~ !!pattern))
        } else {
          sql_expr(!!string ~ !!pattern)
        }
      },
      str_replace = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      },
      str_replace_all = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement, 'g'))
      },
      str_squish = function(string){
        sql_expr(ltrim(rtrim(regexp_replace(!!string, '\\s+', ' ', 'g'))))
      },
      str_remove = function(string, pattern){
        sql_expr(regexp_replace(!!string, !!pattern, ''))
      },
      str_remove_all = function(string, pattern){
        sql_expr(regexp_replace(!!string, !!pattern, '', 'g'))
      },

      # lubridate functions
      week = function(x) {
        sql_expr(EXTRACT(WEEK %FROM% !!x))
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr(EXTRACT(MONTH %FROM% !!x))
        } else {
          if (abbr) {
            sql_expr(TO_CHAR(!!x, "Mon"))
          } else {
            sql_expr(TO_CHAR(!!x, "Month"))
          }
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        if (fiscal_start != 1) {
          stop("`fiscal_start` is not supported in PostgreSQL translation. Must be 1.", call. = FALSE)
        }

        if (with_year) {
          sql_expr((EXTRACT(YEAR %FROM% !!x) || '.' || EXTRACT(QUARTER %FROM% !!x)))
        } else {
          sql_expr(EXTRACT(QUARTER %FROM% !!x))
        }
      },
      wday = function(x, label = FALSE, abbr = TRUE, week_start = NULL) {
        if (!label) {
          week_start <- week_start %||% getOption("lubridate.week.start", 7)
          offset <- as.integer(7 - week_start)
          sql_expr(EXTRACT("dow" %FROM% DATE(!!x) + !!offset) + 1)
        } else if (label && !abbr) {
          sql_expr(TO_CHAR(!!x, "Day"))
        } else if (label && abbr) {
          sql_expr(SUBSTR(TO_CHAR(!!x, "Day"), 1, 3))
        } else {
          stop("Unrecognized arguments to `wday`", call. = FALSE)
        }
      },
      yday = function(x) sql_expr(EXTRACT(DOY %FROM% !!x)),

      # https://www.postgresql.org/docs/13/datatype-datetime.html#DATATYPE-INTERVAL-INPUT
      seconds = function(x) {
        interval <- paste(x, "seconds")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      minutes = function(x) {
        interval <- paste(x, "minutes")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      hours = function(x) {
        interval <- paste(x, "hours")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      days = function(x) {
        interval <- paste(x, "days")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      weeks = function(x) {
        interval <- paste(x, "weeks")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      months = function(x) {
        interval <- paste(x, "months")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      years = function(x) {
        interval <- paste(x, "years")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },

      # https://www.postgresql.org/docs/current/functions-datetime.html#FUNCTIONS-DATETIME-TRUNC
      floor_date = function(x, unit = "seconds") {
        unit <- arg_match(unit,
          c("second", "minute", "hour", "day", "week", "month", "quarter", "year")
        )
        sql_expr(DATE_TRUNC(!!unit, !!x))
      },
    ),
    sql_translator(.parent = base_agg,
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      sd = sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      all = sql_aggregate("BOOL_AND", "all"),
      any = sql_aggregate("BOOL_OR", "any"),
      str_flatten = function(x, collapse) sql_expr(string_agg(!!x, !!collapse))
    ),
    sql_translator(.parent = base_win,
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      sd =  win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      all = win_aggregate("BOOL_AND"),
      any = win_aggregate("BOOL_OR"),
      str_flatten = function(x, collapse) {
        win_over(
          sql_expr(string_agg(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )
  )
}
#' @export
sql_translation.PostgreSQL <- sql_translation.PqConnection

#' @export
sql_expr_matches.PqConnection <- function(con, x, y) {
  # https://www.postgresql.org/docs/current/functions-comparison.html
  build_sql(x, " IS NOT DISTINCT FROM ", y, con = con)
}
#' @export
sql_expr_matches.PostgreSQL <- sql_expr_matches.PqConnection

# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @export
sql_query_explain.PqConnection <- function(con, sql, format = "text", ...) {
  format <- match.arg(format, c("text", "json", "yaml", "xml"))

  build_sql(
    "EXPLAIN ",
    if (!is.null(format)) sql(paste0("(FORMAT ", format, ") ")),
    sql,
    con = con
  )
}
#' @export
sql_query_explain.PostgreSQL <- sql_query_explain.PqConnection

globalVariables(c("strpos", "%::%", "%FROM%", "DATE", "EXTRACT", "TO_CHAR", "string_agg", "%~*%", "%~%", "MONTH", "DOY", "DATE_TRUNC", "INTERVAL"))
