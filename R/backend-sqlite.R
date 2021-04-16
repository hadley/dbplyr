#' Backend: SQLite
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Uses non-standard `LOG()` function
#' * Date-time extraction functions from lubridate
#' * Custom median translation
#' * Right and full joins are simulated using left joins
#'
#' Use `simulate_sqlite()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-sqlite
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_sqlite())
#' lf %>% transmute(x = paste(c, " times"))
#' lf %>% transmute(x = log(b), y = log(b, base = 2))
NULL

#' @export
#' @rdname backend-sqlite
simulate_sqlite <- function() simulate_dbi("SQLiteConnection")

#' @export
dbplyr_edition.SQLiteConnection <- function(con) {
  2L
}

#' @export
db_connection_describe.SQLiteConnection <- function(con) {
  paste0("sqlite ", sqlite_version(), " [", con@dbname, "]")
}

#' @export
sql_query_explain.SQLiteConnection <- function(con, sql, ...) {
  build_sql("EXPLAIN QUERY PLAN ", sql, con = con)
}

#' @export
sql_query_set_op.SQLiteConnection <- function(con, x, y, method, ..., all = FALSE, lvl = 0) {
  # SQLite does not allow parentheses
  method <- paste0(method, if (all) " ALL")
  build_sql(
    x,
    "\n", indent_lvl(sql_kw(method), lvl = lvl), "\n",
    y,
    con = con
  )
}

sqlite_version <- function() {
  numeric_version(RSQLite::rsqliteVersion()[[2]])
}

# SQL methods -------------------------------------------------------------

#' @export
sql_translation.SQLiteConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      as.numeric = sql_cast("REAL"),
      as.double = sql_cast("REAL"),
      log = function(x, base = exp(1)) {
        if (base != exp(1)) {
          sql_expr(log(!!x) / log(!!base))
        } else {
          sql_expr(log(!!x))
        }
      },
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),
      # https://www.sqlite.org/lang_corefunc.html#maxoreunc
      pmin = sql_aggregate_n("MIN", "pmin"),
      pmax = sql_aggregate_n("MAX", "pmax"),

      # lubridate,
      today = function() {
        date <- function(x) {} # suppress R CMD check note
        sql_expr(date("now"))
      },
      now = function() sql_expr(datetime("now")),
      # https://modern-sql.com/feature/extract#proprietary-strftime
      year = function(x) sql_expr(cast(strftime("%Y", !!x) %as% NUMERIC)),
      month = function(x) sql_expr(cast(strftime("%m", !!x) %as% NUMERIC)),
      mday = function(x) sql_expr(cast(strftime("%d", !!x) %as% NUMERIC)),
      day = function(x) sql_expr(cast(strftime("%d", !!x) %as% NUMERIC)),
      hour = function(x) sql_expr(cast(strftime("%H", !!x) %as% NUMERIC)),
      minute = function(x) sql_expr(cast(strftime("%M", !!x) %as% NUMERIC)),
      second = function(x) sql_expr(cast(strftime("%f", !!x) %as% REAL)),
      yday = function(x) sql_expr(cast(strftime("%j", !!x) %as% NUMERIC)),

    ),
    sql_translator(.parent = base_agg,
      sd = sql_aggregate("STDEV", "sd"),
      median = sql_aggregate("MEDIAN"),
    ),
    if (sqlite_version() >= "3.25") {
      sql_translator(.parent = base_win,
        sd = win_aggregate("STDEV"),
        median = win_absent("median")
      )
    } else {
      base_no_win
    }
  )
}

#' @export
sql_escape_logical.SQLiteConnection <- function(con, x){
  y <- as.character(as.integer(x))
  y[is.na(x)] <- "NULL"
  y
}

#' @export
sql_query_wrap.SQLiteConnection <- function(con, from, name = unique_subquery_name(), ..., lvl = 0) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {

    if (is.null(name)) {
      build_sql(ident_subquery(from, con, lvl), con = con)
    } else {
      build_sql(ident_subquery(from, con, lvl), " AS ", ident(name), con = con)
    }
  }
}

#' @export
sql_expr_matches.SQLiteConnection <- function(con, x, y) {
  # https://sqlite.org/lang_expr.html#isisnot
  build_sql(x, " IS ", y, con = con)
}

#' @export
sql_query_join.SQLiteConnection <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ..., lvl = 0) {
  # workaround as SQLite doesn't support FULL OUTER JOIN and RIGHT JOIN
  # see: https://www.sqlite.org/omitted.html

  # Careful: in the right join resp. the second join in the full join do not
  # name `y` LHS and `x` RHS as it messes up the select query!

  if (type == "full") {
    join_sql <- build_sql(
      sql_query_join(con, x, y, vars, type = "left", by = by, na_matches = na_matches, ..., lvl = lvl + 1),
      indent_lvl(sql_kw("UNION"), lvl = lvl + 1), "\n",
      sql_query_join(con, y, x, vars, type = "left", by = by, na_matches = na_matches, ..., lvl = lvl + 1),
      con = con
    )

    sql_query_select(
      con,
      select = ident(vars$alias),
      # TODO this should probably be `dbplyr_sql_subquery`
      from = sql_subquery(con, join_sql, lvl = lvl),
      subquery = TRUE,
      lvl = lvl
    )
  } else if (type == "right") {
    # TODO remove superfluous line break
    sql_query_join(con, y, x, vars, type = "left", by = by, na_matches = na_matches, ..., lvl = lvl)
  } else {
    NextMethod()
  }
}

globalVariables(c("datetime", "NUMERIC", "REAL"))
