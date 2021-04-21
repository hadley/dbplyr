#' Generate SQL expression for window functions
#'
#' `win_over()` makes it easy to generate the window function specification.
#' `win_absent()`, `win_rank()`, `win_aggregate()`, and `win_cumulative()`
#' provide helpers for constructing common types of window functions.
#' `win_current_group()` and `win_current_order()` allow you to access
#' the grouping and order context set up by [group_by()] and [arrange()].
#'
#' @param expr The window expression
#' @param parition Variables to partition over
#' @param order Variables to order by
#' @param frame A numeric vector of length two defining the frame.
#' @param f The name of an sql function as a string
#' @export
#' @keywords internal
#' @examples
#' con <- simulate_dbi()
#'
#' win_over(sql("avg(x)"), con = con)
#' win_over(sql("avg(x)"), "y", con = con)
#' win_over(sql("avg(x)"), order = "y", con = con)
#' win_over(sql("avg(x)"), order = c("x", "y"), con = con)
#' win_over(sql("avg(x)"), frame = c(-Inf, 0), order = "y", con = con)
win_over <- function(expr, partition = NULL, order = NULL, frame = NULL, con = sql_current_con()) {
  if (length(partition) > 0) {
    partition <- as.sql(partition, con = con)

    partition <- build_sql(
      "PARTITION BY ",
      sql_vector(
        escape(partition, con = con),
        collapse = ", ",
        parens = FALSE,
        con = con
      ),
      con = con
    )
  }

  if (length(order) > 0) {
    order <- as.sql(order, con = con)

    order <- build_sql(
      "ORDER BY ",
      sql_vector(
        escape(order, con = con),
        collapse = ", ",
        parens = FALSE,
        con = con
      ),
      con = con
    )
  }
  if (length(frame) > 0) {
    if (length(order) == 0) {
      warning(
        "Windowed expression '", expr, "' does not have explicit order.\n",
        "Please use arrange() or window_order() to make determinstic.",
        call. = FALSE
      )
    }

    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    frame <- build_sql("ROWS ", frame, con = con)
  }

  over <- sql_vector(purrr::compact(list(partition, order, frame)), parens = TRUE, con = con)

  if (sql_context$register_windows) {
    win_register(over)
  } else {
    over <- win_get(over, con)
  }
  sql <- build_sql(expr, " OVER ", over, con = con)

  sql
}

win_register_activate <- function() {
  sql_context$register_windows <- TRUE
}

win_register_deactivate <- function() {
  sql_context$register_windows <- FALSE
}

win_register <- function(over) {
  sql_context$windows <- append(sql_context$windows, over)
}

win_register_names <- function() {
  windows <- sql_context$windows %||% character()

  window_count <- vctrs::vec_count(windows)
  window_count <- vctrs::vec_slice(window_count, window_count$count > 1)
  if (nrow(window_count) > 0) {
    window_count$name <- ident(paste0("win", seq_along(window_count$key)))
  } else {
    window_count$name <- ident()
  }
  window_count$key <- window_count$key

  sql_context$window_names <- window_count
  window_count
}

win_get <- function(over, con) {
  windows <- sql_context$window_names

  if (vctrs::vec_in(over, windows$key)) {
    id <- vctrs::vec_match(over, windows$key)
    ident(windows$name[[id]])
  } else {
    over
  }
}

win_reset <- function() {
  sql_context$window_names <- NULL
  sql_context$windows <- list()
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) stop("from must be less than to", call. = FALSE)

  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) return("CURRENT ROW")
    paste(val(x), dir(x))
  }

  if (to == 0) {
    sql(bound(from))
  } else {
    sql(paste0("BETWEEN ", bound(from), " AND ", bound(to)))
  }
}


#' @rdname win_over
#' @export
win_rank <- function(f) {
  force(f)
  function(order = NULL) {
    win_over(
      build_sql(sql(f), list()),
      partition = win_current_group(),
      order = order %||% win_current_order(),
      frame = win_current_frame()
    )
  }
}

#' @rdname win_over
#' @export
win_aggregate <- function(f) {
  force(f)
  warned <- FALSE
  function(x, na.rm = FALSE) {
    warned <<- check_na_rm(f, na.rm, warned)
    frame <- win_current_frame()

    win_over(
      build_sql(sql(f), list(x)),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname win_over
#' @export
win_aggregate_2 <- function(f) {

  function(x, y) {
    frame <- win_current_frame()

    win_over(
      build_sql(sql(f), list(x, y)),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname win_over
#' @usage NULL
#' @export
win_recycled <- win_aggregate


#' @rdname win_over
#' @export
win_cumulative <- function(f) {
  force(f)
  function(x, order = NULL) {
    win_over(
      build_sql(sql(f), list(x)),
      partition = win_current_group(),
      order = order %||% win_current_order(),
      frame = c(-Inf, 0)
    )
  }
}

#' @rdname win_over
#' @export
win_absent <- function(f) {
  force(f)

  function(...) {
    stop(
      "Window function `", f, "()` is not supported by this database",
      call. = FALSE
    )
  }
}


# API to set default partitioning etc -------------------------------------

# Use a global variable to communicate state of partitioning between
# tbl and sql translator. This isn't the most amazing design, but it keeps
# things loosely coupled and is easy to understand.
sql_context <- new.env(parent = emptyenv())
sql_context$group_by <- NULL
sql_context$order_by <- NULL
sql_context$con <- NULL
# Used to carry additional information needed for special cases
sql_context$context <- list()

sql_context$register_windows <- FALSE
sql_context$windows <- NULL
sql_context$window_names <- NULL


set_current_con <- function(con) {
  old <- sql_context$con
  sql_context$con <- con
  invisible(old)
}

local_con <- function(con, env = parent.frame()) {
  old <- set_current_con(con)
  withr::defer(set_current_con(old), envir = env)
  invisible()
}

set_win_current_group <- function(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- sql_context$group_by
  sql_context$group_by <- vars
  invisible(old)
}

set_win_current_order <- function(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- sql_context$order_by
  sql_context$order_by <- vars
  invisible(old)
}

set_win_current_frame <- function(frame) {
  stopifnot(is.null(frame) || is.numeric(frame))

  old <- sql_context$frame
  sql_context$frame <- frame
  invisible(old)
}
#' @export
#' @rdname win_over
win_current_group <- function() sql_context$group_by

#' @export
#' @rdname win_over
win_current_order <- function() sql_context$order_by

#' @export
#' @rdname win_over
win_current_frame <- function() sql_context$frame

# Not exported, because you shouldn't need it
sql_current_con <- function() {
  sql_context$con
}

# Functions to manage information for special cases
set_current_context <- function(context) {
  old <- sql_context$context
  sql_context$context <- context
  invisible(old)
}

sql_current_context <- function() sql_context$context

local_context <- function(x, env = parent.frame()) {
  old <- set_current_context(x)
  withr::defer(set_current_context(old), envir = env)
  invisible()
}

# Where translation -------------------------------------------------------

uses_window_fun <- function(x, con) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    calls <- unlist(lapply(x, all_calls))
  } else {
    calls <- all_calls(x)
  }

  win_f <- ls(envir = dbplyr_sql_translation(con)$window)
  any(calls %in% win_f)
}

common_window_funs <- function() {
  ls(dbplyr_sql_translation(NULL)$window)
}

#' @noRd
#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
translate_window_where <- function(expr, window_funs = common_window_funs()) {
  switch(typeof(expr),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    string = ,
    symbol = window_where(expr, list()),
    language = {
      if (is_formula(expr)) {
        translate_window_where(f_rhs(expr), window_funs)
      } else if (is_call(expr, name = window_funs)) {
        name <- unique_subquery_name()
        window_where(sym(name), set_names(list(expr), name))
      } else {
        args <- lapply(expr[-1], translate_window_where, window_funs = window_funs)
        expr <- call2(node_car(expr), splice(lapply(args, "[[", "expr")))

        window_where(
          expr = expr,
          comp = unlist(lapply(args, "[[", "comp"), recursive = FALSE)
        )

      }
    },
    abort(glue("Unknown type: ", typeof(expr)))
  )
}


#' @noRd
#' @examples
#' translate_window_where_all(list(quote(x == 1), quote(n() > 2)))
#' translate_window_where_all(list(quote(cumsum(x) == 10), quote(n() > 2)))
translate_window_where_all <- function(x, window_funs = common_window_funs()) {
  out <- lapply(x, translate_window_where, window_funs = window_funs)

  list(
    expr = unlist(lapply(out, "[[", "expr"), recursive = FALSE),
    comp = unlist(lapply(out, "[[", "comp"), recursive = FALSE)
  )
}

window_where <- function(expr, comp) {
  stopifnot(is.call(expr) || is.name(expr) || is.atomic(expr))
  stopifnot(is.list(comp))

  list(
    expr = expr,
    comp = comp
  )
}
