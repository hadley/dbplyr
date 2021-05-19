#' Build and render SQL from a sequence of lazy operations
#'
#' `sql_build()` creates a `select_query` S3 object, that is rendered
#' to a SQL string by `sql_render()`. The output from `sql_build()` is
#' designed to be easy to test, as it's database agnostic, and has
#' a hierarchical structure. Outside of testing, however, you should
#' always call `sql_render()`.
#'
#' `sql_build()` is generic over the lazy operations, \link{lazy_ops},
#' and generates an S3 object that represents the query. `sql_render()`
#' takes a query object and then calls a function that is generic
#' over the database. For example, `sql_build.op_mutate()` generates
#' a `select_query`, and `sql_render.select_query()` calls
#' `sql_select()`, which has different methods for different databases.
#' The default methods should generate ANSI 92 SQL where possible, so you
#' backends only need to override the methods if the backend is not ANSI
#' compliant.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default `NULL` uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con = NULL, ...) {
  # unique_subquery_name_reset()
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, con = op$src$con, ...) {
  # only used for testing
  qry <- sql_build(op$ops, con = con, ...)
  sql_optimise(qry, con = con, ...)
}

#' @export
sql_build.ident <- function(op, con = NULL, ...) {
  op
}


# Render ------------------------------------------------------------------

#' @export
#' @rdname sql_build
#' @param subquery Is this SQL going to be used in a subquery?
#'   This is important because you can place a bare table name in a subquery
#'   and  ORDER BY does not work in subqueries.
sql_render <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  UseMethod("sql_render")
}

#' @export
sql_render.tbl_lazy <- function(query, con = query$src$con, ..., subquery = FALSE, query_list = NULL) {
  sql_render(query$ops, con = con, ..., subquery = subquery, query_list = query_list)
}

cte_render <- function(query_list, con) {
  query_list <- purrr::keep(query_list, "render")

  n <- length(query_list)
  if (n == 1) {
    return(query_list[[1]]$query)
  }

  cte_queries <- purrr::map_chr(query_list[-n], "query")
  cte_names <- purrr::map_chr(query_list[-n], "name")
  ctes <- purrr::map2(cte_names, cte_queries,
    function(name, query) {
      build_sql(ident(name), " AS (\n", sql(query), "\n)", con = con)
    }
  )
  cte_query <- sql_vector(ctes, parens = FALSE, collapse = ",\n", con = con)

  build_sql("WITH ", cte_query, "\n", query_list[[n]]$query, con = con)
}

cte_wrap <- function(query_list, sql, render = TRUE) {
  if (is.list(query_list)) {
    id <- vctrs::vec_match(as.character(sql), purrr::map_chr(query_list, "query"))

    if (!is.na(id)) {
      query_entry <- query_list[[id]]
      query_entry$render <- FALSE
    } else {
      if (is.ident(sql)) {
        name <- sql
      } else {
        name <- unique_subquery_name()
      }
      query_entry <- list(query = sql, name = name, render = render)
    }

    append(query_list, list(query_entry))
  } else {
    sql
  }
}

query_list_from <- function(query_list, con, name = NULL) {
  if (is.list(query_list)) {
    last_query <- dplyr::last(query_list)
    purrr::set_names(maybe_ident(last_query$name), name)
  } else {
    dbplyr_sql_subquery(con, query_list, name = name)
  }
}

maybe_ident <- function(x) {
  if (is.ident(x)) {
    x
  } else {
    ident(x)
  }
}


#' @export
sql_render.op <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  qry <- sql_build(query, con = con, ...)
  qry <- sql_optimise(qry, con = con, ..., subquery = subquery)
  sql_render(qry, con = con, ..., subquery = subquery, query_list = query_list)
}

#' @export
sql_render.sql <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  cte_wrap(query_list, query, render = TRUE)
}

#' @export
sql_render.ident <- function(query, con = NULL, ..., subquery = FALSE, query_list = NULL) {
  if (subquery) {
    cte_wrap(query_list, query, render = FALSE)
  } else {
    query <- dbplyr_query_select(con, sql("*"), query)
    cte_wrap(query_list, query, render = TRUE)
  }
}

# Optimise ----------------------------------------------------------------

#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ..., subquery = FALSE) {
  UseMethod("sql_optimise")
}

#' @export
sql_optimise.sql <- function(x, con = NULL, ...) {
  # Can't optimise raw SQL
  x
}

#' @export
sql_optimise.ident <- function(x, con = NULL, ...) {
  x
}
