#' Compute results of a query
#'
#' These are methods for the dplyr generics [collapse()], [compute()],
#' and [collect()]. `collapse()` creates a subquery, `compute()` stores
#' the results in a remote table, and `collect()` executes the query and
#' downloads the data into R.
#'
#' @export
#' @param x A lazy data frame backed by a database query.
#' @importFrom dplyr collapse
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db %>% filter(a <= 2) %>% collect()
collapse.tbl_sql <- function(x, ...) {
  sql <- db_sql_render(x$src$con, x)

  tbl(x$src, sql) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

# compute -----------------------------------------------------------------

#' @rdname collapse.tbl_sql
#' @param name Table name in remote database.
#' @param temporary Should the table be temporary (`TRUE`, the default`) or
#'   persistent (`FALSE`)?
#' @inheritParams copy_to.src_sql
#' @export
#' @importFrom dplyr compute
compute.tbl_sql <- function(x,
                            name = unique_table_name(),
                            temporary = TRUE,
                            unique_indexes = list(),
                            indexes = list(),
                            analyze = TRUE,
                            ...) {

  vars <- op_vars(x)
  assert_that(all(unlist(indexes) %in% vars))
  assert_that(all(unlist(unique_indexes) %in% vars))

  x_aliased <- select(x, !!! syms(vars)) # avoids problems with SQLite quoting (#1754)
  sql <- db_sql_render(x$src$con, x_aliased$ops)

  name <- db_compute(x$src$con, name, sql,
    temporary = temporary,
    unique_indexes = unique_indexes,
    indexes = indexes,
    analyze = analyze,
    ...
  )

  tbl(x$src, name) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

# collect -----------------------------------------------------------------

#' @rdname collapse.tbl_sql
#' @param n Number of rows to fetch. Defaults to `Inf`, meaning all rows.
#' @param warn_incomplete Warn if `n` is less than the number of result rows?
#' @importFrom dplyr collect
#' @export
collect.tbl_sql <- function(x, ..., n = Inf, warn_incomplete = TRUE) {
  if (identical(n, Inf)) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  sql <- db_sql_render(x$src$con, x, query_list = list())
  sql <- cte_render(sql, x$src$con)
  out <- db_collect(x$src$con, sql, n = n, warn_incomplete = warn_incomplete)
  dplyr::grouped_df(out, intersect(op_grps(x), names(out)))
}
