#' Join SQL tables
#'
#' @description
#' These are methods for the dplyr [join] generics. They are translated
#' to the following SQL queries:
#'
#' * `inner_join(x, y)`: `SELECT * FROM x JOIN y ON x.a = y.a`
#' * `left_join(x, y)`:  `SELECT * FROM x LEFT JOIN y ON x.a = y.a`
#' * `right_join(x, y)`: `SELECT * FROM x RIGHT JOIN y ON x.a = y.a`
#' * `full_join(x, y)`:  `SELECT * FROM x FULL JOIN y ON x.a = y.a`
#' * `semi_join(x, y)`:  `SELECT * FROM x WHERE EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`
#' * `anti_join(x, y)`:  `SELECT * FROM x WHERE NOT EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`
#'
#' @param x,y A pair of lazy data frames backed by database queries.
#' @inheritParams dplyr::join
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into a
#'   temporary table in same database as `x`. `*_join()` will automatically
#'   run `ANALYZE` on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'
#'   This allows you to join tables across srcs, but it's potentially expensive
#'   operation so you must opt into it.
#' @param auto_index if `copy` is `TRUE`, automatically create
#'   indices for the variables in `by`. This may speed up the join if
#'   there are matching indexes in `x`.
#' @param sql_on A custom join predicate as an SQL expression.
#'   Usually joins use column equality, but you can perform more complex
#'   queries by supply `sql_on` which should be a SQL expression that
#'   uses `LHS` and `RHS` aliases to refer to the left-hand side or
#'   right-hand side of the join respectively.
#' @param na_matches Should NA (NULL) values match one another?
#'   The default, "never", is how databases usually work. `"na"` makes
#'   the joins behave like the dplyr join functions, [merge()], [match()],
#'   and `%in%`.
#' @param lhs_as,rhs_as Alias to use for `x` resp. `y`. Defaults to `"LHS"` resp.
#'   `"RHS"`
#' @inherit arrange.tbl_lazy return
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' band_db <- tbl_memdb(dplyr::band_members)
#' instrument_db <- tbl_memdb(dplyr::band_instruments)
#' band_db %>% left_join(instrument_db) %>% show_query()
#'
#' # Can join with local data frames by setting copy = TRUE
#' band_db %>%
#'   left_join(dplyr::band_instruments, copy = TRUE)
#'
#' # Unlike R, joins in SQL don't usually match NAs (NULLs)
#' db <- memdb_frame(x = c(1, 2, NA))
#' label <- memdb_frame(x = c(1, NA), label = c("one", "missing"))
#' db %>% left_join(label, by = "x")
#' # But you can activate R's usual behaviour with the na_matches argument
#' db %>% left_join(label, by = "x", na_matches = "na")
#'
#' # By default, joins are equijoins, but you can use `sql_on` to
#' # express richer relationships
#' db1 <- memdb_frame(x = 1:5)
#' db2 <- memdb_frame(x = 1:3, y = letters[1:3])
#' db1 %>% left_join(db2) %>% show_query()
#' db1 %>% left_join(db2, sql_on = "LHS.x < RHS.x") %>% show_query()
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr inner_join
inner_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = NULL,
                                auto_index = FALSE, ...,
                                sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_join(
    x, y,
    "inner",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr left_join
left_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = NULL,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_join(
    x, y,
    "left",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr right_join
right_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = NULL,
                                auto_index = FALSE, ...,
                                sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_join(
    x, y,
    "right",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr full_join
full_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = NULL,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_join(
    x, y,
    "full",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr semi_join
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_semi_join(
    x, y,
    anti = FALSE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr anti_join
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               lhs_as = "LHS", rhs_as = "RHS") {

  add_op_semi_join(
    x, y,
    anti = TRUE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as,
    ...
  )
}


add_op_join <- function(x, y, type, by = NULL, sql_on = NULL, copy = FALSE,
                        suffix = NULL,
                        auto_index = FALSE,
                        na_matches = "never",
                        lhs_as = "LHS",
                        rhs_as = "RHS") {
  vctrs::vec_assert(lhs_as, character(), size = 1)
  vctrs::vec_assert(rhs_as, character(), size = 1)

  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else if (identical(type, "full") && identical(by, character())) {
    type <- "cross"
    by <- list(x = character(0), y = character(0))
  } else {
    by <- dplyr::common_by(by, x, y)
  }

  y <- auto_copy(
    x, y,
    copy = copy,
    indexes = if (auto_index) list(by$y)
  )

  suffix <- suffix %||% sql_join_suffix(x$src$con, suffix)
  vars <- join_vars(op_vars(x), op_vars(y), type = type, by = by, suffix = suffix)

  x$ops <- op_double("join", x, y, args = list(
    vars = vars,
    type = type,
    by = by,
    suffix = suffix,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as
  ))
  x
}

add_op_semi_join <- function(x, y, anti = FALSE, by = NULL, sql_on = NULL, copy = FALSE,
                             auto_index = FALSE, na_matches = "never",
                             lhs_as = "LHS", rhs_as = "RHS") {
  vctrs::vec_assert(lhs_as, character(), size = 1)
  vctrs::vec_assert(rhs_as, character(), size = 1)

  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else {
    by <- dplyr::common_by(by, x, y)
  }

  y <- auto_copy(
    x, y, copy,
    indexes = if (auto_index) list(by$y)
  )

  x$ops <- op_double("semi_join", x, y, args = list(
    anti = anti,
    by = by,
    na_matches = na_matches,
    lhs_as = lhs_as,
    rhs_as = rhs_as
  ))
  x
}

join_vars <- function(x_names, y_names, type, by, suffix = c(".x", ".y")) {
  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
  suffix <- check_suffix(suffix)
  x_new <- add_suffixes(x_names, y_names, suffix$x)
  y_new <- add_suffixes(y_names, x_names, suffix$y)

  # In left and inner joins, return key values only from x
  # In right joins, return key values only from y
  # In full joins, return key values by coalescing values from x and y
  x_x <- x_names
  x_y <- by$y[match(x_names, by$x)]
  x_y[type == "left" | type == "inner"] <- NA
  x_x[type == "right" & !is.na(x_y)] <- NA
  y_x <- rep_len(NA, length(y_names))
  y_y <- y_names

  # Return a list with 3 parallel vectors
  # At each position, values in the 3 vectors represent
  #  alias - name of column in join result
  #  x - name of column from left table or NA if only from right table
  #  y - name of column from right table or NA if only from left table
  list(
    alias = c(x_new, y_new),
    x = c(x_x, y_x),
    y = c(x_y, y_y),
    all_x = x_names,
    all_y = c(y_names, by$y)
  )
}

check_suffix <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    stop("`suffix` must be a character vector of length 2.", call. = FALSE)
  }

  list(x = x[1], y = x[2])
}

add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  out <- character(length(x))
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out) {
      nm <- paste0(nm, suffix)
    }

    out[[i]] <- nm
  }
  out
}


#' @export
op_vars.op_join <- function(op) {
  op$args$vars$alias
}
#' @export
op_vars.op_semi_join <- function(op) {
  op_vars(op$x)
}

#' @export
sql_build.op_join <- function(op, con, ...) {
  join_query(
    op$x, op$y,
    vars = op$args$vars,
    type = op$args$type,
    by = op$args$by,
    suffix = op$args$suffix,
    na_matches = op$args$na_matches,
    lhs_as = op$args$lhs_as,
    rhs_as = op$args$rhs_as
  )
}

#' @export
sql_build.op_semi_join <- function(op, con, ...) {
  semi_join_query(
    op$x, op$y,
    anti = op$args$anti,
    by = op$args$by,
    na_matches = op$args$na_matches,
    lhs_as = op$args$lhs_as,
    rhs_as = op$args$rhs_as
  )
}
