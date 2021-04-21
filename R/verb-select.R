#' Subset, rename, and reorder columns using their names
#'
#' @description
#' These are methods for the dplyr [select()], [rename()], and [relocate()]
#' generics. They generate the `SELECT` clause of the SQL query.
#'
#' These functions do not support predicate functions, i.e. you can
#' not use `where(is.numeric)` to select all numeric variables.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::select
#' @export
#' @importFrom dplyr select
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1, y = 2, z = 3)
#' db %>% select(-y) %>% show_query()
#' db %>% relocate(z) %>% show_query()
#' db %>% rename(first = x, last = z) %>% show_query()
select.tbl_lazy <- function(.data, ...) {
  sim_data <- simulate_vars(.data)
  sim_data <- group_by(sim_data, !!!syms(group_vars(.data)))
  loc <- tidyselect::eval_select(expr(c(...)), sim_data)
  loc <- ensure_group_vars(loc, sim_data, notify = TRUE)
  new_vars <- set_names(names(sim_data)[loc], names(loc))

  .data$ops <- op_select(.data$ops, syms(new_vars))
  .data
}

ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), colnames(data))
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
    if (notify) {
      inform(glue(
        "Adding missing grouping variables: ",
        paste0("`", names(data)[missing], "`", collapse = ", ")
      ))
    }
    loc <- c(set_names(missing, vars), loc)
  }

  loc
}


#' @rdname select.tbl_lazy
#' @importFrom dplyr rename
#' @export
rename.tbl_lazy <- function(.data, ...) {
  sim_data <- simulate_vars(.data)
  loc <- tidyselect::eval_rename(expr(c(...)), sim_data)

  new_vars <- set_names(names(sim_data), names(sim_data))
  names(new_vars)[loc] <- names(loc)

  .data$ops <- op_select(.data$ops, syms(new_vars))
  .data
}

#' @rdname select.tbl_lazy
#' @importFrom dplyr rename_with
#' @importFrom tidyselect everything
#' @inheritParams dplyr::rename_with
#' @export
rename_with.tbl_lazy <- function(.data, .fn, .cols = everything(), ...) {
  .fn <- as_function(.fn)
  cols <- tidyselect::eval_select(enquo(.cols), simulate_vars(.data))

  new_vars <- set_names(op_vars(.data))
  names(new_vars)[cols] <- .fn(new_vars[cols], ...)

  .data$ops <- op_select(.data$ops, syms(new_vars))
  .data
}

#' @rdname select.tbl_lazy
#' @importFrom dplyr relocate
#' @inheritParams dplyr::relocate
#' @export
relocate.tbl_lazy <- function(.data, ..., .before = NULL, .after = NULL) {
  vars <- simulate_vars(.data)
  new_vars <- dplyr::relocate(
    simulate_vars(.data),
    ...,
    .before = {{.before}},
    .after = {{.after}}
  )
  .data$ops <- op_select(.data$ops, syms(set_names(names(new_vars))))
  .data
}

simulate_vars <- function(x) {
  as_tibble(rep_named(op_vars(x), list(logical())))
}

# op_select ---------------------------------------------------------------

op_select <- function(x, vars) {
  if (inherits(x, "op_select")) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in nest_vars()
    prev_vars <- x$args$vars

    if (purrr::every(vars, is.symbol)) {
      # if current operation is pure projection
      # we can just subset the previous selection
      sel_vars <- purrr::map_chr(vars, as_string)
      vars <- set_names(prev_vars[sel_vars], names(sel_vars))
      x <- x$x
    } else if (purrr::every(prev_vars, is.symbol)) {
      # if previous operation is pure projection
      sel_vars <- purrr::map_chr(prev_vars, as_string)
      if (all(names(sel_vars) == sel_vars)) {
        # and there's no renaming
        # we can just ignore the previous step
        x <- x$x
      }
    }
  }

  new_op_select(x, vars)
}

# SELECT in the SQL sense - powers select(), rename(), mutate(), and transmute()
new_op_select <- function(x, vars) {
  stopifnot(inherits(x, "op"))
  stopifnot(is.list(vars))

  op_single("select", x, dots = list(), args = list(vars = vars))
}

#' @export
op_vars.op_select <- function(op) {
  names(op$args$vars)
}

#' @export
op_grps.op_select <- function(op) {
  # Find renamed variables
  symbols <- purrr::keep(op$args$vars, is_symbol)
  new2old <- purrr::map_chr(symbols, as_string)
  old2new <- set_names(names(new2old), new2old)

  grps <- op_grps(op$x)
  renamed <- grps %in% names(old2new)
  grps[renamed] <- old2new[grps[renamed]]
  grps
}

#' @export
sql_build.op_select <- function(op, con, ...) {
  # translate once just to register windows
  win_register_activate()
  on.exit(win_reset())
  on.exit(win_register_deactivate(), add = TRUE)

  new_vars <- translate_sql_(
    op$args$vars, con,
    vars_group = op_grps(op),
    vars_order = translate_sql_(op_sort(op), con, context = list(clause = "ORDER")),
    vars_frame = op_frame(op),
    context = list(clause = "SELECT")
  )

  named_windows <- win_register_names()
  if (nrow(named_windows) > 0) {
    # need to translate again and use registered windows names
    win_register_deactivate()
    new_vars <- translate_sql_(
      op$args$vars, con,
      vars_group = op_grps(op),
      vars_order = translate_sql_(op_sort(op), con, context = list(clause = "ORDER")),
      vars_frame = op_frame(op),
      context = list(clause = "SELECT")
    )

    # build window sql
    names_esc <- sql_escape_ident(con, named_windows$name)
    window_sql <- sql(paste0(names_esc, " AS ", named_windows$key))
  } else {
    window_sql <- character()
  }

  select_query(
    sql_build(op$x, con),
    select = new_vars,
    window = window_sql
  )
}
