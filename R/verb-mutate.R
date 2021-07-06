#' Create, modify, and delete columns
#'
#' These are methods for the dplyr [mutate()] and [transmute()] generics.
#' They are translated to computed expressions in the `SELECT` clause of
#' the SQL query.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::mutate
#' @inherit arrange.tbl_lazy return
#' @export
#' @importFrom dplyr mutate
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1:5, y = 5:1)
#' db %>%
#'   mutate(a = (x + y) / 2, b = sqrt(x^2L + y^2L)) %>%
#'   show_query()
#'
#' # dbplyr automatically creates subqueries as needed
#' db %>%
#'   mutate(x1 = x + 1, x2 = x1 * 2) %>%
#'   show_query()
mutate.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  nest_vars(.data, dots, union(op_vars(.data), op_grps(.data)))
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  missing_group_vars <- syms(setdiff(group_vars(.data), names(dots)))
  dots <- c(set_names(missing_group_vars), dots)

  nest_vars(.data, dots, character())
}

# helpers -----------------------------------------------------------------

# TODO: refactor to remove `.data` argument and return a list of layers.
nest_vars <- function(.data, dots, all_vars) {
  # For each expression, check if it uses any newly created variables.
  # If so, nest the mutate()
  new_vars <- character()
  init <- 0L
  for (i in seq_along(dots)) {
    cur_var <- names(dots)[[i]]
    used_vars <- all_names(get_expr(dots[[i]]))

    if (any(used_vars %in% new_vars)) {
      new_actions <- dots[seq2(init, length(dots))][new_vars]
      .data$ops <- op_select(.data$ops, carry_over(union(all_vars, used_vars), new_actions))
      all_vars <- c(all_vars, setdiff(new_vars, all_vars))
      new_vars <- cur_var
      init <- i
    } else {
      new_vars <- c(new_vars, cur_var)
    }
  }

  if (init != 0L) {
    dots <- dots[-seq2(1L, init - 1)]
  }
  .data$ops <- op_select(.data$ops, carry_over(all_vars, dots))
  .data
}

# Combine a selection (passed through from subquery)
# with new actions
carry_over <- function(sel = character(), act = list()) {
  if (is.null(names(sel))) {
    names(sel) <- sel
  }
  sel <- syms(sel)

  # Keep last of duplicated acts
  act <- act[!duplicated(names(act), fromLast = TRUE)]

  # Preserve order of sel
  both <- intersect(names(sel), names(act))
  sel[both] <- act[both]

  # Adding new variables at end
  new <- setdiff(names(act), names(sel))

  c(sel, act[new])
}

