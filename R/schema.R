#' Refer to a table in a schema
#'
#' @param schema,table Names of schema and table. These will be automatically
#'   quoted; use `sql()` to pass a raw name that won't get quoted.
#' @export
#' @examples
#' in_schema("my_schema", "my_table")
#' # eliminate quotes
#' in_schema(sql("my_schema"), sql("my_table"))
#'
#' # Example using schemas with SQLite
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Add auxilary schema
#' tmp <- tempfile()
#' DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))
#'
#' library(dplyr, warn.conflicts = FALSE)
#' copy_to(con, iris, "df", temporary = FALSE)
#' copy_to(con, mtcars, in_schema("aux", "df"), temporary = FALSE)
#'
#' con %>% tbl("df")
#' con %>% tbl(in_schema("aux", "df"))
in_schema <- function(schema, table) {
  structure(
    list(schema = as.sql(schema), table = as.sql(table)),
    class = "dbplyr_schema"
  )
}

#' @export
print.dbplyr_schema <- function(x, ...) {
  cat_line("<SCHEMA> ", escape_ansi(x$schema), ".", escape_ansi(x$table))
}

#' @export
as.sql.dbplyr_schema <- function(x, con) {
  ident_q(paste0(escape(x$schema, con = con), ".", escape(x$table, con = con)))
}

is.schema <- function(x) inherits(x, "dbplyr_schema")

# Support for DBI::Id() ---------------------------------------------------

#' @export
as.sql.Id <- function(x, con) ident_q(dbQuoteIdentifier(con, x))

# Old dbplyr approach -----------------------------------------------------

#' Declare a identifer as being pre-quoted.
#'
#' No longer needed; please use [sql()] instead.
#'
#' @keywords internal
#' @export
ident_q <- function(...) {
  x <- c_character(...)
  structure(x, class = c("ident_q", "ident", "character"))
}

#' @export
escape.ident_q <- function(x, parens = FALSE, collapse = ", ", con = NULL, align_as = FALSE) {
  sql_vector(names_to_as(x, names2(x), con = con, align_as = align_as), parens, collapse, con = con)
}

#' @export
dbi_quote.ident_q <- function(x, con) DBI::SQL(as.character(x))
