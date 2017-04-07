simulate_test <- function() {
  structure(list(), class = c("DBITestConnection", "DBIConnection"))
}

db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
}

sql_escape_ident.DBITestConnection <- function(con, x) {
  sql_quote(x, "`")
}

# DBI connections --------------------------------------------------------------

simulate_dbi <- function() {
  structure(
    list(),
    class = "DBIConnection"
  )
}

simulate_sqlite <- function() {
  structure(
    list(),
    class = c("SQLiteConnection", "DBIConnection")
  )
}

simulate_postgres <- function() {
  structure(
    list(),
    class = c("PostgreSQLConnection", "DBIConnection")
  )
}

# odbc connections --------------------------------------------------------

setClass(
  "OdbcConnection",
  contains = "DBIConnection",
  slots = list(
    ptr = "externalptr",
    quote = "character",
    info = "ANY"
  )
)

class_cache <- new.env(parent = emptyenv())

simulate_odbc <- function(dbms.name = NULL) {
  class <- getClassDef(dbms.name, where = class_cache, inherits = FALSE)
  if (is.null(class)) {
    setClass(dbms.name,
             contains = "OdbcConnection", where = class_cache)
  }
  new(dbms.name, quote = "\"")
}
