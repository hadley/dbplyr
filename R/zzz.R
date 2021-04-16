# nocov start
.onLoad <- function(...) {
  register_s3_method("dplyr", "union", "tbl_lazy")
  register_s3_method("dplyr", "intersect", "tbl_lazy")
  register_s3_method("dplyr", "setdiff", "tbl_lazy")
  register_s3_method("dplyr", "setdiff", "tbl_Oracle")
  register_s3_method("dplyr", "setdiff", "OraConnection")
  register_s3_method("dplyr", "filter", "tbl_lazy")

  register_s3_method("tidyr", "pivot_wider", "tbl_lazy")
  register_s3_method("tidyr", "pivot_longer", "tbl_lazy")
  register_s3_method("tidyr", "fill", "tbl_lazy")
  register_s3_method("tidyr", "complete", "tbl_lazy")
  register_s3_method("tidyr", "expand", "tbl_lazy")
  register_s3_method("tidyr", "replace_na", "tbl_lazy")

  if (utils::packageVersion("dplyr") >= "0.8.99") {
    register_s3_method("dplyr", "group_by_drop_default", "tbl_lazy")

    methods::setOldClass(c("ident_q", "ident", "character"), ident_q())
    methods::setOldClass(c("ident", "character"), ident())
    methods::setOldClass(c("sql", "character"), sql())
  }

  base_scalar$`%>%` <- magrittr::`%>%`

  op <- options()
  op.dbplyr <- list(
    dbplyr_indent_strategy = "indent"
  )
  toset <- !(names(op.dbplyr) %in% names(op))
  if (any(toset)) options(op.dbplyr[toset])
}

# Silence R CMD check note:
# ** checking whether the namespace can be loaded with stated dependencies ... NOTE
# Warning in .undefineMethod("initialize", Class, classWhere) :
#   no generic function 'initialize' found
#
# I'm not sure why this is necessary, but I suspect it's due to the use of
# setOldClass onLoad
#' @importFrom methods initialize
NULL

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
