wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(
    x,
    indent = indent,
    exdent = indent + 2,
    width = getOption("width")
  )

  paste0(wrapped, collapse = "\n")
}

indent <- function(x) {
  x <- paste0(x, collapse = "\n")
  paste0("  ", gsub("\n", "\n  ", x))
}

indent_print <- function(x) {
  indent(utils::capture.output(print(x)))
}

# function for the thousand separator,
# returns "," unless it's used for the decimal point, in which case returns "."
'big_mark' <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}
