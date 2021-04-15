test_that("dplyr.strict_sql = TRUE prevents auto conversion", {
  old <- options(dplyr.strict_sql = TRUE)
  on.exit(options(old))

  expect_equal(translate_sql(1 + 2), sql("1.0 + 2.0"))
  expect_error(translate_sql(blah(x)), "could not find function")
})

test_that("namespace calls are translated", {
  expect_equal(translate_sql(dplyr::n(), window = FALSE), sql("COUNT(*)"))
  expect_equal(translate_sql(base::ceiling(x)), sql("CEIL(`x`)"))

  expect_snapshot_error(translate_sql(NOSUCHPACKAGE::foo()))
  expect_snapshot_error(translate_sql(dbplyr::NOSUCHFUNCTION()))
  expect_snapshot_error(translate_sql(base::abbreviate(x)))
})

test_that("Wrong number of arguments raises error", {
  expect_error(translate_sql(mean(1, 2, na.rm = TRUE), window = FALSE), "unused argument")
})

test_that("between translated to special form (#503)", {
  out <- translate_sql(between(x, 1, 2))
  expect_equal(out, sql("`x` BETWEEN 1.0 AND 2.0"))
})

test_that("is.na and is.null are equivalent", {
  # Needs to be wrapped in parens to ensure correct precedence
  expect_equal(translate_sql(is.na(x)), sql("((`x`) IS NULL)"))
  expect_equal(translate_sql(is.null(x)), sql("((`x`) IS NULL)"))

  expect_equal(translate_sql(x + is.na(x)), sql("`x` + ((`x`) IS NULL)"))
  expect_equal(translate_sql(!is.na(x)), sql("NOT(((`x`) IS NULL))"))
})

test_that("%in% translation parenthesises when needed", {
  expect_equal(translate_sql(x %in% 1L), sql("`x` IN (1)"))
  expect_equal(translate_sql(x %in% c(1L)), sql("`x` IN (1)"))
  expect_equal(translate_sql(x %in% 1:2), sql("`x` IN (1, 2)"))
  expect_equal(translate_sql(x %in% y), sql("`x` IN `y`"))
})

test_that("%in% strips vector names", {
  expect_equal(translate_sql(x %in% c(a = 1L)), sql("`x` IN (1)"))
  expect_equal(translate_sql(x %in% !!c(a = 1L)), sql("`x` IN (1)"))
})

test_that("%in% with empty vector", {
  expect_equal(translate_sql(x %in% !!integer()), sql("FALSE"))
})

test_that("n_distinct(x) translated to COUNT(distinct, x)", {
  expect_equal(
    translate_sql(n_distinct(x), window = FALSE),
    sql("COUNT(DISTINCT `x`)")
  )
  expect_equal(
    translate_sql(n_distinct(x), window = TRUE),
    sql("COUNT(DISTINCT `x`) OVER ()")
  )
  expect_error(translate_sql(n_distinct(x, y), window = FALSE), "unused argument")
})

test_that("na_if is translated to NULLIF (#211)", {
  expect_equal(translate_sql(na_if(x, 0L)), sql("NULLIF(`x`, 0)"))
})

test_that("connection affects quoting character", {
  lf <- lazy_frame(field1 = 1, con = simulate_sqlite())
  out <- select(lf, field1)
  expect_match(sql_render(out), "^SELECT `field1`\nFROM `df`$")
})

test_that("magrittr pipe is translated", {
  expect_identical(translate_sql(1 %>% is.na()), translate_sql(is.na(1)))
})

# casts -------------------------------------------------------------------

test_that("casts as expected", {
  expect_equal(translate_sql(as.integer64(x)), sql("CAST(`x` AS BIGINT)"))
  expect_equal(translate_sql(as.logical(x)),   sql("CAST(`x` AS BOOLEAN)"))
  expect_equal(translate_sql(as.Date(x)),      sql("CAST(`x` AS DATE)"))
})

# conditionals ------------------------------------------------------------

test_that("all forms of if translated to case statement", {
  expected <- sql("CASE WHEN (`x`) THEN (1) WHEN NOT(`x`) THEN (2) END")

  expect_equal(translate_sql(if (x) 1L else 2L), expected)
  expect_equal(translate_sql(ifelse(x, 1L, 2L)), expected)
  expect_equal(translate_sql(if_else(x, 1L, 2L)), expected)
})

test_that("if translation adds parens", {
  expect_equal(
    translate_sql(if (x) y),
    sql("CASE WHEN (`x`) THEN (`y`) END")
  )
  expect_equal(
    translate_sql(if (x) y else z),
    sql("CASE WHEN (`x`) THEN (`y`) WHEN NOT(`x`) THEN (`z`) END")
  )
})

test_that("if and ifelse use correctly named arguments",{
  exp <- translate_sql(if (x) 1 else 2)

  expect_equal(translate_sql(ifelse(test = x, yes = 1, no = 2)), exp)
  expect_equal(translate_sql(if_else(condition = x, true = 1, false = 2)), exp)

  expect_equal(
    translate_sql(if_else(condition = x, true = 1, false = 2, missing = 3)),
    sql("CASE WHEN (`x`) THEN (1.0) WHEN NOT(`x`) THEN (2.0) WHEN ((`x`) IS NULL) THEN (3.0) END")
  )
})

test_that("switch translated to CASE WHEN", {
  expect_equal(
    translate_sql(switch(x, a = 1L)),
    sql("CASE `x` WHEN ('a') THEN (1) END")
  )
  expect_equal(
    translate_sql(switch(x, a = 1L, 2L)),
    sql("CASE `x` WHEN ('a') THEN (1) ELSE (2) END")
  )
})

# numeric -----------------------------------------------------------------

test_that("hypergeometric functions use manual calculation", {
  expect_equal(translate_sql(cosh(x)), sql("(EXP(`x`) + EXP(-(`x`))) / 2"))
  expect_equal(translate_sql(sinh(x)), sql("(EXP(`x`) - EXP(-(`x`))) / 2"))
  expect_equal(translate_sql(tanh(x)), sql("(EXP(2 * (`x`)) - 1) / (EXP(2 * (`x`)) + 1)"))
  expect_equal(translate_sql(coth(x)), sql("(EXP(2 * (`x`)) + 1) / (EXP(2 * (`x`)) - 1)"))
})


test_that("pmin and max use GREATEST and LEAST", {
  expect_warning(translate_sql(pmin(x, y)), "always removed")
  expect_equal(translate_sql(pmin(x, y, z)), sql("LEAST(`x`, `y`, `z`)"))

  expect_equal(translate_sql(pmax(x, y, na.rm = TRUE)), sql("GREATEST(`x`, `y`)"))
})

test_that("round uses integer digits", {
  expect_equal(translate_sql(round(10.1)), sql("ROUND(10.1, 0)"))
  expect_equal(translate_sql(round(10.1, digits = 1)),  sql("ROUND(10.1, 1)"))
})

# string functions --------------------------------------------------------

test_that("paste() translated to CONCAT_WS", {
  expect_equal(translate_sql(paste0(x, y)),             sql("CONCAT_WS('', `x`, `y`)"))
  expect_equal(translate_sql(paste(x, y)),              sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(translate_sql(paste(x, y, sep = ",")),   sql("CONCAT_WS(',', `x`, `y`)"))
})

# stringr -------------------------------------------

test_that("str_length() translates correctly ", {
  expect_equal(translate_sql(str_length(x)), sql("LENGTH(`x`)"))
})

test_that("lower/upper translates correctly ", {
  expect_equal(translate_sql(str_to_upper(x)), sql("UPPER(`x`)"))
  expect_equal(translate_sql(str_to_lower(x)), sql("LOWER(`x`)"))
})

test_that("str_trim() translates correctly ", {
  expect_equal(
    translate_sql(str_trim(x, "both")),
    sql("LTRIM(RTRIM(`x`))")
  )
})

# subsetting --------------------------------------------------------------

test_that("[ treated as if it is logical subsetting", {
  expect_equal(translate_sql(y[x == 0L]), sql("CASE WHEN (`x` = 0) THEN (`y`) END"))
})

