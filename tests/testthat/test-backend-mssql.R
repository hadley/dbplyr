# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(as.logical(x)),   sql("TRY_CAST(`x` AS BIT)"))
  expect_equal(translate_sql(as.numeric(x)),   sql("TRY_CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.integer(x)),   sql("TRY_CAST(TRY_CAST(`x` AS NUMERIC) AS INT)"))
  expect_equal(translate_sql(as.integer64(x)), sql("TRY_CAST(TRY_CAST(`x` AS NUMERIC(38, 0)) AS BIGINT)"))
  expect_equal(translate_sql(as.double(x)),    sql("TRY_CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.character(x)), sql("TRY_CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(translate_sql(log(x)),          sql("LOG(`x`)"))
  expect_equal(translate_sql(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(translate_sql(atan2(x)),        sql("ATN2(`x`)"))
  expect_equal(translate_sql(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(translate_sql(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(translate_sql(substr(x, 1, 2)), sql("SUBSTRING(`x`, 1, 2)"))
  expect_equal(translate_sql(trimws(x)),       sql("LTRIM(RTRIM(`x`))"))
  expect_equal(translate_sql(paste(x, y)),     sql("`x` + ' ' + `y`"))

  expect_error(translate_sql(bitwShiftL(x, 2L)), sql("not available"))
  expect_error(translate_sql(bitwShiftR(x, 2L)), sql("not available"))
})

test_that("contents of [ have bool context", {
  local_con(simulate_mssql())
  local_context(list(clause = "SELECT"))

  expect_equal(translate_sql(x[x > y]), sql("CASE WHEN (`x` > `y`) THEN (`x`) END"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(str_length(x)),     sql("LEN(`x`)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = FALSE),  sql("STDEV(`x`)"))
  expect_equal(translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR(`x`)"))

  expect_error(translate_sql(cor(x), window = FALSE), "not available")
  expect_error(translate_sql(cov(x), window = FALSE), "not available")

  expect_equal(translate_sql(str_flatten(x), window = FALSE), sql("STRING_AGG(`x`, '')"))
})

test_that("custom window functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE)),  sql("STDEV(`x`) OVER ()"))
  expect_equal(translate_sql(var(x, na.rm = TRUE)), sql("VAR(`x`) OVER ()"))

  expect_error(translate_sql(cor(x)), "not supported")
  expect_error(translate_sql(cov(x)), "not supported")

  expect_equal(translate_sql(str_flatten(x)), sql("STRING_AGG(`x`, '') OVER ()"))
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_mssql())
  expect_equal(translate_sql(as_date(x)),     sql("TRY_CAST(`x` AS DATE)"))
  expect_equal(translate_sql(as_datetime(x)), sql("TRY_CAST(`x` AS DATETIME2)"))
  expect_equal(translate_sql(today()),   sql("CAST(SYSDATETIME() AS DATE)"))
  expect_equal(translate_sql(year(x)),   sql("DATEPART(YEAR, `x`)"))
  expect_equal(translate_sql(day(x)),    sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(mday(x)),   sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(yday(x)),   sql("DATEPART(DAYOFYEAR, `x`)"))
  expect_equal(translate_sql(hour(x)),   sql("DATEPART(HOUR, `x`)"))
  expect_equal(translate_sql(minute(x)), sql("DATEPART(MINUTE, `x`)"))
  expect_equal(translate_sql(second(x)), sql("DATEPART(SECOND, `x`)"))
  expect_equal(translate_sql(month(x)), sql("DATEPART(MONTH, `x`)"))
  expect_equal(translate_sql(month(x, label = TRUE, abbr = FALSE)), sql("DATENAME(MONTH, `x`)"))
  expect_error(translate_sql(month(x, abbr = TRUE, abbr = TRUE)))

  expect_equal(translate_sql(quarter(x)), sql("DATEPART(QUARTER, `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(DATENAME(YEAR, `x`) + '.' + DATENAME(QUARTER, `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 5)))
})

# verb translation --------------------------------------------------------

test_that("convert between bit and boolean as needed", {
  mf <- lazy_frame(x = 1, con = simulate_mssql())

  # No conversion
  expect_snapshot(mf %>% filter(is.na(x)))
  expect_snapshot(mf %>% filter(!is.na(x)))
  expect_snapshot(mf %>% filter(x == 1L || x == 2L))
  expect_snapshot(mf %>% mutate(z = ifelse(x == 1L, 1L, 2L)))
  expect_snapshot(mf %>% mutate(z = case_when(x == 1L ~ 1L)))

  # Single conversion on outer layer
  expect_snapshot(mf %>% mutate(z = !is.na(x)))
  expect_snapshot(mf %>% mutate(x = x == 1L))
  expect_snapshot(mf %>% mutate(x = x == 1L || x == 2L))
  expect_snapshot(mf %>% mutate(x = x == 1L || x == 2L || x == 3L))
  expect_snapshot(mf %>% mutate(x = !(x == 1L || x == 2L || x == 3L)))
})

test_that("handles ORDER BY in subqueries", {
  expect_snapshot(
    sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = "z", subquery = TRUE)
  )
})

test_that("custom limit translation", {
  expect_snapshot(
    sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = ident("z"), limit = 10)
  )
})

test_that("custom escapes translated correctly", {
  mf <- lazy_frame(x = "abc", con = simulate_mssql())

  a <- blob::as_blob("abc")
  b <- blob::as_blob(as.raw(c(0x01, 0x02)))
  L <- c(a, b)

  expect_snapshot(mf %>% filter(x == a))
  expect_snapshot(mf %>% filter(x %in% L))

  # expect_snapshot() also uses !!
  qry <- mf %>% filter(x %in% !!L)
  expect_snapshot(qry)
})

test_that("logical escaping depends on context", {
  mf <- lazy_frame(x = "abc", con = simulate_mssql())
  expect_snapshot(mf %>% filter(x == TRUE))
  expect_snapshot(mf %>% mutate(x = TRUE))
})

test_that("generates custom sql", {
  con <- simulate_mssql()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))

  # Creates the same SQL since there's no temporary CLAUSE
  # Automatic renaming is handled upstream by db_collect()/db_copy_to()
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl")))
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"), temporary = FALSE))
})

# Live database -----------------------------------------------------------

test_that("can copy_to() and compute() with temporary tables (#272)", {
  con <- src_test("mssql")
  df <- tibble(x = 1:3)
  expect_message(
    db <- copy_to(con, df, name = "temp", temporary = TRUE),
    "Created a temporary table",
  )
  expect_equal(db %>% pull(), 1:3)

  db2 <- expect_message(
    db %>% mutate(y = x + 1) %>% compute(),
    "Created a temporary table"
  )
  expect_equal(db2 %>% pull(), 2:4)
})

test_that("bit conversion works for important cases", {
  df <- tibble(x = 1:3, y = 3:1)
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())
  expect_equal(db %>% mutate(z = x == y) %>% pull(), c(FALSE, TRUE, FALSE))
  expect_equal(db %>% filter(x == y) %>% pull(), 2)

  df <- tibble(x = c(TRUE, FALSE, FALSE), y = c(TRUE, FALSE, TRUE))
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())
  expect_equal(db %>% filter(x == 1) %>% pull(), TRUE)
  expect_equal(db %>% mutate(z = TRUE) %>% pull(), c(1, 1, 1))

  # Currently not supported: would need to determine that we have a bit
  # vector in a boolean context, and convert to boolean with x == 1.
  # expect_equal(db %>% mutate(z = x) %>% pull(), c(TRUE, FALSE, FALSE))
  # expect_equal(db %>% mutate(z = !x) %>% pull(), c(FALSE, TRUE, TRUE))
  # expect_equal(db %>% mutate(z = x & y) %>% pull(), c(TRUE, FALSE, FALSE))

})

test_that("as.integer and as.integer64 translations if parsing failures", {
  df <- data.frame(x = c("1.3", "2x"))
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())

  out <- db %>%
    mutate(
      integer = as.integer(x),
      integer64 = as.integer64(x),
      numeric = as.numeric(x),
    ) %>%
    collect()

  expect_identical(out$integer, c(1L, NA))
  expect_identical(out$integer64, bit64::as.integer64(c(1L, NA)))
  expect_identical(out$numeric, c(1.3, NA))
})
