test_that("custom scalar functions translated correctly", {
  local_con(simulate_oracle())

  expect_equal(translate_sql(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(translate_sql(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(translate_sql(as.double(x)),    sql("CAST(`x` AS NUMBER)"))
})

test_that("paste and paste0 translate correctly", {
  local_con(simulate_oracle())

  expect_equal(translate_sql(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(translate_sql(paste0(x, y)), sql("`x` || `y`"))
})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_oracle())
  expect_snapshot(mf %>% head())
})

test_that("generates custom sql", {
  con <- simulate_oracle()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM foo")))

  lf <- lazy_frame(x = 1, con = con)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl")))
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"), temporary = FALSE))
})
