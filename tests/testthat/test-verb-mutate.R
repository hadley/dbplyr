test_that("mutate computed before summarise", {
  mf <- memdb_frame(x = c(1, 2, 3), y = c(9, 8, 7))

  out <- mutate(mf, z = x + y) %>%
    summarise(sum_z = sum(z, na.rm = TRUE)) %>%
    collect()

  expect_equal(out$sum_z, 30)
})

test_that("two mutates equivalent to one", {
  mf <- memdb_frame(x = c(1, 5, 9), y = c(3, 12, 11))

  df1 <- mf %>% mutate(x2 = x * 2, y4 = y * 4) %>% collect()
  df2 <- mf %>% collect() %>% mutate(x2 = x * 2, y4 = y * 4)
  expect_equal_tbl(df1, df2)
})

test_that("can refer to fresly created values", {
  out1 <- memdb_frame(x1 = 1) %>%
    mutate(x2 = x1 + 1, x3 = x2 + 1, x4 = x3 + 1) %>%
    collect()
  expect_equal(out1, tibble(x1 = 1, x2 = 2, x3 = 3, x4 = 4))

  out2 <- memdb_frame(x = 1, .name = "multi_mutate") %>%
    mutate(x = x + 1, x = x + 2, x = x + 4)
  expect_equal(collect(out2), tibble(x = 8))
  expect_snapshot(show_query(out2))
})

test_that("transmute includes all needed variables", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- transmute(lf, x = x / 2, x2 = x + y)
  expect_named(out$ops$x$args$vars, c("x", "y"))
  expect_snapshot(out)
})

test_that("queries are not nested unnecessarily", {
  # Should only be one query deep
  sql <- memdb_frame(x = 1) %>%
    mutate(y = x + 1, a = y + 1, b = y + 1) %>%
    sql_build()

  expect_s3_class(sql$from, "select_query")
  expect_s3_class(sql$from$from, "ident")
})

test_that("maintains order of existing columns (#3216, #3223)", {
  lazy <- lazy_frame(x = 1, y = 2) %>%
    mutate(z = 3, y = 4, y = 5)

  expect_equal(op_vars(lazy), c("x", "y", "z"))
})

test_that("supports overwriting variables (#3222)", {
  df <- memdb_frame(x = 1, y = 2) %>%
    mutate(y = 4, y = 5) %>%
    collect()
  expect_equal(df, tibble(x = 1, y = 5))

  df <- memdb_frame(x = 1, y = 2) %>%
    mutate(y = 4, y = y + 1) %>%
    collect()
  expect_equal(df, tibble(x = 1, y = 5))

  df <- memdb_frame(x = 1, y = 2) %>%
    mutate(y = 4, y = x + 4) %>%
    collect()
  expect_equal(df, tibble(x = 1, y = 5))
})

# SQL generation -----------------------------------------------------------

test_that("mutate generates new variables and replaces existing", {
  df1 <- memdb_frame(x = 1)
  out <- df1 %>% mutate(x = 2, y = 3) %>% collect()
  expect_equal(out, tibble(x = 2, y = 3))
})

test_that("transmute only returns new variables", {
  df1 <- memdb_frame(x = 1)
  out <- df1 %>% transmute(y = 3) %>% collect()
  expect_equal(out, tibble(y = 3))
})

test_that("mutate calls windowed versions of sql functions", {
  df1 <- memdb_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- df1 %>% group_by(g) %>% mutate(r = row_number(x)) %>% collect()
  expect_equal(out$r, c(1, 2, 1, 2))
})

test_that("recycled aggregates generate window function", {
  df1 <- memdb_frame(x = as.numeric(1:4), g = rep(c(1, 2), each = 2))
  out <- df1 %>%
    group_by(g) %>%
    mutate(r = x - mean(x, na.rm = TRUE)) %>%
    collect()

  expect_equal(out$r, c(-0.5, 0.5, -0.5, 0.5))
})

test_that("cumulative aggregates generate window function", {
  df1 <- memdb_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- df1 %>%
    group_by(g) %>%
    arrange(x) %>%
    mutate(r = cumsum(x)) %>%
    collect()

  expect_equal(out$r, c(1, 3, 3, 7))
})

test_that("mutate overwrites previous variables", {
  df <- memdb_frame(x = 1:5) %>%
    mutate(x = x + 1) %>%
    mutate(x = x + 1) %>%
    collect()

  expect_equal(names(df), "x")
  expect_equal(df$x, 1:5 + 2)
})

test_that("sequence of operations work", {
  out <- memdb_frame(x = c(1, 2, 3, 4)) %>%
    select(y = x) %>%
    mutate(z = 2 * y) %>%
    filter(z == 2) %>%
    collect()

  expect_equal(out, tibble(y = 1, z = 2))
})


# sql_render --------------------------------------------------------------

test_that("quoting for rendering mutated grouped table", {
  out <- memdb_frame(x = 1, y = 2) %>% mutate(y = x)
  expect_match(out %>% sql_render, "^SELECT\n  `x`,\n  `x` AS `y`\nFROM `[^`]*`$")
  expect_equal(out %>% collect, tibble(x = 1, y = 1))
})

test_that("mutate generates subqueries as needed", {
  lf <- lazy_frame(x = 1, con = simulate_sqlite())

  expect_snapshot(lf %>% mutate(x = x + 1, x = x + 1))
  expect_snapshot(lf %>% mutate(x1 = x + 1, x2 = x1 + 1))
})

test_that("mutate collapses over nested select", {
  lf <- lazy_frame(g = 0, x = 1, y = 2)

  expect_snapshot(lf %>% select(x:y) %>% mutate(x = x * 2, y = y * 2))
  expect_snapshot(lf %>% select(y:x) %>% mutate(x = x * 2, y = y * 2))
})

# sql_build ---------------------------------------------------------------

test_that("mutate generates simple expressions", {
  out <- lazy_frame(x = 1) %>%
    mutate(y = x + 1L) %>%
    sql_build()

  expect_equal(out$select, sql(x = '`x`', y = '`x` + 1'))
})

test_that("mutate can drop variables with NULL", {
  out <- lazy_frame(x = 1, y = 1) %>%
    mutate(y = NULL) %>%
    sql_build()

  expect_named(out$select, "x")
})

test_that("mutate_all generates correct sql", {
  out <- lazy_frame(x = 1, y = 1) %>%
    dplyr::mutate_all(~ . + 1L) %>%
    sql_build()

  expect_equal(out$select, sql(x = '`x` + 1', y = '`y` + 1'))

  out <- lazy_frame(x = 1) %>%
    dplyr::mutate_all(list(one = ~ . + 1L, two = ~ . + 2L)) %>%
    sql_build()
  expect_equal(out$select, sql(`x` = '`x`', one = '`x` + 1', two = '`x` + 2'))
})

test_that("mutate_all scopes nested quosures correctly", {
  num <- 10L
  out <- lazy_frame(x = 1, y = 1) %>%
    dplyr::mutate_all(~ . + num) %>%
    sql_build()

  expect_equal(out$select, sql(x = '`x` + 10', y = '`y` + 10'))
})


# ops ---------------------------------------------------------------------

test_that("mutate adds new", {
  out <- tibble(x = 1) %>% tbl_lazy() %>% mutate(y = x + 1, z = y + 1)
  expect_equal(op_vars(out), c("x", "y", "z"))
})

test_that("mutated vars are always named", {
  mf <- dbplyr::memdb_frame(a = 1)

  out2 <- mf %>% mutate(1) %>% op_vars()
  expect_equal(out2, c("a", "1"))
})

