test_that("expand completes all values", {
  expect_equal(
    memdb_frame(x = 1:2, y = 1:2) %>% tidyr::expand(x, y) %>% collect(),
    tibble(x = c(1, 1, 2, 2), y = c(1, 2, 1, 2))
  )

  expect_snapshot(lazy_frame(x = 1, y = 1) %>% tidyr::expand(x, y))
})

test_that("nesting doesn't expand values", {
  df <- tibble(x = 1:2, y = 1:2)
  expect_equal(
    tidyr::expand(memdb_frame(!!!df), nesting(x, y)) %>%
      collect(),
    df
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% tidyr::expand(nesting(x, y)))
})

test_that("expand accepts expressions", {
  df <- lazy_frame(x = 1)
  expect_snapshot(tidyr::expand(df, round(x / 2)))
  expect_snapshot(tidyr::expand(df, nesting(x_half = round(x / 2), x1 = x + 1)))
})

test_that("expand respects groups", {
  df <- tibble(
    a = c(1L, 1L, 2L),
    b = c(1L, 2L, 1L),
    c = c("b", "a", "a")
  )
  expect_equal(
    memdb_frame(!!!df) %>% group_by(a) %>% tidyr::expand(b, c) %>% collect(),
    tibble(
      a = c(1, 1, 1, 1, 2),
      b = c(1, 1, 2, 2, 1),
      c = c("b", "a", "b", "a", "a")
    ) %>%
      group_by(a)
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% group_by(a) %>% tidyr::expand(b, c))
})

test_that("NULL inputs", {
  expect_snapshot(tidyr::expand(lazy_frame(x = 1), x, y = NULL))
})

test_that("expand() errors when expected", {
  expect_snapshot_error(tidyr::expand(memdb_frame(x = 1)))
  expect_snapshot_error(tidyr::expand(memdb_frame(x = 1), x = NULL))
})

test_that("nesting() respects .name_repair", {
  expect_snapshot_error(
    tidyr::expand(
      memdb_frame(x = 1, y = 1),
      nesting(x, x = x + 1)
    )
  )

  vars <- suppressMessages(
    tidyr::expand(
      memdb_frame(x = 1, y = 1),
      nesting(x, x = x + 1, .name_repair = "unique")
    ) %>%
      op_vars()
  )

  expect_equal(vars, c("x...1", "x...2"))
})

test_that("expand respect .name_repair", {
  vars <- suppressMessages(
    memdb_frame(x = integer(), z = integer()) %>%
      tidyr::expand(x, z = x, nesting(x), nesting(z), .name_repair = "unique") %>%
      op_vars()
  )

  expect_equal(vars, c("x...1", "z...2", "x...3", "z...4"))
})

# replace_na --------------------------------------------------------------

test_that("replace_na replaces missing values", {
  expect_equal(
    memdb_frame(x = c(1, NA), y = c(NA, "b")) %>%
      tidyr::replace_na(list(x = 0, y = "unknown")) %>%
      collect(),
    tibble(
      x = c(1, 0),
      y = c("unknown", "b")
    )
  )

  expect_snapshot(
    lazy_frame(x = 1, y = "a") %>% tidyr::replace_na(list(x = 0, y = "unknown"))
  )
})

test_that("replace_na ignores missing columns", {
  expect_snapshot(lazy_frame(x = 1) %>% tidyr::replace_na(list(not_there = 0)))
})

# complete ----------------------------------------------------------------

test_that("complete completes missing combinations", {
  df <- tibble(
    x = 1:2,
    y = 1:2,
    z = c("a", "b")
  )

  expect_equal(
    memdb_frame(!!!df) %>% tidyr::complete(x, y, fill = list(z = "c")) %>% collect(),
    tibble(
      x = c(1, 1, 2, 2),
      y = c(1, 2, 1, 2),
      z = c("a", "c", "c", "b")
    )
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% tidyr::complete(x, y, fill = list(z = "c")))
})
