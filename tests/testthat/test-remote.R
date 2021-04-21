test_that("remote_name returns null for computed tables", {
  mf <- memdb_frame(x = 5, .name = "refxiudlph")
  expect_equal(remote_name(mf), ident("refxiudlph"))

  mf2 <- mf %>% filter(x == 3)
  expect_equal(remote_name(mf2), NULL)
})

test_that("remote_name ignores the last group_by() operation(s)", {
  mf <- memdb_frame(x = 5, .name = "asdf")
  for (i in seq(4)) {
    mf <- mf %>% dplyr::group_by(x)
    expect_equal(remote_name(mf), ident("asdf"))
  }
})

test_that("remote_name ignores the last ungroup() operation(s)", {
  mf <- memdb_frame(x = 5, .name = "ghjkl") %>% dplyr::group_by(x)
  for (i in seq(4)) {
    mf <- mf %>% dplyr::ungroup()
    expect_equal(remote_name(mf), ident("ghjkl"))
  }
})

test_that("result from dplyr::compute() has remote name", {
  mf <- memdb_frame(x = 5, .name = "zxcvb")
  mf <- mf %>% dplyr::mutate(y = 5) %>% dplyr::compute()
  expect_false(is.null(remote_name(mf)))
})

test_that("can retrieve query, src and con metadata", {
  mf <- memdb_frame(x = 5)

  expect_s4_class(remote_con(mf), "DBIConnection")
  expect_s3_class(remote_src(mf), "src_sql")
  expect_s3_class(remote_query(mf), "sql")
  expect_type(remote_query_plan(mf), "character")
})

