# convert between bit and boolean as needed

    Code
      mf %>% filter(is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (((`x`) IS NULL))

---

    Code
      mf %>% filter(!is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (NOT(((`x`) IS NULL)))

---

    Code
      mf %>% filter(x == 1L || x == 2L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 1 OR `x` = 2)

---

    Code
      mf %>% mutate(z = ifelse(x == 1L, 1L, 2L))
    Output
      <SQL>
      SELECT
        `x`,
        IIF(`x` = 1, 1, 2) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = case_when(x == 1L ~ 1L))
    Output
      <SQL>
      SELECT
        `x`,
        CASE
      WHEN (`x` = 1) THEN (1)
      END AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = !is.na(x))
    Output
      <SQL>
      SELECT
        `x`,
        CAST(IIF(~((`x`) IS NULL), 1, 0) AS BIT) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1 OR `x` = 2, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L || x == 3L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1 OR `x` = 2 OR `x` = 3, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x == 1L || x == 2L || x == 3L))
    Output
      <SQL>
      SELECT CAST(IIF(~(`x` = 1 OR `x` = 2 OR `x` = 3), 1, 0) AS BIT) AS `x`
      FROM `df`

# handles ORDER BY in subqueries

    Code
      sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = "z",
      subquery = TRUE)
    Warning <warning>
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      <SQL> SELECT `x`
      FROM `y`

# custom limit translation

    Code
      sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = ident("z"),
      limit = 10)
    Output
      <SQL> SELECT TOP 10 `x`
      FROM `y`
      ORDER BY `z`

# custom escapes translated correctly

    Code
      mf %>% filter(x == a)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 0x616263)

---

    Code
      mf %>% filter(x %in% L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

---

    Code
      qry
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

# logical escaping depends on context

    Code
      mf %>% filter(x == TRUE)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = TRUE)

---

    Code
      mf %>% mutate(x = TRUE)
    Output
      <SQL>
      SELECT 1 AS `x`
      FROM `df`

# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> UPDATE STATISTICS `schema`.`tbl`

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"))
    Output
      <SQL> SELECT * INTO `schema`.`tbl` FROM (SELECT * FROM foo) AS temp

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"),
      temporary = FALSE)
    Output
      <SQL> SELECT * INTO `schema`.`tbl` FROM (SELECT * FROM foo) AS temp

