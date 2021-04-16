# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl`

---

    Code
      sql_query_explain(con, sql("SELECT * FROM table"))
    Output
      <SQL> EXPLAIN SELECT * FROM table

---

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (`LHS`.`x` <=> `RHS`.`x`)

# can explain

    Code
      db %>% mutate(y = x + 1) %>% explain()
    Output
      <SQL>
      SELECT `x`, `x` + 1.0 AS `y`
      FROM `test`
      
      <PLAN>
        id select_type table type possible_keys  key key_len  ref rows Extra
      1  1      SIMPLE  test  ALL          <NA> <NA>    <NA> <NA>    3      

