# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT *
      FROM (`df`) 
      FETCH FIRST 6 ROWS ONLY

# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl` COMPUTE STATISTICS

---

    Code
      sql_query_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN PLAN FOR SELECT * FROM foo;
      SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));

---

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM (`df`) `LHS`
      LEFT JOIN (`df`) `RHS`
      ON (decode(`LHS`.`x`, `RHS`.`x`, 0, 1) = 0)

