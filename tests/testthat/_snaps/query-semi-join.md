# print method doesn't change unexpectedly

    Code
      sql_build(semi_join(lf1, lf2))
    Message <message>
      Joining, by = "x"
    Output
      <SQL SEMI JOIN>
      By:
        x-x
      X:
        <IDENT> df
      Y:
        <IDENT> df

# generated sql doesn't change unexpectedly

    Code
      semi_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE
          (`LHS`.`x` = `RHS`.`x`) AND
          (`LHS`.`y` = `RHS`.`y`)
      )

---

    Code
      anti_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE
          (`LHS`.`x` = `RHS`.`x`) AND
          (`LHS`.`y` = `RHS`.`y`)
      )

