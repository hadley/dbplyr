# print method doesn't change unexpectedly

    Code
      sql_build(left_join(lf1, lf2))
    Message <message>
      Joining, by = "x"
    Output
      <SQL JOIN (LEFT)>
      By:
        x-x
      X:
        <IDENT> df
      Y:
        <IDENT> df

# generated sql doesn't change unexpectedly

    Code
      inner_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x`,
        `LHS`.`y` AS `y`
      FROM `df` AS `LHS`
      INNER JOIN `df` AS `RHS`
      ON (
        `LHS`.`x` = `RHS`.`x` AND
        `LHS`.`y` = `RHS`.`y`
      )

---

    Code
      left_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x`,
        `LHS`.`y` AS `y`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (
        `LHS`.`x` = `RHS`.`x` AND
        `LHS`.`y` = `RHS`.`y`
      )

---

    Code
      right_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT
        `RHS`.`x` AS `x`,
        `RHS`.`y` AS `y`
      FROM `df` AS `LHS`
      RIGHT JOIN `df` AS `RHS`
      ON (
        `LHS`.`x` = `RHS`.`x` AND
        `LHS`.`y` = `RHS`.`y`
      )

---

    Code
      full_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT
        COALESCE(`LHS`.`x`, `RHS`.`x`) AS `x`,
        COALESCE(`LHS`.`y`, `RHS`.`y`) AS `y`
      FROM `df` AS `LHS`
      FULL JOIN `df` AS `RHS`
      ON (
        `LHS`.`x` = `RHS`.`x` AND
        `LHS`.`y` = `RHS`.`y`
      )

# only disambiguates shared variables

    Code
      left_join(lf1, lf2)
    Message <message>
      Joining, by = "x"
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x`,
        `y`,
        `z`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (`LHS`.`x` = `RHS`.`x`)

---

    Code
      left_join(lf1, lf2, by = c(y = "z"))
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x.x`,
        `y`,
        `RHS`.`x` AS `x.y`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (`LHS`.`y` = `RHS`.`z`)

# sql_on query doesn't change unexpectedly

    Code
      inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x.x`,
        `y`,
        `RHS`.`x` AS `x.y`,
        `z`
      FROM `df` AS `LHS`
      INNER JOIN `df` AS `RHS`
      ON (LHS.y < RHS.z)

---

    Code
      left_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x.x`,
        `y`,
        `RHS`.`x` AS `x.y`,
        `z`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (LHS.y < RHS.z)

---

    Code
      right_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x.x`,
        `y`,
        `RHS`.`x` AS `x.y`,
        `z`
      FROM `df` AS `LHS`
      RIGHT JOIN `df` AS `RHS`
      ON (LHS.y < RHS.z)

---

    Code
      full_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT
        `LHS`.`x` AS `x.x`,
        `y`,
        `RHS`.`x` AS `x.y`,
        `z`
      FROM `df` AS `LHS`
      FULL JOIN `df` AS `RHS`
      ON (LHS.y < RHS.z)

---

    Code
      semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE (LHS.y < RHS.z)
      )

---

    Code
      anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE (LHS.y < RHS.z)
      )

