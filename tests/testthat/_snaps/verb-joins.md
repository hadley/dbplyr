# can optionally match NA values

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (CASE WHEN (`LHS`.`x` = `RHS`.`x`) OR (`LHS`.`x` IS NULL AND `RHS`.`x` IS NULL) THEN 0 ELSE 1 = 0)

