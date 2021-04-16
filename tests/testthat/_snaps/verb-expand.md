# expand completes all values

    Code
      lazy_frame(x = 1, y = 1) %>% tidyr::expand(x, y)
    Output
      <SQL>
      SELECT
        `x`,
        `y`
      FROM (
        SELECT DISTINCT `x`
        FROM `df`
      ) `LHS`
      LEFT JOIN (
        SELECT DISTINCT `y`
        FROM `df`
      ) `RHS`
      

# nesting doesn't expand values

    Code
      df_lazy %>% tidyr::expand(nesting(x, y))
    Output
      <SQL>
      SELECT DISTINCT
        `x`,
        `y`
      FROM `df`

# expand accepts expressions

    Code
      tidyr::expand(df, round(x / 2))
    Output
      <SQL>
      SELECT DISTINCT ROUND(`x` / 2.0, 0) AS `round(x/2)`
      FROM `df`

---

    Code
      tidyr::expand(df, nesting(x_half = round(x / 2), x1 = x + 1))
    Output
      <SQL>
      SELECT DISTINCT
        ROUND(`x` / 2.0, 0) AS `x_half`,
        `x` + 1.0 AS `x1`
      FROM `df`

# expand respects groups

    Code
      df_lazy %>% group_by(a) %>% tidyr::expand(b, c)
    Output
      <SQL>
      SELECT
        `LHS`.`a` AS `a`,
        `b`,
        `c`
      FROM (
        SELECT DISTINCT
          `a`,
          `b`
        FROM `df`
      ) `LHS`
      LEFT JOIN (
        SELECT DISTINCT
          `a`,
          `c`
        FROM `df`
      ) `RHS`
      ON (`LHS`.`a` = `RHS`.`a`)

# NULL inputs

    Code
      tidyr::expand(lazy_frame(x = 1), x, y = NULL)
    Output
      <SQL>
      SELECT DISTINCT `x`
      FROM `df`

# expand() errors when expected

    Must supply variables in `...`

---

    Must supply variables in `...`

# replace_na replaces missing values

    Code
      lazy_frame(x = 1, y = "a") %>% tidyr::replace_na(list(x = 0, y = "unknown"))
    Output
      <SQL>
      SELECT
        COALESCE(`x`, 0.0) AS `x`,
        COALESCE(`y`, 'unknown') AS `y`
      FROM `df`

# replace_na ignores missing columns

    Code
      lazy_frame(x = 1) %>% tidyr::replace_na(list(not_there = 0))
    Output
      <SQL>
      SELECT *
      FROM `df`

# complete completes missing combinations

    Code
      df_lazy %>% tidyr::complete(x, y, fill = list(z = "c"))
    Output
      <SQL>
      SELECT
        `x`,
        `y`,
        COALESCE(`z`, 'c') AS `z`
      FROM (
        SELECT
          COALESCE(`LHS`.`x`, `RHS`.`x`) AS `x`,
          COALESCE(`LHS`.`y`, `RHS`.`y`) AS `y`,
          `z`
        FROM (
          SELECT
            `x`,
            `y`
          FROM (
            SELECT DISTINCT `x`
            FROM `df`
          ) `LHS`
          LEFT JOIN (
            SELECT DISTINCT `y`
            FROM `df`
          ) `RHS`
      
        ) `LHS`
        FULL JOIN `df` AS `RHS`
        ON (
          `LHS`.`x` = `RHS`.`x` AND
          `LHS`.`y` = `RHS`.`y`
        )
      ) `q01`

