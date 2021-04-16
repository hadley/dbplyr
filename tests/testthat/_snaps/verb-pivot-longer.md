# can pivot all cols to long

    Code
      lazy_frame(x = 1:2, y = 3:4) %>% tidyr::pivot_longer(x:y)
    Output
      <SQL>
      (
        SELECT
          'x' AS `name`,
          `x` AS `value`
        FROM `df`
      )
      UNION ALL
      (
        SELECT
          'y' AS `name`,
          `y` AS `value`
        FROM `df`
      )

# can add multiple columns from spec

    Code
      pv
    Output
      <SQL>
      (
        SELECT
          11 AS `a`,
          13 AS `b`,
          `x` AS `v`
        FROM `df`
      )
      UNION ALL
      (
        SELECT
          12 AS `a`,
          14 AS `b`,
          `y` AS `v`
        FROM `df`
      )

# preserves original keys

    Code
      pv
    Output
      <SQL>
      (
        SELECT
          `x`,
          'y' AS `name`,
          `y` AS `value`
        FROM `df`
      )
      UNION ALL
      (
        SELECT
          `x`,
          'z' AS `name`,
          `z` AS `value`
        FROM `df`
      )

# can drop missing values

    Code
      lazy_frame(x = c(1, NA), y = c(NA, 2)) %>% tidyr::pivot_longer(x:y,
      values_drop_na = TRUE)
    Output
      <SQL>
      SELECT *
      FROM (
        (
          SELECT
            'x' AS `name`,
            `x` AS `value`
          FROM `df`
        )
        UNION ALL
        (
          SELECT
            'y' AS `name`,
            `y` AS `value`
          FROM `df`
        )
      ) `q01`
      WHERE (NOT(((`value`) IS NULL)))

# can handle missing combinations

    Code
      sql
    Output
      <SQL>
      (
        SELECT
          `id`,
          `n`,
          `x`,
          NULL AS `y`
        FROM (
          SELECT
            `id`,
            '1' AS `n`,
            `x_1` AS `x`
          FROM `df`
        ) `q01`
      )
      UNION ALL
      (
        SELECT
          `id`,
          '2' AS `n`,
          `x_2` AS `x`,
          `y_2` AS `y`
        FROM `df`
      )

# can override default output column type

    Code
      lazy_frame(x = 1) %>% tidyr::pivot_longer(x, values_transform = list(value = as.character))
    Output
      <SQL>
      SELECT
        'x' AS `name`,
        CAST(`x` AS TEXT) AS `value`
      FROM `df`

# can pivot to multiple measure cols

    Code
      pv
    Output
      <SQL>
      SELECT
        1.0 AS `row`,
        `x` AS `X`,
        `y` AS `Y`
      FROM `df`

# .value can be at any position in `names_to`

    Code
      value_first
    Output
      <SQL>
      (
        SELECT
          `i`,
          't1' AS `time`,
          `y_t1` AS `y`,
          `z_t1` AS `z`
        FROM `df`
      )
      UNION ALL
      (
        SELECT
          `i`,
          't2' AS `time`,
          `y_t2` AS `y`,
          `z_t2` AS `z`
        FROM `df`
      )

---

    Code
      value_second
    Output
      <SQL>
      (
        SELECT
          `i`,
          't1' AS `time`,
          `t1_y` AS `y`,
          `t1_z` AS `z`
        FROM `df`
      )
      UNION ALL
      (
        SELECT
          `i`,
          't2' AS `time`,
          `t2_y` AS `y`,
          `t2_z` AS `z`
        FROM `df`
      )

