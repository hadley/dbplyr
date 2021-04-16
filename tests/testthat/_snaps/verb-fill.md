# up-direction works

    Code
      df_lazy_ns %>% window_order(id) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partion_1`) AS `n1`
      FROM (
        SELECT
          `id`,
          `group`,
          `n1`,
          SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (ORDER BY -`id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
        FROM `df`
      )

---

    Code
      df_lazy_std %>% window_order(id) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY -`id`) AS `n1`
      FROM `df`

# up-direction works with descending

    Code
      df_lazy_ns %>% window_order(desc(id)) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partion_1`) AS `n1`
      FROM (
        SELECT
          `id`,
          `group`,
          `n1`,
          SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (ORDER BY -`id` DESC ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
        FROM `df`
      )

---

    Code
      df_lazy_std %>% window_order(desc(id)) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY -`id` DESC) AS `n1`
      FROM `df`

# groups are respected

    Code
      group_by(df_lazy_ns, group) %>% window_order(id) %>% tidyr::fill(n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `group`, `..dbplyr_partion_1`) AS `n1`
      FROM (
        SELECT
          `id`,
          `group`,
          `n1`,
          SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (PARTITION BY `group` ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
        FROM `df`
      )

---

    Code
      group_by(df_lazy_std, group) %>% window_order(id) %>% tidyr::fill(n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (PARTITION BY `group` ORDER BY `id`) AS `n1`
      FROM `df`

# fill errors on unsorted data

    Code
      expect_error(df_db %>% tidyr::fill(n1))

