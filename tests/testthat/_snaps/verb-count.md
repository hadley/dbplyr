# generates expected SQL for common situations

    Code
      db %>% count(g)
    Output
      <SQL>
      SELECT
        `g`,
        COUNT(*) AS `n`
      FROM `df`
      GROUP BY `g`

---

    Code
      db %>% count(g, wt = x)
    Output
      <SQL>
      SELECT
        `g`,
        SUM(`x`) AS `n`
      FROM `df`
      GROUP BY `g`

---

    Code
      db %>% count(g, sort = TRUE)
    Output
      <SQL>
      SELECT
        `g`,
        COUNT(*) AS `n`
      FROM `df`
      GROUP BY `g`
      ORDER BY `n` DESC

# complains about bad names

    Code
      db <- lazy_frame(g = 1, x = 2)
      db %>% count(g, name = "g")
    Error <rlang_error>
      'g' already present in output
      i Use `name = "new_name"` to pick a new name.

