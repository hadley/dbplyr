# summarise(.groups=)

    Code
      eval_bare(expr(lazy_frame(x = 1, y = 2) %>% dplyr::group_by(x, y) %>% dplyr::summarise() %>%
        remote_query()), env(global_env()))
    Message <message>
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      <SQL> SELECT
        `x`,
        `y`
      FROM `df`
      GROUP BY
        `x`,
        `y`

---

    `.groups` can't be "rowwise" in dbplyr
    i Possible values are NULL (default), "drop_last", "drop", and "keep"

# quoting for rendering summarized grouped table

    Code
      out %>% sql_render
    Output
      <SQL> SELECT
        `x`,
        COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`

