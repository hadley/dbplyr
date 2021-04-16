# across() translates character vectors

    Code
      lf %>% summarise(across(a:b, "log"))
    Output
      <SQL>
      SELECT
        LN(`a`) AS `a`,
        LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, "log", base = 2))
    Output
      <SQL>
      SELECT
        LOG(2.0, `a`) AS `a`,
        LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a, c("log", "exp")))
    Output
      <SQL>
      SELECT
        LN(`a`) AS `a_log`,
        EXP(`a`) AS `a_exp`
      FROM `df`

# across() translates functions

    Code
      lf %>% summarise(across(a:b, log))
    Output
      <SQL>
      SELECT
        LN(`a`) AS `a`,
        LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, log, base = 2))
    Output
      <SQL>
      SELECT
        LOG(2.0, `a`) AS `a`,
        LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, list(log, exp)))
    Output
      <SQL>
      SELECT
        LN(`a`) AS `a_log`,
        EXP(`a`) AS `a_exp`,
        LN(`b`) AS `b_log`,
        EXP(`b`) AS `b_exp`
      FROM `df`

# untranslatable functions are preserved

    Code
      lf %>% summarise(across(a:b, SQL_LOG))
    Output
      <SQL>
      SELECT
        SQL_LOG(`a`) AS `a`,
        SQL_LOG(`b`) AS `b`
      FROM `df`

# across() translates formulas

    Code
      lf %>% summarise(across(a:b, ~log(.x, 2)))
    Output
      <SQL>
      SELECT
        LOG(2.0, `a`) AS `a`,
        LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, list(~log(.x, 2))))
    Output
      <SQL>
      SELECT
        LOG(2.0, `a`) AS `a`,
        LOG(2.0, `b`) AS `b`
      FROM `df`

# across() translates NULL

    Code
      lf %>% mutate(across(a:b))
    Output
      <SQL>
      SELECT
        `a`,
        `b`
      FROM `df`

# old _at functions continue to work

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum")
    Warning <simpleWarning>
      Missing values are always removed in SQL.
      Use `SUM(x, na.rm = TRUE)` to silence this warning
      This warning is displayed only once per session.
    Output
      <SQL>
      SELECT
        SUM(`a`) AS `a`,
        SUM(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum)
    Output
      <SQL>
      SELECT
        SUM(`a`) AS `a`,
        SUM(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~sum(.))
    Output
      <SQL>
      SELECT
        SUM(`a`) AS `a`,
        SUM(`b`) AS `b`
      FROM `df`

# if_all/any works in filter()

    Code
      lf %>% filter(if_all(a:b, ~. > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 AND `b` > 0.0)

---

    Code
      lf %>% filter(if_any(a:b, ~. > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 OR `b` > 0.0)

# if_all/any works in mutate()

    Code
      lf %>% mutate(c = if_all(a:b, ~. > 0))
    Output
      <SQL>
      SELECT
        `a`,
        `b`,
        `a` > 0.0 AND `b` > 0.0 AS `c`
      FROM `df`

---

    Code
      lf %>% mutate(c = if_any(a:b, ~. > 0))
    Output
      <SQL>
      SELECT
        `a`,
        `b`,
        `a` > 0.0 OR `b` > 0.0 AS `c`
      FROM `df`

