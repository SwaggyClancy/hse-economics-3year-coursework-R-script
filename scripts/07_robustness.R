# ================== 07. ROBUSTNESS CHECKS =======================================

message("\n[07] Проверки устойчивости...")

# ── 7.1 Robustness 1: Только регулярные цены (без промо) ──────────────────────
# ЧТО ТЕСТИРУЕМ (RC1): сохраняется ли эффект Магнита, если убрать промо из анализа?
# Если β(is_magnit) значим и в основной модели (effective price), и здесь
# (regular price) — разница между сетями не объясняется только промо-акциями.
# Используем category_name (не category_id!) как FE — иначе is_magnit поглощается
# (каждый category_id принадлежит только одной сети → perfect collinearity).
message("  [RC-1] Только регулярные цены...")

reg_data_regular <- panel %>%
  left_join(cat_group_map, by = "category_id") %>%   # cat_group_map из script 06
  filter(!is.na(delta_regular_pct)) %>%
  mutate(
    dln_regular = log(price_regular / lag_regular),
    is_magnit   = as.integer(store_chain == "Magnit"),
    week_fct    = factor(week),
    cat_fct     = factor(cat_group),   # кросс-цепочечный ключ (как в script 06)
    store_fct   = factor(store_code)
  ) %>%
  filter(is.finite(dln_regular), !is.na(cat_fct))

m_rc1 <- feols(
  dln_regular ~ is_magnit | cat_fct + week_fct,
  data    = reg_data_regular,
  cluster = ~store_fct
)

# ── 7.2 Robustness 2: Другой порог изменения (0.5%) ───────────────────────────
# ЧТО ТЕСТИРУЕМ (RC2): чувствительны ли результаты к выбору порога "значимого изменения"?
# Основная модель использует порог 1% (CHANGE_THRESHOLD = 0.01).
# Здесь: 0.5% — более мягкий порог, включает мелкие изменения цен.
# Если β(is_magnit) сохраняет знак и значимость — результат не зависит от порога.
# Используем category_name (не category_id!) как FE — та же причина, что в RC1.
message("  [RC-2] Порог изменения 0.5%...")

THRESHOLD_RC2 <- 0.005

reg_data_rc2 <- panel %>%
  left_join(cat_group_map, by = "category_id") %>%   # cat_group_map из script 06
  filter(!is.na(delta_effective_pct)) %>%
  mutate(
    dln_effective   = log(effective_price / lag_effective),
    is_magnit       = as.integer(store_chain == "Magnit"),
    is_promo_i      = as.integer(is_promo),
    changed_eff_rc2 = abs(delta_effective_pct) > THRESHOLD_RC2,
    week_fct        = factor(week),
    cat_fct         = factor(cat_group),   # кросс-цепочечный ключ (как в script 06)
    store_fct       = factor(store_code)
  ) %>%
  filter(is.finite(dln_effective), changed_eff_rc2, !is.na(cat_fct))   # только периоды с изменением

m_rc2 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data_rc2,
  cluster = ~store_fct
)

# ── 7.3 Robustness 3: Исключение выбросов (|ΔP| > 50%) ───────────────────────
# ЧТО ТЕСТИРУЕМ (RC3): не искажают ли результаты экстремальные ценовые изменения?
# Выбросы >50% за неделю могут быть ошибками данных или уценкой при списании.
# Используем reg_data из script 06 (с cat_group как FE) — уже очищен корректно.
message("  [RC-3] Без выбросов (|ΔP| > 50%)...")

m_rc3 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data %>% filter(abs(dln_effective) <= 0.5),
  cluster = ~store_fct
)

# ── 7.4 Сводная таблица robustness ────────────────────────────────────────────
# КАК ЧИТАТЬ РЕЗУЛЬТАТ: если β(is_magnit) и β(is_promo_i) сохраняют знак и уровень
# значимости во всех RC — выводы из M1 устойчивы к выбору спецификации.
# Если знак меняется или эффект пропадает — результат чувствителен к допущениям.
# tidy_feols() определена в секции 06
tidy_rc <- function(model, label) {
  tidy_feols(model) %>% mutate(model = label)
}

rc_coefs <- bind_rows(
  tidy_rc(m1,    "Основная (M1)"),
  tidy_rc(m_rc1, "RC1: Только рег. цены"),
  tidy_rc(m_rc2, glue("RC2: Порог {THRESHOLD_RC2*100}%")),
  tidy_rc(m_rc3, "RC3: Без выбросов")
) %>%
  filter(term %in% c("is_magnit", "is_promo_i"))

write_csv(rc_coefs, file.path(PATH_TABLES, "11_robustness_coefs.csv"))

# График robustness: коэффициент при Magnit
p_rc <- rc_coefs %>%
  mutate(
    ci_lo   = estimate - 1.96 * std_error,
    ci_hi   = estimate + 1.96 * std_error,
    sig     = p_value < 0.05,
    term_ru = if_else(term == "is_magnit", "Magnit (β)", "Промо (β)")
  ) %>%
  ggplot(aes(x = estimate, y = model, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.7) +
  scale_colour_manual(
    values = c("TRUE" = "#d73027", "FALSE" = "grey60"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = NULL
  ) +
  facet_wrap(~term_ru, scales = "free_x") +
  labs(
    title    = "Проверки устойчивости: коэффициенты основных регрессоров",
    subtitle = "Сравнение оценок β (±95% ДИ) по спецификациям",
    x        = "Оценка β",
    y        = NULL
  )

ggsave(file.path(PATH_PLOTS, "10_robustness_coef_plot.png"),
       p_rc, width = 12, height = 5, dpi = 150)

# gt-таблица сравнения robustness
gt_rc <- rc_coefs %>%
  select(model, term, estimate, std_error, p_value) %>%
  mutate(
    term      = case_when(
      term == "is_magnit"  ~ "Magnit",
      term == "is_promo_i" ~ "Промо",
      TRUE                 ~ term
    ),
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    coef_fmt = glue("{round(estimate, 4)}{stars}\n({round(std_error, 4)})")
  ) %>%
  select(model, term, coef_fmt) %>%
  pivot_wider(names_from = term, values_from = coef_fmt) %>%
  gt() %>%
  tab_header(
    title    = "Устойчивость результатов",
    subtitle = "В скобках — робастные SE, кластеризованные по магазину"
  ) %>%
  cols_label(model = "Спецификация") %>%
  tab_footnote("* p<0.10, ** p<0.05, *** p<0.01") %>%
  tab_style(
    style     = cell_fill(color = "#e8f4e8"),
    locations = cells_body(rows = model == "Основная (M1)")
  ) %>%
  opt_stylize(style = 6)

save_gt_table(gt_rc, rc_coefs %>% select(model, term, estimate, std_error, p_value),
              "11_robustness_table")

message("  Robustness checks завершены.")
