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
    # dln_regular = log(P_reg_t / P_reg_{t-1}) — только РЕГУЛЯРНАЯ цена (без акций).
    # Промо-периоды включены, но в зависимой переменной отражается изменение
    # цены на полке (price_regular), а не то, сколько платит покупатель.
    dln_regular = log(price_regular / lag_regular),
    is_magnit   = as.integer(store_chain == "Magnit"),
    # is_promo_i здесь НЕ включается в модель: RC1 тестирует только динамику
    # регулярных цен вне зависимости от промо-активности
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
    # RC2 использует ту же ЭФФЕКТИВНУЮ цену, что и M1, но с более мягким порогом
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

# RC3 также использует ЭФФЕКТИВНУЮ цену — та же dln_effective, что в M1,
# но без наблюдений с экстремальными изменениями (> ±50%)
m_rc3 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data %>% filter(abs(dln_effective) <= 0.5),
  cluster = ~store_fct
)

# ── 7.3b Диагностика выбросов RC3 ────────────────────────────────────────────
# ОТКУДА БЕРУТСЯ ВЫБРОСЫ |Δln(P)| > 50%?
# 1. Ошибки сбора данных: парсинг страницы поймал акционный баннер вместо цены.
# 2. Товары при уценке/списании (напр. скоропорт за день до истечения срока).
# 3. Редкие промо >50%: «-60% на всё молоко» — реальная, но нетипичная акция.
# 4. Ввод/вывод товара: цена появляется после долгого перерыва с другим значением.
# Доля таких наблюдений мала, но они сильно влияют на OLS (квадрат отклонения).

n_total    <- nrow(reg_data)
n_outliers <- sum(abs(reg_data$dln_effective) > 0.5)
pct_out    <- round(n_outliers / n_total * 100, 2)

message(glue(
  "  RC3 выбросы: {n_outliers} из {n_total} ({pct_out}%) наблюдений исключено (|Δln P| > 50%)"
))

# Профиль выбросов: где они сконцентрированы?
outlier_profile <- reg_data %>%
  filter(abs(dln_effective) > 0.5) %>%
  group_by(store_chain, cat_group) %>%
  summarise(
    n_obs        = n(),
    avg_abs_chg  = mean(abs(dln_effective)),
    max_abs_chg  = max(abs(dln_effective)),
    pct_promo    = mean(is_promo_i),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

write_excel_csv(outlier_profile, file.path(PATH_TABLES, "11_outlier_profile.csv"))
message("  Профиль выбросов сохранён: 11_outlier_profile.csv")

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

write_excel_csv(rc_coefs, file.path(PATH_TABLES, "11_robustness_coefs.csv"))

# ── График robustness: ОТДЕЛЬНЫЙ файл на каждый коэффициент ──────────────────
# Каждый график — один коэффициент, все 4 спецификации на оси Y.
# Нет перекрытий, нет dodging, читается сразу.

make_rc_plot <- function(data, term_filter, title_text, subtitle_text) {
  data %>%
    filter(term == term_filter) %>%
    mutate(
      ci_lo = estimate - 1.96 * std_error,
      ci_hi = estimate + 1.96 * std_error,
      sig   = p_value < 0.05,
      stars = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                        p_value < 0.10 ~ "*",   TRUE ~ "")
    ) %>%
    ggplot(aes(x = estimate, y = reorder(model, estimate), colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
    geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.9, linewidth = 1.1) +
    geom_text(aes(label = glue("{round(estimate, 4)}{stars}")),
              hjust = -0.15, size = 4.2, fontface = "bold") +
    scale_colour_manual(values = c("TRUE" = "#d73027", "FALSE" = "grey55"),
                        labels  = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name    = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.30))) +
    labs(title = title_text, subtitle = subtitle_text,
         x = "Оценка β (±95% ДИ)", y = NULL) +
    theme_price() +
    theme(legend.position = "bottom")
}

# 10a: β(Магнит) — M1, RC1, RC2, RC3
p_rc_mag <- make_rc_plot(
  rc_coefs, "is_magnit",
  title_text    = "β(Магнит): устойчивость к выбору спецификации",
  subtitle_text = "Зависимая переменная — Δln(P) | M1: эффективная; RC1: регулярная; RC2/RC3: эффективная"
)
ggsave(file.path(PATH_PLOTS, "18_rc_magnit.png"),
       p_rc_mag, width = 10, height = 5, dpi = 180)

# 10b: β(Промо) — M1, RC2, RC3 (RC1 не включает промо)
p_rc_prm <- make_rc_plot(
  rc_coefs, "is_promo_i",
  title_text    = "β(Промо): устойчивость к выбору спецификации",
  subtitle_text = "Зависимая переменная — Δln(эффективной цены) | RC1 исключён (нет промо-переменной)"
)
ggsave(file.path(PATH_PLOTS, "19_rc_promo.png"),
       p_rc_prm, width = 10, height = 5, dpi = 180)

message("  RC-plots: 18_rc_magnit.png, 19_rc_promo.png")

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
message(glue("  Выбросы RC3: {n_outliers} наблюдений ({pct_out}%) с |Δln P| > 50% — см. 11_outlier_profile.csv"))
message("=== Блок 07 завершён: robustness checks и диагностика выбросов ===")
