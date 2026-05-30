# ================== 09. РЕГРЕССИИ НА ЧАСТОТУ ИЗМЕНЕНИЙ ========================
# Зависимая переменная: вероятность изменения эффективной цены (changed_effective).
# Анализ: LPM (OLS + FE), кросс-цепочечная синхронность (Granger), AR(1), Random Forest.
# Требует в памяти: panel, weekly_chg_rate, weekly_wide, inter_cat_cors,
#                   inter_cat_combined_df, cat_group_map, tidy_feols, reg_data.
# ==============================================================================

message("\n[08] Регрессии на частоту изменений цен...")

# ── Вспомогательная функция: один коэффициент × несколько спецификаций ───────
# label-столбец должен быть в data перед вызовом.
make_single_coef_plot <- function(data, title_text, subtitle_text = "",
                                   x_label = "Оценка β (±95% ДИ)") {
  data %>%
    mutate(
      ci_lo = estimate - 1.96 * std_error,
      ci_hi = estimate + 1.96 * std_error,
      sig   = p_value < 0.05,
      stars = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                        p_value < 0.10 ~ "*",   TRUE ~ "")
    ) %>%
    ggplot(aes(x = estimate, y = reorder(label, estimate), colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
    geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.9, linewidth = 1.1) +
    geom_text(aes(label = glue("{round(estimate, 4)}{stars}")),
              hjust = -0.15, size = 4.2, fontface = "bold") +
    scale_colour_manual(values = c("TRUE" = "#d73027", "FALSE" = "grey55"),
                        labels  = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name    = NULL, drop = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.30))) +
    labs(title = title_text, subtitle = subtitle_text, x = x_label, y = NULL) +
    theme_price() +
    theme(legend.position = "bottom")
}

# ── Установка дополнительных пакетов (ML) ──────────────────────────────────────
RUN_ML <- TRUE   # FALSE — пропустить RF и XGBoost (ускоряет прогон)

pkgs_needed <- c("ranger")
if (RUN_ML) {
  to_inst <- pkgs_needed[!pkgs_needed %in% installed.packages()[, "Package"]]
  if (length(to_inst) > 0) {
    message("Устанавливаем пакеты ML: ", paste(to_inst, collapse = ", "))
    install.packages(to_inst, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(ranger))
}

# ══════════════════════════════════════════════════════════════════════════════
# 9.1  ТАБЛИЦА ТОП-5 КОРРЕЛЯЦИЙ ПО КАТЕГОРИЯМ
# Для каждой категории и каждой выборки (Пятёрочка / Магнит / Обе) —
# топ-5 наиболее коррелирующих категорий по частоте изменений.
# ══════════════════════════════════════════════════════════════════════════════
message("\n[08-1] Топ-5 корреляций по категориям...")

# Объединяем попарные корреляции трёх выборок в один датафрейм
all_cors <- bind_rows(inter_cat_cors, inter_cat_combined_df) %>%
  # добавляем симметричные пары, чтобы каждая категория была как «источник»
  bind_rows(
    bind_rows(inter_cat_cors, inter_cat_combined_df) %>%
      select(cat_from = cat_to, cat_to = cat_from, correlation, store_chain)
  ) %>%
  distinct(store_chain, cat_from, cat_to, .keep_all = TRUE) %>%
  filter(cat_from != cat_to)

# Топ-5 по каждой (сеть, категория)
top5_long <- all_cors %>%
  group_by(store_chain, cat_from) %>%
  slice_max(abs(correlation), n = 5, with_ties = FALSE) %>%
  arrange(store_chain, cat_from, desc(abs(correlation))) %>%
  mutate(rank = row_number(),
         entry = glue("{cat_to} ({round(correlation, 3)})")) %>%
  ungroup()

write_excel_csv(top5_long, file.path(PATH_TABLES, "12_top5_correlations_long.csv"))

# Сводная таблица: широкий формат (категория × сеть = топ-5 строкой)
top5_wide <- top5_long %>%
  group_by(store_chain, cat_from) %>%
  summarise(top5_str = paste(entry, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = store_chain, values_from = top5_str,
              names_prefix = "Топ5_") %>%
  rename(Категория = cat_from)

write_excel_csv(top5_wide, file.path(PATH_TABLES, "12_top5_correlations.csv"))

gt_top5 <- top5_wide %>%
  gt() %>%
  tab_header(
    title    = "Топ-5 категорий с наибольшей корреляцией изменений цен",
    subtitle = "Попарная корреляция недельной частоты изменения эффективной цены"
  ) %>%
  tab_footnote("В скобках — коэффициент корреляции Пирсона") %>%
  opt_stylize(style = 6)

gt_top5 %>% gtsave(file.path(PATH_TABLES, "12_top5_correlations.html"))
message("  Топ-5 корреляций сохранены: 12_top5_correlations.csv + .html")

# ══════════════════════════════════════════════════════════════════════════════
# 9.2  LPM — ЛИНЕЙНАЯ МОДЕЛЬ ВЕРОЯТНОСТИ ИЗМЕНЕНИЯ ЦЕНЫ
# DV = changed_effective (0/1), оцениваем OLS.
# LPM интерпретируется как: на сколько п.п. растёт вероятность изменения цены
# при наличии промо или принадлежности к Магниту (после контроля FE).
# ══════════════════════════════════════════════════════════════════════════════
message("\n[08-2] LPM-регрессии на вероятность изменения цены...")

# reg_data уже содержит changed_effective, cat_fct, week_fct, store_fct, is_magnit, is_promo_i
# (создан в 06_panel_regression.R)

# M_LPM1: FE по категории и неделе (как основная M1, но DV = changed_effective)
lpm_m1 <- feols(
  changed_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# M_LPM2: + FE по магазину (поглощает is_magnit)
lpm_m2 <- feols(
  changed_effective ~ is_promo_i | cat_fct + week_fct + store_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# M_LPM3: только Пятёрочка
lpm_m3 <- feols(
  changed_effective ~ is_promo_i | cat_fct + week_fct,
  data    = filter(reg_data, store_chain == "Pyaterochka"),
  cluster = ~store_fct
)

lpm_coefs <- bind_rows(
  tidy_feols(lpm_m1) %>% mutate(model = "LPM-M1: FE(cat+week)"),
  tidy_feols(lpm_m2) %>% mutate(model = "LPM-M2: FE(cat+week+store)"),
  tidy_feols(lpm_m3) %>% mutate(model = "LPM-M3: только Пятёрочка")
)

lpm_gof <- tibble(
  Спецификация = c("LPM-M1", "LPM-M2", "LPM-M3"),
  N            = c(nobs(lpm_m1), nobs(lpm_m2), nobs(lpm_m3)),
  R2_within    = c(r2(lpm_m1, "r2"),  r2(lpm_m2, "r2"),  r2(lpm_m3, "r2")),
  R2_adj       = c(r2(lpm_m1, "ar2"), r2(lpm_m2, "ar2"), r2(lpm_m3, "ar2"))
)

write_excel_csv(lpm_coefs, file.path(PATH_TABLES, "13_lpm_coefs.csv"))
write_excel_csv(lpm_gof,   file.path(PATH_TABLES, "13_lpm_gof.csv"))

gt_lpm <- lpm_gof %>%
  gt() %>%
  tab_header(
    title    = "LPM: качество подгонки",
    subtitle = "Зависимая переменная: вероятность изменения эффективной цены (0/1)"
  ) %>%
  fmt_integer(N) %>%
  fmt_percent(c(R2_within, R2_adj), decimals = 2) %>%
  opt_stylize(style = 6)

gt_lpm %>% gtsave(file.path(PATH_TABLES, "13_lpm_gof.html"))

# 20a: β(Магнит) — только LPM-M1 (M2 поглощает is_magnit через store FE; M3 — Pya only)
lpm_coefs %>%
  filter(term == "is_magnit") %>%
  mutate(label = model) %>%
  make_single_coef_plot(
    title_text    = "LPM — β(Магнит): вероятность изменения эффективной цены",
    subtitle_text = "LPM-M1: FE(категория + неделя) | SE кластеризованы по магазину",
    x_label       = "Изменение вероятности (п.п.)"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "20a_lpm_magnit.png"), ., width = 9, height = 3.5, dpi = 180) }

# 20b: β(Промо) — три LPM-спецификации, одна строка на спецификацию
lpm_coefs %>%
  filter(term == "is_promo_i") %>%
  mutate(label = model) %>%
  make_single_coef_plot(
    title_text    = "LPM — β(Промо): вероятность изменения эффективной цены",
    subtitle_text = "Три спецификации | SE кластеризованы по магазину",
    x_label       = "Изменение вероятности (п.п.)"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "20b_lpm_promo.png"), ., width = 9, height = 5, dpi = 180) }

message("  LPM завершён. Сохранено: 13_lpm_coefs.csv, 13_lpm_gof.csv/.html, 20a_lpm_magnit.png, 20b_lpm_promo.png")

# ══════════════════════════════════════════════════════════════════════════════
# 9.3  КРОСС-ЦЕПОЧЕЧНАЯ СИНХРОННОСТЬ (GRANGER-ПОДХОД)
# Вопрос: предсказывает ли прошлая частота изменений Магнита частоту Пятёрочки
# в следующую неделю (и наоборот)?
# Это не строгий тест Грейнджера (нет F-теста на ограничение), но показывает
# направление и силу кросс-лагового эффекта.
# ══════════════════════════════════════════════════════════════════════════════
message("\n[08-3] Кросс-цепочечная синхронность (Granger-подход)...")

# weekly_wide содержит chg_rate_Pyaterochka и chg_rate_Magnit по неделям и категориям
granger_data <- weekly_wide %>%
  group_by(category_name) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(
    lag_pya = lag(chg_rate_Pyaterochka),
    lag_mag = lag(chg_rate_Magnit)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_pya), !is.na(lag_mag),
         !is.na(chg_rate_Pyaterochka), !is.na(chg_rate_Magnit)) %>%
  mutate(cat_fct = factor(category_name))

# Пятёрочка ~ лаг(Пятёрочки) + лаг(Магнита): значим ли лаг Магнита?
granger_pya <- feols(
  chg_rate_Pyaterochka ~ lag_pya + lag_mag | cat_fct,
  data    = granger_data,
  cluster = ~cat_fct
)

# Магнит ~ лаг(Магнита) + лаг(Пятёрочки): значим ли лаг Пятёрочки?
granger_mag <- feols(
  chg_rate_Magnit ~ lag_mag + lag_pya | cat_fct,
  data    = granger_data,
  cluster = ~cat_fct
)

granger_coefs <- bind_rows(
  tidy_feols(granger_pya) %>% mutate(model = "DV: частота Пятёрочки"),
  tidy_feols(granger_mag) %>% mutate(model = "DV: частота Магнита")
) %>%
  mutate(term_ru = case_when(
    term == "lag_pya" ~ "Лаг частоты Пятёрочки (t-1)",
    term == "lag_mag" ~ "Лаг частоты Магнита (t-1)",
    TRUE ~ term
  ))

write_excel_csv(granger_coefs, file.path(PATH_TABLES, "14_granger_coefs.csv"))

gt_granger <- granger_coefs %>%
  select(model, term_ru, estimate, std_error, p_value) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",  TRUE ~ ""
    ),
    coef_fmt = glue("{round(estimate, 4)}{stars} ({round(std_error, 4)})")
  ) %>%
  select(model, term_ru, coef_fmt) %>%
  pivot_wider(names_from = term_ru, values_from = coef_fmt) %>%
  gt() %>%
  tab_header(
    title    = "Кросс-цепочечная синхронность: лаговые регрессии",
    subtitle = "FE по категории | SE кластеризованы по категории | * p<0.10, ** p<0.05, *** p<0.01"
  ) %>%
  cols_label(model = "Зависимая переменная") %>%
  opt_stylize(style = 6)

gt_granger %>% gtsave(file.path(PATH_TABLES, "14_granger_coefs.html"))

# 21a: DV = частота Пятёрочки (предсказывает ли лаг Магнита?)
granger_coefs %>%
  filter(model == "DV: частота Пятёрочки") %>%
  mutate(label = term_ru) %>%
  make_single_coef_plot(
    title_text    = "Granger: предсказание частоты изменений Пятёрочки",
    subtitle_text = "DV = chg_rate_Pya_t | FE по категории | Значим ли лаг Магнита?"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "21a_granger_pya.png"), ., width = 9, height = 4.5, dpi = 180) }

# 21b: DV = частота Магнита (предсказывает ли лаг Пятёрочки?)
granger_coefs %>%
  filter(model == "DV: частота Магнита") %>%
  mutate(label = term_ru) %>%
  make_single_coef_plot(
    title_text    = "Granger: предсказание частоты изменений Магнита",
    subtitle_text = "DV = chg_rate_Mag_t | FE по категории | Значим ли лаг Пятёрочки?"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "21b_granger_mag.png"), ., width = 9, height = 4.5, dpi = 180) }

message("  Granger завершён: 14_granger_coefs.csv/.html, 21a_granger_pya.png, 21b_granger_mag.png")

# ══════════════════════════════════════════════════════════════════════════════
# 9.4  AR(1) ПАНЕЛЬНАЯ МОДЕЛЬ
# Зависит ли вероятность изменения цены в t от того, менялась ли она в t-1?
# AR(1) коэффициент < 0: «ценовое торможение» (после изменения — пауза).
# AR(1) коэффициент > 0: «инерция» (если меняли — скорее всего изменят снова).
# ══════════════════════════════════════════════════════════════════════════════
message("\n[08-4] AR(1) панельная модель на вероятность изменения цены...")

ar_data <- panel %>%
  left_join(cat_group_map, by = "category_id") %>%
  filter(!is.na(changed_effective), !is.na(cat_group)) %>%
  group_by(store_chain, store_code, product_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    lag_changed = lag(changed_effective),  # 1 если на прошлой неделе цена менялась
    lag_promo   = as.integer(lag(is_promo))
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_changed)) %>%
  mutate(
    cat_fct   = factor(cat_group),
    week_fct  = factor(week),
    store_fct = factor(store_code),
    is_magnit = as.integer(store_chain == "Magnit"),
    is_promo_i = as.integer(is_promo)
  )

# AR(1) базовая: только лаг DV
ar_m1 <- feols(
  changed_effective ~ lag_changed | cat_fct + week_fct,
  data    = ar_data,
  cluster = ~store_fct
)

# AR(1) + промо
ar_m2 <- feols(
  changed_effective ~ lag_changed + is_promo_i | cat_fct + week_fct,
  data    = ar_data,
  cluster = ~store_fct
)

# AR(1) + промо + сеть
ar_m3 <- feols(
  changed_effective ~ lag_changed + is_promo_i + is_magnit | cat_fct + week_fct,
  data    = ar_data,
  cluster = ~store_fct
)

ar_coefs <- bind_rows(
  tidy_feols(ar_m1) %>% mutate(model = "AR(1) базовая"),
  tidy_feols(ar_m2) %>% mutate(model = "AR(1) + Промо"),
  tidy_feols(ar_m3) %>% mutate(model = "AR(1) + Промо + Сеть")
) %>%
  mutate(term_ru = case_when(
    term == "lag_changed" ~ "P(изм. цены) в t-1",
    term == "is_promo_i"  ~ "Промо-акция",
    term == "is_magnit"   ~ "Магнит (vs Пятёрочка)",
    TRUE ~ term
  ))

ar_gof <- tibble(
  Спецификация = c("AR(1) базовая", "AR(1) + Промо", "AR(1) + Промо + Сеть"),
  N            = c(nobs(ar_m1), nobs(ar_m2), nobs(ar_m3)),
  R2_within    = c(r2(ar_m1, "r2"),  r2(ar_m2, "r2"),  r2(ar_m3, "r2")),
  R2_adj       = c(r2(ar_m1, "ar2"), r2(ar_m2, "ar2"), r2(ar_m3, "ar2"))
)

write_excel_csv(ar_coefs, file.path(PATH_TABLES, "15_ar_coefs.csv"))
write_excel_csv(ar_gof,   file.path(PATH_TABLES, "15_ar_gof.csv"))

gt_ar <- ar_gof %>%
  gt() %>%
  tab_header(
    title    = "AR(1): качество подгонки",
    subtitle = "Зависимая переменная: P(изменение эффективной цены) | FE: категория + неделя"
  ) %>%
  fmt_integer(N) %>%
  fmt_percent(c(R2_within, R2_adj), decimals = 2) %>%
  opt_stylize(style = 6)

gt_ar %>% gtsave(file.path(PATH_TABLES, "15_ar_gof.html"))

# 22a: ρ(lag_changed) — по всем спецификациям AR(1)
ar_coefs %>%
  filter(term == "lag_changed") %>%
  mutate(label = model) %>%
  make_single_coef_plot(
    title_text    = "AR(1) — ρ(лаг изменения цены): инерция vs торможение",
    subtitle_text = "ρ < 0 = пауза после изменения | ρ > 0 = серия изменений подряд"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "22a_ar_lag.png"), ., width = 9, height = 5, dpi = 180) }

# 22b: β(Промо) из AR-M2 и AR-M3
ar_coefs %>%
  filter(term == "is_promo_i") %>%
  mutate(label = model) %>%
  make_single_coef_plot(
    title_text    = "AR(1) — β(Промо): вклад акции сверх авторегрессии",
    subtitle_text = "AR-M2 и AR-M3 | SE кластеризованы по магазину"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "22b_ar_promo.png"), ., width = 9, height = 4.5, dpi = 180) }

message("  AR(1) завершён: 15_ar_coefs.csv, 15_ar_gof.csv/.html, 22a_ar_lag.png, 22b_ar_promo.png")

# ══════════════════════════════════════════════════════════════════════════════
# 9.5  RANDOM FOREST: ВАЖНОСТЬ ПЕРЕМЕННЫХ
# Какие факторы лучше всего предсказывают изменение цены?
# Используем стандартную импurity-важность из ranger.
# ══════════════════════════════════════════════════════════════════════════════
if (RUN_ML) {
  message("\n[08-5] Random Forest (важность переменных)...")

  rf_data <- reg_data %>%
    select(changed_effective, is_magnit, is_promo_i, cat_fct, week_fct, store_fct) %>%
    na.omit() %>%
    mutate(changed_effective = as.numeric(changed_effective))

  set.seed(42)
  rf_mod <- ranger(
    formula     = changed_effective ~ .,
    data        = rf_data,
    num.trees   = 300,
    importance  = "impurity",
    num.threads = 2,
    seed        = 42
  )

  rf_imp <- tibble(
    variable   = names(rf_mod$variable.importance),
    importance = rf_mod$variable.importance
  ) %>%
    arrange(desc(importance)) %>%
    mutate(rank = row_number())

  # Показываем топ-20, если переменных больше (category/week/store FE создают много уровней)
  rf_imp_top <- rf_imp %>%
    mutate(var_group = case_when(
      str_starts(variable, "cat_fct")   ~ "Категория (FE)",
      str_starts(variable, "week_fct")  ~ "Неделя (FE)",
      str_starts(variable, "store_fct") ~ "Магазин (FE)",
      variable == "is_promo_i"          ~ "Промо-акция",
      variable == "is_magnit"           ~ "Сеть (Магнит)",
      TRUE ~ variable
    )) %>%
    group_by(var_group) %>%
    summarise(importance_sum = sum(importance), .groups = "drop") %>%
    arrange(desc(importance_sum))

  write_excel_csv(rf_imp,     file.path(PATH_TABLES, "16_rf_importance_full.csv"))
  write_excel_csv(rf_imp_top, file.path(PATH_TABLES, "16_rf_importance_grouped.csv"))

  p_rf <- rf_imp_top %>%
    ggplot(aes(x = reorder(var_group, importance_sum), y = importance_sum)) +
    geom_col(fill = "#2563eb", alpha = 0.85) +
    geom_text(aes(label = formatC(importance_sum, format = "e", digits = 2)),
              hjust = -0.1, size = 4) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = "Random Forest: важность групп переменных для предсказания изменения цены",
      subtitle = glue("N деревьев: 300 | N наблюдений: {nrow(rf_data)} | OOB R²: {round(1 - rf_mod$prediction.error, 3)}"),
      x        = NULL,
      y        = "Суммарная важность (impurity)"
    ) +
    theme_price()

  ggsave(file.path(PATH_PLOTS, "23_rf_importance.png"),
         p_rf, width = 11, height = 6, dpi = 180)

  message(glue("  Random Forest завершён. OOB R² = {round(1 - rf_mod$prediction.error, 3)}"))
  message("  Сохранено: 16_rf_importance_*.csv, 23_rf_importance.png")

  # XGBoost + SHAP — опционально (раскомментировать при наличии пакетов)
  # pkgs_xgb <- c("xgboost", "shapviz")
  # if (all(pkgs_xgb %in% installed.packages()[,"Package"])) {
  #   library(xgboost); library(shapviz)
  #   # ... XGBoost pipeline ...
  # } else {
  #   message("  XGBoost/SHAP пропущен: установите xgboost и shapviz для запуска")
  # }

} else {
  message("  [09-5] ML пропущен (RUN_ML = FALSE)")
}

message("\n=== Блок 08 завершён: топ-корреляции, LPM, Granger, AR(1), RF ===")
