# ================== 02. DESCRIPTIVE METRICS =========================

message("\n[02] Описательные метрики...")

# ── 2.0 Подготовка (маппинг категорий) ───────────────────────────────────────
# ЗАЧЕМ: category_id различается у Пятёрочки (251C1xxxx) и Магнита (6xxxx/4xxxx).
# Поэтому кросс-цепочечное сравнение делается через category_name — человекочитаемое
# название, которое одинаково для обеих сетей.
# Категория 47161 — алкоголь Магнита, исключается из анализа (NA → filter уберёт строки).
# У Магнита "Консервы" (64199) — отдельная категория; у Пятёрочки она входит в "Бакалея".
category_mapping <- tribble(
  ~category_id, ~category_name,
  # Пятёрочка
  "251C12886",  "Овощи и фрукты",
  "251C12887",  "Молочный прилавок",
  "251C12888",  "Хлеб и выпечка",
  "251C12889",  "Мясо и птица",        # колбасы тоже входят у обеих сетей
  "251C12890",  "Рыба и морепродукты",
  "251C12902",  "Бакалея",             # у Пятёрочки включает консервы
  "251C12904",  "Вода и напитки",
  # Магнит
  "63963",      "Молочный прилавок",
  "63905",      "Овощи и фрукты",
  "65001",      "Хлеб и выпечка",
  "64121",      "Бакалея",             # у Магнита консервы вынесены отдельно
  "64199",      "Консервы",            # нет аналога у Пятёрочки — Magnit-only
  "64243",      "Мясо и птица",        # колбасы тоже входят
  "4998",       "Рыба и морепродукты",
  "63791",      "Вода и напитки",
  "47161",      NA_character_          # алкоголь Магнита — исключаем из анализа
)

panel <- panel %>%
  select(-any_of("category_name")) %>%                        # сброс при повторном запуске
  left_join(category_mapping, by = "category_id") %>%
  filter(!is.na(category_name)) %>%                           # убираем алкоголь (47161 → NA)
  mutate(category_name = coalesce(category_name, paste("Неизвестная категория", category_id)))

# Диагностика: показываем неизвестные категории если есть
unknown_cats <- panel %>%
  filter(str_starts(category_name, "Неизвестная")) %>%
  distinct(store_chain, category_id, category_name)
if (nrow(unknown_cats) > 0) {
  message("Внимание: найдены категории без маппинга:")
  print(unknown_cats)
}

# ── 2.1 Метрики липкости по сети + категории ─────────────────────────────────
# ЧТО ТЕСТИРУЕМ: основная описательная статистика ценовой липкости.
# Гипотеза: у Пятёрочки и Магнита разная степень липкости цен (freq_effective,
# avg_spell_length) и разная роль промо (promo_share, avg_promo_depth).
# Вывод — таблицы 01 и 02 в output/tables/.
stickiness <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  group_by(store_chain, category_id, category_name) %>%
  summarise(
    n_obs                = n(),
    n_products           = n_distinct(product_id),

    freq_regular         = mean(changed_regular, na.rm = TRUE),
    avg_change_regular   = mean(abs(delta_regular_pct[changed_regular]), na.rm = TRUE),

    freq_effective       = mean(changed_effective, na.rm = TRUE),
    avg_change_effective = mean(abs(delta_effective_pct[changed_effective]), na.rm = TRUE),
    med_change_effective = median(abs(delta_effective_pct[changed_effective]), na.rm = TRUE),

    promo_share          = mean(is_promo, na.rm = TRUE),

    avg_promo_depth      = if_else(
      sum(is_promo, na.rm = TRUE) > 0,
      mean((price_regular[is_promo] - effective_price[is_promo]) /
             price_regular[is_promo], na.rm = TRUE),
      NA_real_
    ),

    avg_spell_length     = mean(spell_length_weeks, na.rm = TRUE),
    med_spell_length     = median(spell_length_weeks, na.rm = TRUE),

    volatility_effective = sd(delta_effective_pct, na.rm = TRUE),

    .groups = "drop"
  )

message("Метрики stickiness посчитаны")

# ── 2.2 Сводка по сетям ─────────────────────────────────────────────────────
# ЧТО ТЕСТИРУЕМ: агрегированное сравнение Пятёрочки и Магнита.
# Взвешивание по n_obs чтобы крупные категории влияли сильнее, чем маленькие.
# Ключевые переменные для сравнения: freq_effective (как часто меняется цена),
# avg_spell_length (сколько недель цена стоит на месте), promo_share (доля акций).
chain_summary <- stickiness %>%
  group_by(store_chain) %>%
  summarise(
    across(
      c(freq_regular, freq_effective, avg_change_regular, avg_change_effective,
        promo_share, avg_spell_length, volatility_effective),
      ~ weighted.mean(.x, w = n_obs, na.rm = TRUE)
    ),
    avg_promo_depth  = weighted.mean(avg_promo_depth, w = n_obs, na.rm = TRUE),
    med_spell_length = weighted.mean(med_spell_length, w = n_obs, na.rm = TRUE),
    n_categories = n(),
    n_products   = sum(n_products),
    .groups = "drop"
  )

# ── 2.3 Красивые gt-таблицы ────────────────────────────────────────────────
save_gt_table <- function(gt_obj, data_df, filename_stem) {
  write_csv(data_df, file.path(PATH_TABLES, paste0(filename_stem, ".csv")))
  gt_obj %>% gtsave(file.path(PATH_TABLES, paste0(filename_stem, ".html")))
  message(glue("Таблица сохранена: {filename_stem}.csv + .html"))
  print(gt_obj)
}

# Таблица 1: По сетям
gt_chain <- chain_summary %>%
  gt(rowname_col = "store_chain") %>%
  tab_header(
    title    = "Ценовая липкость: Пятёрочка vs Магнит",
    subtitle = "Взвешенное среднее по категориям (10 волн наблюдений)"
  ) %>%
  cols_label(
    store_chain          = "Сеть",
    freq_effective       = "Частота изменений",
    avg_change_effective = "Ср. размер изменения (%)",
    promo_share          = "Доля акций (%)",
    avg_promo_depth      = "Глубина акции (%)",
    avg_spell_length     = "Ср. длит. спелла (нед)",
    med_spell_length     = "Мед. длит. спелла (нед)",
    volatility_effective = "Волатильность",
    n_categories         = "Категорий",
    n_products           = "Товаров"
  ) %>%
  fmt_percent(
    columns  = c(freq_effective, avg_change_effective, promo_share,
                 avg_promo_depth, volatility_effective),
    decimals = 1
  ) %>%
  fmt_number(columns = c(avg_spell_length, med_spell_length), decimals = 1) %>%
  fmt_integer(columns = c(n_categories, n_products)) %>%
  tab_style(
    style     = cell_fill(color = "#E6F0FF"),
    locations = cells_body(rows = store_chain == "Pyaterochka")
  ) %>%
  tab_style(
    style     = cell_fill(color = "#FFF0F5"),
    locations = cells_body(rows = store_chain == "Magnit")
  ) %>%
  opt_stylize(style = 6)

save_gt_table(gt_chain, chain_summary, "01_chain_stickiness")

# Таблица 2: По категориям
gt_cat <- stickiness %>%
  arrange(store_chain, desc(freq_effective)) %>%
  gt(groupname_col = "store_chain") %>%
  tab_header(
    title    = "Липкость цен по категориям",
    subtitle = glue("Порог значимого изменения: {CHANGE_THRESHOLD * 100}%")
  ) %>%
  cols_label(
    category_name        = "Категория",
    n_obs                = "Наблюдений",
    n_products           = "Товаров",
    freq_effective       = "Частота ΔP",
    avg_change_effective = "Ср. ΔP (%)",
    promo_share          = "Доля акций",
    avg_spell_length     = "Ср. длит. спелла (нед)"
  ) %>%
  fmt_percent(c(freq_effective, avg_change_effective, promo_share), decimals = 1) %>%
  fmt_number(avg_spell_length, decimals = 1) %>%
  fmt_integer(c(n_obs, n_products)) %>%
  cols_hide(columns = c(category_id, freq_regular, avg_change_regular,
                        med_change_effective, avg_promo_depth,
                        med_spell_length, volatility_effective)) %>%
  opt_stylize(style = 6)

save_gt_table(gt_cat, stickiness, "02_category_stickiness")

# ── 2.4 Графики ─────────────────────────────────────────────────────────────
# 01_freq_by_category.png  — насколько "подвижны" цены в каждой категории
# 02_promo_vs_freq.png     — связаны ли акции и частота изменений? (если да — акции
#                            объясняют большую часть ценовой подвижности)
# 03_spell_distribution.png — как долго цены стоят на месте (форма распределения
#                            говорит о характере ценовых решений: меню-косты vs акции)
message("\n[02] Создаём визуализации...")

library(ggrepel)

# 1. Частота изменений по категориям
p_freq <- stickiness %>%
  ggplot(aes(x = reorder(category_name, freq_effective),
             y = freq_effective, fill = store_chain)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.8) +
  scale_fill_manual(values = CHAIN_COLOURS) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title    = "Частота изменения эффективной цены по категориям",
    subtitle = glue("Порог значимого изменения: {CHANGE_THRESHOLD * 100}% | Период: 10 недель"),
    x    = NULL,
    y    = "Доля недель с изменением цены",
    fill = "Сеть"
  ) +
  theme_price() +
  theme(legend.position = "bottom")

print(p_freq)
ggsave(file.path(PATH_PLOTS, "01_freq_by_category.png"), p_freq, width = 11, height = 8, dpi = 200)

# 2. Связь акций и частоты изменений
p_promo <- stickiness %>%
  ggplot(aes(x = promo_share, y = freq_effective,
             colour = store_chain, size = n_products, label = category_name)) +
  geom_point(alpha = 0.85) +
  ggrepel::geom_text_repel(size = 3.2, max.overlaps = 20, box.padding = 0.5) +
  scale_colour_manual(values = CHAIN_COLOURS) +
  scale_size_continuous(range = c(2, 6), name = "Кол-во товаров") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Доля акций и частота изменения цен",
    subtitle = "Размер точки = количество товаров в категории",
    x      = "Доля периодов с акцией",
    y      = "Частота изменения эффективной цены",
    colour = "Сеть"
  ) +
  theme_price()

print(p_promo)
ggsave(file.path(PATH_PLOTS, "02_promo_vs_freq.png"), p_promo, width = 10, height = 7, dpi = 200)

# 3. Распределение длительности ценовых спеллов
p_spell <- panel %>%
  filter(spell_length_weeks <= 25) %>%
  ggplot(aes(x = spell_length_weeks, fill = store_chain)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.75, color = "white") +
  scale_fill_manual(values = CHAIN_COLOURS) +
  facet_wrap(~ store_chain, scales = "free_y") +
  labs(
    title    = "Распределение длительности ценовых спеллов",
    subtitle = "Сколько недель цена остаётся без изменений",
    x    = "Длительность спелла (недель)",
    y    = "Количество случаев",
    fill = "Сеть"
  ) +
  theme_price()

print(p_spell)
ggsave(file.path(PATH_PLOTS, "03_spell_distribution.png"), p_spell, width = 10, height = 6, dpi = 200)

message("Все графики сохранены в output/plots/")

# ── Сохранение данных для следующих скриптов ─────────────────────────────────
saveRDS(stickiness, file.path(PATH_PROCESSED, "stickiness_metrics.rds"))
write_csv(stickiness, file.path(PATH_PROCESSED, "stickiness_metrics.csv"))

message("stickiness_metrics сохранён для дальнейшего анализа")
