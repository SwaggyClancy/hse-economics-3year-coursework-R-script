# ================== 02. DESCRIPTIVE METRICS =====================================

message("\n[02] Описательные метрики...")

# ── 2.1 Метрики липкости по сети + категории ──────────────────────────────────
# (используем только наблюдения с рассчитанными дельтами, т.е. не первое наблюдение)

stickiness <- panel %>%
  filter(!is.na(delta_regular_pct)) %>%  # убираем первые наблюдения в спелле
  group_by(store_chain, category_id) %>%
  summarise(
    # Число наблюдений
    n_obs = n(),

    # --- Regular price ---
    freq_regular       = mean(changed_regular, na.rm = TRUE),
    avg_size_regular   = mean(abs(delta_regular_pct[changed_regular]),   na.rm = TRUE),
    med_size_regular   = median(abs(delta_regular_pct[changed_regular]), na.rm = TRUE),

    # --- Effective price ---
    freq_effective     = mean(changed_effective, na.rm = TRUE),
    avg_size_effective = mean(abs(delta_effective_pct[changed_effective]),   na.rm = TRUE),
    med_size_effective = median(abs(delta_effective_pct[changed_effective]), na.rm = TRUE),

    # --- Акции ---
    promo_share        = mean(is_promo, na.rm = TRUE),

    # --- Длительность заморозки ---
    avg_spell_length   = mean(spell_length, na.rm = TRUE),
    med_spell_length   = median(spell_length, na.rm = TRUE),

    # --- Волатильность (стандартное отклонение log-изменений) ---
    volatility_regular   = sd(delta_regular_pct,   na.rm = TRUE),
    volatility_effective = sd(delta_effective_pct, na.rm = TRUE),

    .groups = "drop"
  )

# ── 2.2 Сводная таблица по сетям (агрегат по категориям) ──────────────────────
chain_summary <- stickiness %>%
  group_by(store_chain) %>%
  summarise(
    across(
      c(freq_regular, freq_effective, avg_size_regular, avg_size_effective,
        promo_share, avg_spell_length, volatility_regular, volatility_effective),
      ~ weighted.mean(.x, w = n_obs, na.rm = TRUE)
    ),
    n_categories = n(),
    .groups = "drop"
  )

# ── 2.3 gt-таблица: метрики по категориям ────────────────────────────────────

# Функция сохранения gt-таблицы в docx и csv
save_gt_table <- function(gt_obj, data_df, filename_stem) {
  # CSV
  write_csv(data_df, file.path(PATH_TABLES, paste0(filename_stem, ".csv")))

  # DOCX через officer
  tbl_html <- as_raw_html(gt_obj)
  doc <- read_docx() %>%
    body_add_par(filename_stem, style = "heading 1") %>%
    body_add_par("") %>%
    body_add_fpar(fpar(ftext(format(Sys.time(), "%Y-%m-%d %H:%M"))))
  # Сохраняем HTML-версию в docx (officer не поддерживает gt напрямую,
  # поэтому сохраняем как HTML-файл рядом)
  gt_obj %>%
    gtsave(filename = file.path(PATH_TABLES, paste0(filename_stem, ".html")))
  message(glue("  Таблица сохранена: {filename_stem}.csv / .html"))
}

# Таблица 1: Сводные метрики по сетям
gt_chain <- chain_summary %>%
  gt(rowname_col = "store_chain") %>%
  tab_header(
    title    = "Метрики ценовой липкости",
    subtitle = "Взвешенное среднее по категориям"
  ) %>%
  cols_label(
    freq_regular          = "Частота (рег.)",
    freq_effective        = "Частота (эфф.)",
    avg_size_regular      = "Ср. размер (рег.)",
    avg_size_effective    = "Ср. размер (эфф.)",
    promo_share           = "Доля акций",
    avg_spell_length      = "Длит. заморозки",
    volatility_regular    = "Волатильность (рег.)",
    volatility_effective  = "Волатильность (эфф.)",
    n_categories          = "Категорий"
  ) %>%
  fmt_percent(columns = c(freq_regular, freq_effective,
                           avg_size_regular, avg_size_effective,
                           promo_share,
                           volatility_regular, volatility_effective),
              decimals = 1) %>%
  fmt_number(columns = avg_spell_length, decimals = 1) %>%
  tab_style(
    style     = cell_fill(color = "#f9f0f0"),
    locations = cells_body(rows = store_chain == "Pyaterochka")
  ) %>%
  tab_style(
    style     = cell_fill(color = "#fff0f6"),
    locations = cells_body(rows = store_chain == "Magnit")
  ) %>%
  opt_stylize(style = 6)

save_gt_table(gt_chain, chain_summary, "01_chain_stickiness")

# Таблица 2: Детальная по категориям
gt_cat <- stickiness %>%
  arrange(store_chain, desc(freq_effective)) %>%
  gt(groupname_col = "store_chain") %>%
  tab_header(
    title    = "Метрики ценовой липкости по категориям",
    subtitle = glue("Порог изменения цены: {CHANGE_THRESHOLD * 100}%")
  ) %>%
  cols_label(
    category_id           = "Категория",
    n_obs                 = "Наблюд.",
    freq_regular          = "Частота (рег.)",
    freq_effective        = "Частота (эфф.)",
    avg_size_regular      = "Ср. ΔP (рег.)",
    avg_size_effective    = "Ср. ΔP (эфф.)",
    promo_share           = "Акции",
    avg_spell_length      = "Спелл (ср.)",
    volatility_effective  = "Волат."
  ) %>%
  fmt_percent(columns = c(freq_regular, freq_effective,
                           avg_size_regular, avg_size_effective,
                           promo_share, volatility_effective),
              decimals = 1) %>%
  fmt_number(columns = avg_spell_length, decimals = 1) %>%
  fmt_integer(columns = n_obs) %>%
  cols_hide(columns = c(med_size_regular, med_size_effective,
                         med_spell_length, volatility_regular)) %>%
  opt_stylize(style = 6) %>%
  tab_options(row_group.font.weight = "bold")

save_gt_table(gt_cat, stickiness, "02_category_stickiness")

# ── 2.4 Графики ──────────────────────────────────────────────────────────────

# График 1: Частота изменений по сетям
p_freq <- stickiness %>%
  ggplot(aes(x = reorder(category_id, freq_effective),
             y = freq_effective,
             fill = store_chain)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = CHAIN_COLOURS, name = "Сеть") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title    = "Частота изменения эффективной цены по категориям",
    subtitle = glue("Порог: {CHANGE_THRESHOLD * 100}% | Данные: Пятёрочка и Магнит"),
    x        = "Категория",
    y        = "Доля периодов с изменением цены"
  )

ggsave(file.path(PATH_PLOTS, "01_freq_by_category.png"),
       p_freq, width = 12, height = 8, dpi = 150)

# График 2: Доля акций vs частота изменений
p_promo <- stickiness %>%
  ggplot(aes(x = promo_share, y = freq_effective,
             colour = store_chain, label = category_id)) +
  geom_point(size = 3, alpha = 0.75) +
  ggrepel::geom_text_repel(size = 2.5, max.overlaps = 10) +
  scale_colour_manual(values = CHAIN_COLOURS, name = "Сеть") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Доля акций и частота изменений эффективной цены",
    x     = "Доля промо-периодов",
    y     = "Частота изменений (эффект. цена)"
  ) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.6)

# ggrepel опциональный — если нет, заменяем на geom_text
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p_promo <- p_promo + geom_text(size = 2.5, vjust = -0.5)
}

ggsave(file.path(PATH_PLOTS, "02_promo_vs_freq.png"),
       p_promo, width = 10, height = 7, dpi = 150)

# График 3: Распределение спеллов по сетям
p_spell <- panel %>%
  filter(!is.na(spell_length), spell_length <= 20) %>%
  ggplot(aes(x = spell_length, fill = store_chain)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = CHAIN_COLOURS, name = "Сеть") +
  facet_wrap(~store_chain, ncol = 2) +
  labs(
    title = "Распределение длительности ценового спелла",
    x     = "Число периодов без изменения",
    y     = "Число товаро-периодов"
  )

ggsave(file.path(PATH_PLOTS, "03_spell_distribution.png"),
       p_spell, width = 10, height = 5, dpi = 150)

message("  Метрики и графики сохранены.")

