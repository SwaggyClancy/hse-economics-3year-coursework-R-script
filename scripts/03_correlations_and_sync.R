# ================== 03. CORRELATIONS AND SYNCHRONICITY =========================

message("\n[03] Корреляции и синхронность...")

# ── 3.1 Корреляции между Пятёрочкой и Магнитом по категориям ─────────────────
# ЧТО ТЕСТИРУЕМ: синхронность ценовых решений между сетями.
# Гипотеза: если сети реагируют на одни и те же внешние шоки (сезонность, инфляция,
# общий спрос), корреляция частоты изменений между ними должна быть положительной.
# Высокая корреляция → внешние факторы доминируют; низкая → решения принимаются независимо.
# Сравниваем по category_name (не category_id!) — единственный общий ключ между сетями.

weekly_chg_rate <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  group_by(store_chain, category_name, week) %>%   # category_name — единый ключ для обеих сетей
  summarise(
    # chg_rate и avg_delta — по ЭФФЕКТИВНОЙ цене (то, что платит покупатель)
    chg_rate  = mean(changed_effective, na.rm = TRUE),    # доля товаров с изменением эффективной цены в эту неделю
    avg_delta = mean(delta_effective_pct, na.rm = TRUE),  # среднее изменение эффективной цены в эту неделю
    .groups = "drop"
  )

# Пивотируем: одна строка = неделя + категория (по имени!), столбцы = сети
weekly_wide <- weekly_chg_rate %>%
  pivot_wider(
    id_cols     = c(category_name, week),
    names_from  = store_chain,
    values_from = c(chg_rate, avg_delta),
    names_sep   = "_"
  )

# Корреляции по категориям (Pyaterochka vs Magnit)
cross_chain_cor <- weekly_wide %>%
  group_by(category_name) %>%
  summarise(
    cor_chg_rate  = cor(chg_rate_Pyaterochka,  chg_rate_Magnit,
                        use = "pairwise.complete.obs"),
    cor_avg_delta = cor(avg_delta_Pyaterochka, avg_delta_Magnit,
                        use = "pairwise.complete.obs"),
    n_weeks       = sum(!is.na(chg_rate_Pyaterochka) & !is.na(chg_rate_Magnit)),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(cor_chg_rate)))

# Таблица корреляций
gt_cors <- cross_chain_cor %>%
  gt() %>%
  tab_header(
    title    = "Синхронность изменений цен между сетями по категориям",
    subtitle = "Корреляция недельной частоты изменений: Пятёрочка vs Магнит"
  ) %>%
  cols_label(
    category_name = "Категория",
    cor_chg_rate  = "Корр. (частота)",
    cor_avg_delta = "Корр. (ср. Δ%)",
    n_weeks       = "Недель"
  ) %>%
  fmt_number(columns = c(cor_chg_rate, cor_avg_delta), decimals = 3) %>%
  data_color(
    columns = cor_chg_rate,
    fn      = scales::col_numeric(
      palette = c("#d73027", "#fee090", "#4575b4"),
      domain  = c(-1, 1)
    )
  ) %>%
  opt_stylize(style = 6)

save_gt_table(gt_cors, cross_chain_cor, "03_cross_chain_correlations")

# ── 3.2 Корреляции между категориями внутри одной сети ────────────────────────
# ЧТО ТЕСТИРУЕМ: централизованность ценовых решений внутри каждой сети.
# Если ценообразование централизовано (как у Пятёрочки), все категории меняют
# цены одновременно → высокие межкатегорийные корреляции и pct_pos ≈ 1.
# Если децентрализовано (как у Магнита), каждая категория/магазин принимает
# решения независимо → низкие корреляции и pct_pos ≈ 0.5.
# Вывод в inter_cat_summary: сравни median_cor и pct_pos между сетями.

compute_inter_cat_cor <- function(chain_name) {
  df <- weekly_chg_rate %>%
    filter(store_chain == chain_name) %>%
    select(category_name, week, chg_rate) %>%
    pivot_wider(names_from  = category_name,
                values_from = chg_rate)

  mat <- df %>%
    select(-week) %>%
    cor(use = "pairwise.complete.obs")

  as_tibble(mat, rownames = "cat_from") %>%
    pivot_longer(-cat_from, names_to = "cat_to", values_to = "correlation") %>%
    filter(cat_from < cat_to) %>%
    mutate(store_chain = chain_name)
}

inter_cat_cors <- bind_rows(
  compute_inter_cat_cor("Pyaterochka"),
  compute_inter_cat_cor("Magnit")
)

# ── 3.2b Комбинированные корреляции (обе сети вместе) ────────────────────────
# Усредняем chg_rate по сетям: одна строка на (категория × неделя).
# Показывает «общерыночную» синхронность вне зависимости от принадлежности к сети.
weekly_combined <- weekly_chg_rate %>%
  group_by(category_name, week) %>%
  summarise(chg_rate = mean(chg_rate, na.rm = TRUE), .groups = "drop")

inter_cat_comb_mat <- weekly_combined %>%
  pivot_wider(names_from = category_name, values_from = chg_rate) %>%
  select(-week) %>%
  cor(use = "pairwise.complete.obs")

inter_cat_combined_df <- as_tibble(inter_cat_comb_mat, rownames = "cat_from") %>%
  pivot_longer(-cat_from, names_to = "cat_to", values_to = "correlation") %>%
  filter(cat_from < cat_to) %>%
  mutate(store_chain = "Combined")

# ── Тепловые карты: исправленная функция ─────────────────────────────────────
# Цвета: красный = положительная, синий = отрицательная, белый = 0.
# Диагональ идёт сверху-слева вниз-вправо (стандартная матрица корреляций).
plot_cor_heatmap_df <- function(cors_df, title_label, subtitle_label = "") {
  cats    <- sort(unique(c(cors_df$cat_from, cors_df$cat_to)))
  df_up   <- cors_df %>% select(cat_from, cat_to, correlation)
  df_low  <- cors_df %>% select(cat_from = cat_to, cat_to = cat_from, correlation)
  df_diag <- tibble(cat_from = cats, cat_to = cats, correlation = 1)
  df_full <- bind_rows(df_up, df_low, df_diag) %>%
    distinct(cat_from, cat_to, .keep_all = TRUE)

  ggplot(df_full, aes(x = cat_from, y = cat_to, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(
      data = filter(df_full, cat_from != cat_to),
      aes(label  = sprintf("%.2f", correlation),
          colour = abs(correlation) > 0.45),
      size = 3.8, fontface = "bold"
    ) +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey25"),
                        guide = "none") +
    scale_fill_gradient2(
      low      = "#2166ac",   # синий: отрицательная корреляция
      mid      = "#f7f7f7",   # белый/серый: нулевая
      high     = "#b2182b",   # красный: положительная
      midpoint = 0, limits = c(-1, 1),
      name = "Коэффициент\nкорреляции"
    ) +
    scale_x_discrete(limits = cats, expand = c(0, 0)) +
    scale_y_discrete(limits = rev(cats), expand = c(0, 0)) +  # диагональ ↘
    coord_equal() +
    theme_price() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y     = element_text(size = 11),
      panel.grid      = element_blank(),
      legend.position = "right",
      legend.title    = element_text(size = 11, face = "bold"),
      legend.text     = element_text(size = 10)
    ) +
    labs(title = title_label, subtitle = subtitle_label, x = NULL, y = NULL)
}

p_cor_pya <- inter_cat_cors %>%
  filter(store_chain == "Pyaterochka") %>%
  plot_cor_heatmap_df(
    title_label    = "Межкатегорийные корреляции — Пятёрочка",
    subtitle_label = "По недельной частоте изменения эффективной цены | красный = совместное движение"
  )

p_cor_mag <- inter_cat_cors %>%
  filter(store_chain == "Magnit") %>%
  plot_cor_heatmap_df(
    title_label    = "Межкатегорийные корреляции — Магнит",
    subtitle_label = "По недельной частоте изменения эффективной цены | красный = совместное движение"
  )

p_cor_comb <- inter_cat_combined_df %>%
  plot_cor_heatmap_df(
    title_label    = "Межкатегорийные корреляции — обе сети (усреднено)",
    subtitle_label = "Усреднённая частота по Пятёрочке и Магниту | красный = совместное движение"
  )

ggsave(file.path(PATH_PLOTS, "08_intercategory_cor_pyaterochka.png"),
       p_cor_pya,  width = 11, height = 10, dpi = 180)
ggsave(file.path(PATH_PLOTS, "09_intercategory_cor_magnit.png"),
       p_cor_mag,  width = 11, height = 10, dpi = 180)
ggsave(file.path(PATH_PLOTS, "10_intercategory_cor_combined.png"),
       p_cor_comb, width = 11, height = 10, dpi = 180)

message("  Тепловые карты сохранены: 08_pya, 09_mag, 10_combined.")

# Сводная статистика межкатегорийных корреляций
inter_cat_summary <- inter_cat_cors %>%
  group_by(store_chain) %>%
  summarise(
    mean_cor   = mean(correlation,  na.rm = TRUE),
    median_cor = median(correlation, na.rm = TRUE),
    pct_pos    = mean(correlation > 0, na.rm = TRUE),
    .groups = "drop"
  )

message("  Межкатегорийные корреляции:")
print(inter_cat_summary)

write_excel_csv(inter_cat_cors,        file.path(PATH_TABLES, "04_intercategory_cors.csv"))
write_excel_csv(inter_cat_combined_df, file.path(PATH_TABLES, "04b_intercategory_cors_combined.csv"))
write_excel_csv(inter_cat_summary,     file.path(PATH_TABLES, "05_intercategory_summary.csv"))

message("=== Блок 03 завершён: корреляции, тепловые карты, сводка ===")


