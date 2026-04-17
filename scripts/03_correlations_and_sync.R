# ================== 03. CORRELATIONS AND SYNCHRONICITY =========================

message("\n[03] Корреляции и синхронность...")

# ── 3.1 Корреляции между Пятёрочкой и Магнитом по категориям ─────────────────
# Идея: по каждой категории берём долю изменений за неделю для каждой сети
# и смотрим, коррелируют ли они

weekly_chg_rate <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  group_by(store_chain, category_id, week) %>%
  summarise(
    chg_rate = mean(changed_effective, na.rm = TRUE),
    avg_delta = mean(delta_effective_pct, na.rm = TRUE),
    .groups = "drop"
  )

# Пивотируем: одна строка = неделя + категория, столбцы = сети
weekly_wide <- weekly_chg_rate %>%
  pivot_wider(
    id_cols     = c(category_id, week),
    names_from  = store_chain,
    values_from = c(chg_rate, avg_delta),
    names_sep   = "_"
  )

# Корреляции по категориям (Pyaterochka vs Magnit)
cross_chain_cor <- weekly_wide %>%
  group_by(category_id) %>%
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
    category_id   = "Категория",
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

compute_inter_cat_cor <- function(chain_name) {
  df <- weekly_chg_rate %>%
    filter(store_chain == chain_name) %>%
    select(category_id, week, chg_rate) %>%
    pivot_wider(names_from  = category_id,
                values_from = chg_rate)

  mat <- df %>%
    select(-week) %>%
    cor(use = "pairwise.complete.obs")

  as_tibble(mat, rownames = "cat_from") %>%
    pivot_longer(-cat_from, names_to = "cat_to", values_to = "correlation") %>%
    filter(cat_from < cat_to) %>%   # только верхний треугольник
    mutate(store_chain = chain_name)
}

inter_cat_cors <- bind_rows(
  compute_inter_cat_cor("Pyaterochka"),
  compute_inter_cat_cor("Magnit")
)

# Тепловые карты корреляций по сетям
plot_cor_heatmap <- function(chain_name) {
  df <- inter_cat_cors %>%
    filter(store_chain == chain_name) %>%
    bind_rows(
      inter_cat_cors %>%
        filter(store_chain == chain_name) %>%
        rename(cat_from = cat_to, cat_to = cat_from)   # симметрия
    ) %>%
    bind_rows(
      tibble(cat_from = unique(.$cat_from),
             cat_to   = unique(.$cat_from),
             correlation = 1,
             store_chain = chain_name)
    )

  ggplot(df, aes(x = cat_from, y = cat_to, fill = correlation)) +
    geom_tile(colour = "white") +
    scale_fill_gradient2(
      low  = "#d73027", mid = "#ffffbf", high = "#4575b4",
      midpoint = 0, limits = c(-1, 1),
      name = "Корр."
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = glue("Корреляция частоты изменений между категориями — {chain_name}"),
      x = NULL, y = NULL
    )
}

p_cor_pya <- plot_cor_heatmap("Pyaterochka")
p_cor_mag <- plot_cor_heatmap("Magnit")

ggsave(file.path(PATH_PLOTS, "04_intercategory_cor_pyaterochka.png"),
       p_cor_pya, width = 10, height = 9, dpi = 150)
ggsave(file.path(PATH_PLOTS, "05_intercategory_cor_magnit.png"),
       p_cor_mag, width = 10, height = 9, dpi = 150)

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

write_csv(inter_cat_cors,    file.path(PATH_TABLES, "04_intercategory_cors.csv"))
write_csv(inter_cat_summary, file.path(PATH_TABLES, "05_intercategory_summary.csv"))

