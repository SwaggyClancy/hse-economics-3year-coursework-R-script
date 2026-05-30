# ================== 04. VARIANCE DECOMPOSITION ==================================

message("\n[04] Декомпозиция дисперсии...")

# ── 4.1 Общая дисперсия log-изменений эффективной цены ───────────────────────
# ЧТО ТЕСТИРУЕМ: что важнее объясняет изменчивость цен — принадлежность к сети,
# категория товара или конкретный магазин?
# Метод: последовательное добавление FE и сравнение adj. R².
# Инкрементальный R² (ΔR²) показывает "чистый" вклад каждого уровня сверх предыдущих.
# ВАЖНО: здесь используется category_id (не category_name) — декомпозиция внутри
# каждой сети отдельно, кросс-цепочечное сравнение тут не нужно.
# Модель: delta_eff ~ цепочка + категория + магазин + остаток

vd_data <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  # delta_eff = delta_effective_pct: относительное изменение ЭФФЕКТИВНОЙ цены
  # (то, что платит покупатель — акционная если is_promo=TRUE, иначе регулярная)
  mutate(delta_eff = delta_effective_pct)

# Регрессия только с индикаторами факторов
# Используем fixest::feols для скорости
model_vd <- feols(
  delta_eff ~ 1 | store_chain + category_id + store_code,
  data    = vd_data,
  cluster = ~store_code
)

# Доля дисперсии, объясняемой каждым набором FE
# Метод: последовательное добавление FE и сравнение R2

get_r2 <- function(formula_str, data) {
  m <- feols(as.formula(formula_str), data = data)
  r2(m, type = "ar2")
}

r2_chain    <- get_r2("delta_eff ~ 1 | store_chain",                          vd_data)
r2_cat      <- get_r2("delta_eff ~ 1 | category_id",                          vd_data)
r2_store    <- get_r2("delta_eff ~ 1 | store_code",                            vd_data)
r2_chain_cat<- get_r2("delta_eff ~ 1 | store_chain + category_id",            vd_data)
r2_full     <- get_r2("delta_eff ~ 1 | store_chain + category_id + store_code", vd_data)

vd_table <- tibble(
  Модель          = c(
    "Только сеть",
    "Только категория",
    "Только магазин",
    "Сеть + Категория",
    "Сеть + Категория + Магазин"
  ),
  R2_adj          = c(r2_chain, r2_cat, r2_store, r2_chain_cat, r2_full),
  `ΔR2_инкрем.`  = c(r2_chain, r2_cat, r2_store,
                      r2_chain_cat - r2_chain,
                      r2_full - r2_chain_cat)
)

gt_vd <- vd_table %>%
  gt() %>%
  tab_header(
    title    = "Декомпозиция дисперсии изменений эффективной цены",
    subtitle = "Последовательное добавление фиксированных эффектов"
  ) %>%
  fmt_percent(columns = c(R2_adj, `ΔR2_инкрем.`), decimals = 2) %>%
  tab_style(
    style     = cell_fill(color = "#e8f4e8"),
    locations = cells_body(rows = Модель == "Сеть + Категория + Магазин")
  ) %>%
  opt_stylize(style = 6)

save_gt_table(gt_vd, vd_table, "06_variance_decomposition")

# ── 4.2 График: вклады в дисперсию ────────────────────────────────────────────
p_vd <- vd_table %>%
  filter(Модель %in% c("Только сеть", "Только категория", "Только магазин")) %>%
  ggplot(aes(x = reorder(Модель, R2_adj), y = R2_adj, fill = Модель)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = percent(R2_adj, accuracy = 0.1)),
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Доля дисперсии, объясняемая каждым фактором (R² adj.)",
    x     = NULL,
    y     = "R² adj."
  )

ggsave(file.path(PATH_PLOTS, "11_variance_decomposition.png"),
       p_vd, width = 9, height = 5, dpi = 150)

message("  Декомпозиция завершена.")