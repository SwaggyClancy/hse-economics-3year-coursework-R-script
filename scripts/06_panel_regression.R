# ================== 06. PANEL REGRESSION ========================================

message("\n[06] Панельная регрессия...")

# ── 6.1 Подготовка данных для регрессии ───────────────────────────────────────
# КЛЮЧЕВОЙ МОМЕНТ идентификации: если использовать factor(category_id) как FE,
# каждый category_id принадлежит ровно одной сети → is_magnit поглощается FE
# и стандартная ошибка не считается (perfect collinearity).
# Решение: factor(cat_group) — категории по имени, которые есть в обеих сетях
# (Молочный прилавок, Бакалея и т.д.) → is_magnit остаётся идентифицируемым.

# Кросс-цепочечный маппинг категорий (самодостаточно, не зависит от script 02)
cat_group_map <- tribble(
  ~category_id,  ~cat_group,
  "251C12886",   "Овощи и фрукты",
  "251C12887",   "Молочный прилавок",
  "251C12888",   "Хлеб и выпечка",
  "251C12889",   "Мясо и птица",
  "251C12890",   "Рыба и морепродукты",
  "251C12902",   "Бакалея",
  "251C12904",   "Вода и напитки",
  "63905",       "Овощи и фрукты",
  "63963",       "Молочный прилавок",
  "65001",       "Хлеб и выпечка",
  "64121",       "Бакалея",
  "64199",       "Консервы",
  "64243",       "Мясо и птица",
  "4998",        "Рыба и морепродукты",
  "63791",       "Вода и напитки"
)

reg_data <- panel %>%
  left_join(cat_group_map, by = "category_id") %>%
  filter(!is.na(delta_effective_pct)) %>%
  mutate(
    dln_effective = log(effective_price / lag_effective),
    is_magnit  = as.integer(store_chain == "Magnit"),
    is_promo_i = as.integer(is_promo),
    week_fct   = factor(week),
    cat_fct    = factor(cat_group),   # кросс-цепочечная группа → is_magnit идентифицируем
    store_fct  = factor(store_code),
    chain_fct  = factor(store_chain)
  ) %>%
  filter(is.finite(dln_effective), !is.na(cat_fct))

# ── 6.2 Основная спецификация: FE по категории + неделе ───────────────────────
# ЧТО ТЕСТИРУЕМ (M1): есть ли систематическая разница в динамике цен между сетями?
# β1 (is_magnit): на сколько % в неделю Магнит меняет цены иначе, чем Пятёрочка,
#   после контроля на категорию и период.
# β2 (is_promo_i): как акция влияет на изменение эффективной цены.
# FE по категории контролируют товарную корзину; FE по неделе — общие шоки цен.
# Кластерные SE по магазину — чтобы учесть корреляцию ошибок внутри магазина.
# Δln(P_eff) = β1*Magnit + β2*Promo + α_category + α_week + ε

m1 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# ── 6.3 Расширенная спецификация: тройные FE ──────────────────────────────────
# ЧТО ТЕСТИРУЕМ (M2): устойчив ли эффект промо при полном контроле на магазин?
# store FE поглощает chain-эффект → is_magnit не включаем (коллинеарен store_fct).
# Если β(promo) в M2 ≈ β(promo) в M1 — результат устойчив к магазинной гетерогенности.
# Δln(P_eff) = β2*Promo | category + week + store
m2 <- feols(
  dln_effective ~ is_promo_i | cat_fct + week_fct + store_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# ── 6.4 Эффект промо отдельно по сетям ────────────────────────────────────────
# ЧТО ТЕСТИРУЕМ (M3): одинаков ли эффект промо у Пятёрочки и Магнита?
# Сравниваем β(promo) из m3_pya (только Пятёрочка) с β из M1 (pooled, включает Магнит).
# Взаимодействие is_magnit:is_promo_i коллинеарно из-за Консервы (Magnit-only) →
# отдельные регрессии вместо одной с взаимодействием.
# m3_mag неидентифицируема: у Магнита is_promo_i константен внутри каждой
# (category×week) — промо синхронизированы на уровне всей сети.
m3_pya <- feols(
  dln_effective ~ is_promo_i | cat_fct + week_fct,
  data    = filter(reg_data, store_chain == "Pyaterochka"),
  cluster = ~store_fct
)

# m3_mag невозможна: у Магнита is_promo_i константен внутри каждой (категория × неделя) —
# акции централизованы на уровне сети. Эффект промо для Магнита берём из M1 (pooled).
message("  Примечание: m3_mag не идентифицируется — промо Магнита синхронизировано по сети.")

# ── 6.5 Вспомогательная функция: tidy для feols ──────────────────────────────
tidy_feols <- function(model) {
  coef_df <- coeftable(model)
  tibble(
    term      = rownames(coef_df),
    estimate  = coef_df[, "Estimate"],
    std_error = coef_df[, "Std. Error"],
    t_stat    = coef_df[, "t value"],
    p_value   = coef_df[, "Pr(>|t|)"]
  )
}

# ── 6.6 LaTeX + CSV результатов ────────────────────────────────────────────────────────────────────────────────────
# etable — встроенная функция fixest для красивого вывода в LaTeX
etable(m1, m2, m3_pya,
       digits      = 4,
       se.below    = TRUE,
       depvar      = FALSE,
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       file        = file.path(PATH_TABLES, "09_regression_results.tex"),
       style.tex   = style.tex("base")
)

# CSV-версия коэффициентов
reg_coefs <- bind_rows(
  tidy_feols(m1)     %>% mutate(model = "M1: Pooled FE(cat+week)"),
  tidy_feols(m2)     %>% mutate(model = "M2: FE(cat+week+store)"),
  tidy_feols(m3_pya) %>% mutate(model = "M3: Pyaterochka FE(cat+week)")
)

write_csv(reg_coefs, file.path(PATH_TABLES, "09_regression_coefs.csv"))

# ── 6.7 Визуализация: coef-plot ───────────────────────────────────────────────
p_coef <- reg_coefs %>%
  filter(term %in% c("is_magnit", "is_promo_i")) %>%
  mutate(
    ci_lo   = estimate - 1.96 * std_error,
    ci_hi   = estimate + 1.96 * std_error,
    sig     = p_value < 0.05,
    term_ru = case_when(
      term == "is_magnit"  ~ "Магнит (vs Пятёрочка)",
      term == "is_promo_i" ~ "Промо-акция"
    )
  ) %>%
  ggplot(aes(x = estimate, y = term_ru, colour = sig, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi),
                  position = position_dodge(width = 0.5),
                  size = 0.6) +
  scale_colour_manual(values = c("TRUE" = "#d73027", "FALSE" = "grey60"),
                      labels  = c("TRUE" = "Значимо (p<0.05)", "FALSE" = "Незначимо"),
                      name    = NULL) +
  scale_shape_discrete(name = "Спецификация") +
  labs(
    title = "Коэффициентный граф: панельные регрессии",
    x     = "Оценка β (±95% ДИ)",
    y     = NULL
  )

ggsave(file.path(PATH_PLOTS, "09_coef_plot.png"),
       p_coef, width = 10, height = 6, dpi = 150)

# ── 6.8 Goodness-of-fit ──────────────────────────────────────────────────────
gof_table <- tibble(
  Спецификация = c("M1", "M2", "M3 (Pya)"),
  N            = c(nobs(m1), nobs(m2), nobs(m3_pya)),
  R2_within    = c(r2(m1, "r2"),  r2(m2, "r2"),  r2(m3_pya, "r2")),
  R2_adj       = c(r2(m1, "ar2"), r2(m2, "ar2"), r2(m3_pya, "ar2"))
)

gt_gof <- gof_table %>%
  gt() %>%
  tab_header(title = "Качество подгонки панельных моделей") %>%
  fmt_integer(columns = N) %>%
  fmt_percent(columns = c(R2_within, R2_adj), decimals = 2) %>%
  opt_stylize(style = 6)

save_gt_table(gt_gof, gof_table, "10_regression_gof")

message("  Панельные регрессии завершены.")