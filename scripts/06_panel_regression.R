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
    # Зависимая переменная во всех основных спецификациях:
    # dln_effective = ln(P_eff_t / P_eff_{t-1}) — логарифмическое изменение
    # ЭФФЕКТИВНОЙ цены (то, что платит покупатель).
    # При is_promo=TRUE: P_eff = price_discount (акционная).
    # При is_promo=FALSE: P_eff = price_regular (регулярная).
    dln_effective = log(effective_price / lag_effective),
    is_magnit  = as.integer(store_chain == "Magnit"),
    # is_promo_i = 1 если в данный период действует акция (price_discount < price_regular)
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

write_excel_csv(reg_coefs, file.path(PATH_TABLES, "09_regression_coefs.csv"))

# ── HTML-таблица с подробными описаниями ─────────────────────────────────────
stars_fn <- function(pv) case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**",
                                    pv < 0.10 ~ "*",   TRUE ~ "")

reg_wide <- reg_coefs %>%
  mutate(
    term_ru     = case_when(
      term == "is_magnit"  ~ "β(Магнит) — принадлежность к Магниту",
      term == "is_promo_i" ~ "β(Промо) — активная акция",
      TRUE                 ~ term
    ),
    model_short = case_when(
      grepl("M1", model)                     ~ "M1: Pooled",
      grepl("M2", model)                     ~ "M2: +Store FE",
      grepl("M3|Pyaterochka", model)         ~ "M3: Pya only",
      TRUE                                   ~ model
    ),
    coef_str = glue("{format(round(estimate, 4), nsmall=4)}{stars_fn(p_value)}")
  ) %>%
  select(term_ru, model_short, coef_str) %>%
  pivot_wider(names_from = model_short, values_from = coef_str, values_fill = "—")

reg_se <- reg_coefs %>%
  mutate(
    term_ru     = case_when(
      term == "is_magnit"  ~ "β(Магнит) SE",
      term == "is_promo_i" ~ "β(Промо) SE",
      TRUE                 ~ paste(term, "SE")
    ),
    model_short = case_when(
      grepl("M1", model)             ~ "M1: Pooled",
      grepl("M2", model)             ~ "M2: +Store FE",
      grepl("M3|Pyaterochka", model) ~ "M3: Pya only",
      TRUE                           ~ model
    ),
    se_str = glue("({format(round(std_error, 4), nsmall=4)})")
  ) %>%
  select(term_ru, model_short, se_str) %>%
  pivot_wider(names_from = model_short, values_from = se_str, values_fill = "")

gof_wide <- tibble(
  `term_ru`      = c("N наблюдений", "R² within", "R² adj."),
  `M1: Pooled`   = c(as.character(nobs(m1)),
                      format(round(r2(m1,"r2"),4),  nsmall=4),
                      format(round(r2(m1,"ar2"),4), nsmall=4)),
  `M2: +Store FE`= c(as.character(nobs(m2)),
                      format(round(r2(m2,"r2"),4),  nsmall=4),
                      format(round(r2(m2,"ar2"),4), nsmall=4)),
  `M3: Pya only` = c(as.character(nobs(m3_pya)),
                      format(round(r2(m3_pya,"r2"),4),  nsmall=4),
                      format(round(r2(m3_pya,"ar2"),4), nsmall=4))
)

reg_display <- bind_rows(reg_wide, reg_se, gof_wide)

gt_reg_html <- reg_display %>%
  gt(rowname_col = "term_ru") %>%
  tab_header(
    title    = md("**Панельные регрессии:** Δln(эффективной цены)"),
    subtitle = "SE кластеризованы по магазину | В скобках — стандартные ошибки | * p<0.10, ** p<0.05, *** p<0.01"
  ) %>%
  tab_spanner(label = "Спецификация", columns = c(`M1: Pooled`, `M2: +Store FE`, `M3: Pya only`)) %>%
  tab_row_group(label = "Качество подгонки",   rows = term_ru %in% c("N наблюдений", "R² within", "R² adj.")) %>%
  tab_row_group(label = "SE (кластер. по магазину)", rows = grepl("SE$", term_ru)) %>%
  tab_row_group(label = "Коэффициенты",       rows = !grepl("SE$|наблюдений|within|adj", term_ru)) %>%
  tab_footnote(
    footnote = "is_magnit = 1 если магазин принадлежит Магниту (0 = Пятёрочка). В M2 is_magnit исключён — поглощается store FE. В M3 модель оценена только по Пятёрочке.",
    locations = cells_stub(rows = grepl("Магнит", term_ru))
  ) %>%
  tab_footnote(
    footnote = "is_promo_i = 1 если в данный период действует акция (price_discount < price_regular). Δln(P_eff) = ln(P_eff_t / P_eff_{t-1}).",
    locations = cells_stub(rows = grepl("Промо", term_ru))
  ) %>%
  tab_source_note(md("FE: фиксированные эффекты по **категории** (category_name, кросс-цепочечный) и **неделе** (+**магазину** в M2).")) %>%
  opt_stylize(style = 6)

gt_reg_html %>% gtsave(file.path(PATH_TABLES, "09_regression_coefs.html"))
message("  HTML-таблица регрессий сохранена: 09_regression_coefs.html")

# ── 6.7 Визуализация: отдельные coef-plot'ы ──────────────────────────────────
# Каждый коэффициент — отдельный файл: нет перекрытия интервалов.
# Вспомогательная функция для единообразных графиков.
make_coef_plot <- function(data, title_text, subtitle_text, xlab = "Оценка β (±95% ДИ)") {
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
    geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.8, linewidth = 1) +
    geom_text(aes(label = glue("{round(estimate, 4)}{stars}")),
              hjust = -0.15, size = 4, fontface = "bold") +
    scale_colour_manual(values = c("TRUE" = "#d73027", "FALSE" = "grey55"),
                        labels  = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name    = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(title    = title_text,
         subtitle = subtitle_text,
         x = xlab, y = NULL) +
    theme_price() +
    theme(legend.position = "bottom")
}

# 9a: β(Магнит) — только M1 (M2 и M3_pya не идентифицируют is_magnit)
reg_coefs %>%
  filter(term == "is_magnit") %>%
  mutate(label = "Магнит vs Пятёрочка") %>%
  make_coef_plot(
    title_text    = "β(Магнит): эффект принад-ти к сети на Δln(P_эфф)",
    subtitle_text = "M1: FE(категория + неделя) | SE кластеризованы по магазину"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "15_coef_magnit.png"), ., width = 9, height = 4, dpi = 180) }

# 9b: β(Промо) по всем трём спецификациям — три строки, читаемо
reg_coefs %>%
  filter(term == "is_promo_i") %>%
  mutate(label = model) %>%
  make_coef_plot(
    title_text    = "β(Промо): эффект акции на Δln(P_эфф) — срав-е спецификаций",
    subtitle_text = "Зависимая переменная: эффективная цена"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "16_coef_promo.png"), ., width = 9, height = 5, dpi = 180) }

# 9c: M1 полностью (оба коэффициента вместе — для отчёта)
reg_coefs %>%
  filter(grepl("M1", model), term %in% c("is_magnit", "is_promo_i")) %>%
  mutate(label = case_when(term == "is_magnit"  ~ "Магнит vs Пятёрочка",
                           term == "is_promo_i" ~ "Промо-акция")) %>%
  make_coef_plot(
    title_text    = "M1: все коэффициенты — Δln(эффективной цены)",
    subtitle_text = "FE(категория + неделя) | SE кластеризованы по магазину"
  ) %>%
  { ggsave(file.path(PATH_PLOTS, "14_coef_m1.png"), ., width = 9, height = 4, dpi = 180) }

message("  Coef-plots сохранены: 14_coef_m1.png, 15_coef_magnit.png, 16_coef_promo.png")

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