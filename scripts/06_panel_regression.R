# ================== 06. PANEL REGRESSION ========================================

message("\n[06] Панельная регрессия...")

# ── 6.1 Подготовка данных для регрессии ───────────────────────────────────────
reg_data <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  mutate(
    # Зависимая переменная: log-изменение эффективной цены
    # (delta_effective_pct уже = (P_t - P_{t-1}) / P_{t-1} ≈ Δln P для малых изм.)
    dln_effective = log(effective_price / lag_effective),

    # Регрессоры
    is_magnit  = as.integer(store_chain == "Magnit"),
    is_promo_i = as.integer(is_promo),
    week_fct   = factor(week),
    cat_fct    = factor(category_id),
    store_fct  = factor(store_code),
    chain_fct  = factor(store_chain)
  ) %>%
  filter(is.finite(dln_effective))  # убираем Inf/-Inf (деление на 0)

# ── 6.2 Основная спецификация: FE по категории + неделе ───────────────────────
# Δln(P_eff) = β1*Magnit + β2*Promo + α_category + α_week + ε
# Кластерные SE по магазину

m1 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# ── 6.3 Расширенная спецификация: тройные FE ──────────────────────────────────
# Δln(P_eff) = β1*Magnit + β2*Promo | category + week + store
m2 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct + store_fct,
  data    = reg_data,
  cluster = ~store_fct
)

# ── 6.4 Взаимодействие: Magnit × Promo ────────────────────────────────────────
m3 <- feols(
  dln_effective ~ is_magnit * is_promo_i | cat_fct + week_fct,
  data    = reg_data,
  cluster = ~store_fct
)

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
etable(m1, m2, m3,
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
  tidy_feols(m1) %>% mutate(model = "M1: FE(cat+week)"),
  tidy_feols(m2) %>% mutate(model = "M2: FE(cat+week+store)"),
  tidy_feols(m3) %>% mutate(model = "M3: FE(cat+week)+interaction")
)

write_csv(reg_coefs, file.path(PATH_TABLES, "09_regression_coefs.csv"))

# ── 6.7 Визуализация: coef-plot ───────────────────────────────────────────────
p_coef <- reg_coefs %>%
  filter(term %in% c("is_magnit", "is_promo_i", "is_magnit:is_promo_i")) %>%
  mutate(
    ci_lo   = estimate - 1.96 * std_error,
    ci_hi   = estimate + 1.96 * std_error,
    sig     = p_value < 0.05,
    term_ru = case_when(
      term == "is_magnit"              ~ "Магнит (vs Пятёрочка)",
      term == "is_promo_i"             ~ "Промо-акция",
      term == "is_magnit:is_promo_i"   ~ "Магнит × Промо"
    )
  ) %>%
  ggplot(aes(x = estimate, y = term_ru, colour = sig, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi),
                  position = position_dodge(width = 0.4),
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
  Спецификация = c("M1", "M2", "M3"),
  N            = c(nobs(m1), nobs(m2), nobs(m3)),
  R2_within    = c(r2(m1, "r2"), r2(m2, "r2"), r2(m3, "r2")),
  R2_adj       = c(r2(m1, "ar2"), r2(m2, "ar2"), r2(m3, "ar2"))
)

gt_gof <- gof_table %>%
  gt() %>%
  tab_header(title = "Качество подгонки панельных моделей") %>%
  fmt_integer(columns = N) %>%
  fmt_percent(columns = c(R2_within, R2_adj), decimals = 2) %>%
  opt_stylize(style = 6)

save_gt_table(gt_gof, gof_table, "10_regression_gof")

message("  Панельные регрессии завершены.")