# ==============================================================================
#  АНАЛИЗ ДИНАМИКИ ЦЕН НА ПРОДУКТОВОМ РЫНКЕ РФ
#  Пятёрочка vs Магнит — панельный анализ ценовой жёсткости
#
#  Авторы: Абдуазизов, Андреасян, Андреев
#  Версия: 2026-04
#
#  СТРУКТУРА СКРИПТА:
#    01. LOAD AND CLEAN          — загрузка, чистка, создание панели
#    02. DESCRIPTIVE METRICS     — метрики липкости, сводные таблицы
#    03. CORRELATIONS            — корреляции изменений между сетями и категориями
#    04. VARIANCE DECOMPOSITION  — декомпозиция дисперсии
#    05. CLUSTERING              — K-means кластеризация категорий
#    06. PANEL REGRESSION        — МНК с FE (fixest)
#    07. ROBUSTNESS CHECKS       — проверки устойчивости
# ==============================================================================

# ── Установка пакетов (раскомментировать при первом запуске) ──────────────────
# install.packages(c(
#   "tidyverse", "readxl", "gt", "gtExtras", "officer",
#   "fixest", "lmtest", "sandwich", "cluster", "factoextra",
#   "scales", "glue", "here", "rlang"
# ))

# ── Загрузка библиотек ────────────────────────────────────────────────────────
library(tidyverse)   # dplyr, ggplot2, tidyr, purrr и т.д.
library(readxl)      # чтение xlsx
library(gt)          # красивые таблицы
library(gtExtras)    # дополнения к gt
library(officer)     # экспорт в docx
library(fixest)      # панельные регрессии с FE (быстрее plm)
library(lmtest)      # тесты для регрессий
library(sandwich)    # робастные стандартные ошибки
library(cluster)     # кластеризация (silhouette)
library(factoextra)  # визуализация кластеров
library(scales)      # форматирование осей
library(glue)        # строковая интерполяция
library(here)        # относительные пути

# ── Глобальные параметры ──────────────────────────────────────────────────────
CHANGE_THRESHOLD   <- 0.01   # порог изменения цены (1%) для флага changed_*
SHEET_NAME         <- "Combined"
N_CLUSTERS         <- 5      # число кластеров K-means

# Пути (используем здесь from working directory)
PATH_RAW    <- here("data", "raw")
PATH_RDS    <- here("data", "processed")
PATH_TABLES <- here("output", "tables")
PATH_PLOTS  <- here("output", "plots")

# Создание директорий при необходимости
walk(c(PATH_RAW, PATH_RDS, PATH_TABLES, PATH_PLOTS), dir.create,
     showWarnings = FALSE, recursive = TRUE)

# ── Цветовая палитра для графиков ─────────────────────────────────────────────
CHAIN_COLOURS <- c("Pyaterochka" = "#E63329", "Magnit" = "#E91E8C")

# Тема для всех графиков
theme_price <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text    = element_text(face = "bold")
    )
}
theme_set(theme_price())


# ================== 01. LOAD AND CLEAN ==========================================

message("\n[01] Загрузка и очистка данных...")

# ── 1.1 Чтение всех xlsx-файлов из data/raw/ ──────────────────────────────────
xlsx_files <- list.files(
  path       = PATH_RAW,
  pattern    = "^combined_full_.*\\.xlsx$",
  full.names = TRUE
)

if (length(xlsx_files) == 0) {
  stop(glue(
    "Не найдено ни одного файла combined_full_*.xlsx в папке: {PATH_RAW}\n",
    "Убедитесь, что файлы лежат в data/raw/ относительно рабочей директории."
  ))
}

message(glue("  Найдено файлов: {length(xlsx_files)}"))
message(paste(" ", basename(xlsx_files), collapse = "\n"))

# Вспомогательная функция: читает один файл и добавляет имя источника
read_price_file <- function(path) {
  df <- read_excel(path, sheet = SHEET_NAME, col_types = "text")
  df$source_file <- basename(path)
  df
}

# Читаем все файлы и объединяем
raw <- map_dfr(xlsx_files, read_price_file)

message(glue("  Строк после объединения: {nrow(raw):,}"))

# ── 1.2 Приведение типов ──────────────────────────────────────────────────────

# Функция: заменяет запятую на точку, преобразует в numeric
parse_price <- function(x) {
  as.numeric(str_replace_all(x, ",", "."))
}

panel <- raw %>%
  # Базовая фильтрация
  filter(!is.na(store_chain), !is.na(product_id), !is.na(date)) %>%
  mutate(
    # Типы
    date           = as.Date(date),
    price_regular  = parse_price(price_regular),
    price_discount = parse_price(price_discount),
    category_id    = as.character(category_id),
    store_code     = as.character(store_code),
    product_id     = as.character(product_id),
    store_chain    = as.character(store_chain),

    # Эффективная цена: если есть скидка — берём её, иначе регулярную
    effective_price = if_else(!is.na(price_discount) & price_discount < price_regular,
                               price_discount, price_regular),

    # Флаг акции
    is_promo = (!is.na(price_discount) & price_discount < price_regular),

    # Неделя (для FE по времени)
    week = floor_date(date, "week", week_start = 1)
  ) %>%
  # Убираем строки с нулевыми или отрицательными ценами
  filter(price_regular > 0, effective_price > 0)

# ── 1.3 Лаговые переменные и процентные изменения ─────────────────────────────
# Сортировка: сначала по товару, потом по дате
# ВАЖНО: product_id уникален только внутри одной сети — не мэтчим между цепочками

panel <- panel %>%
  arrange(store_chain, store_code, product_id, date) %>%
  group_by(store_chain, store_code, product_id) %>%
  mutate(
    # Лаговые цены
    lag_regular   = lag(price_regular),
    lag_effective = lag(effective_price),

    # Процентные изменения (log-differencing — стандарт в литературе)
    delta_regular_pct   = (price_regular   - lag_regular)   / lag_regular,
    delta_effective_pct = (effective_price - lag_effective) / lag_effective,

    # Флаги изменений (порог задаётся CHANGE_THRESHOLD)
    changed_regular   = !is.na(delta_regular_pct)   & abs(delta_regular_pct)   > CHANGE_THRESHOLD,
    changed_effective = !is.na(delta_effective_pct) & abs(delta_effective_pct) > CHANGE_THRESHOLD,

    # Длительность "заморозки" цены: сколько периодов подряд цена НЕ менялась
    # TRUE = смена состояния (changed или первое наблюдение)
    price_change_event = changed_effective | is.na(lag_effective),
    spell_id = cumsum(price_change_event)  # каждый спелл = уникальный ценовой уровень
  ) %>%
  ungroup()

# ── 1.4 Длительность спелла (количество периодов без изменения) ───────────────
spell_lengths <- panel %>%
  group_by(store_chain, store_code, product_id, spell_id) %>%
  summarise(spell_length = n(), .groups = "drop")

panel <- panel %>%
  left_join(spell_lengths,
            by = c("store_chain", "store_code", "product_id", "spell_id"))

# ── 1.5 Сохранение готовой панели ─────────────────────────────────────────────
saveRDS(panel, file.path(PATH_RDS, "price_panel.rds"))
write_csv(panel %>% select(-lag_regular, -lag_effective),
          file.path(PATH_RDS, "price_panel.csv"))

message(glue(
  "  Панель сохранена: {nrow(panel):,} строк, ",
  "{n_distinct(panel$product_id)} уникальных товаров, ",
  "{n_distinct(panel$category_id)} категорий"
))


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


# ================== 04. VARIANCE DECOMPOSITION ==================================

message("\n[04] Декомпозиция дисперсии...")

# ── 4.1 Общая дисперсия log-изменений эффективной цены ───────────────────────
# Модель: delta_eff ~ цепочка + категория + магазин + остаток

vd_data <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
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

ggsave(file.path(PATH_PLOTS, "06_variance_decomposition.png"),
       p_vd, width = 9, height = 5, dpi = 150)

message("  Декомпозиция завершена.")


# ================== 05. CLUSTERING OF CATEGORIES ================================

message("\n[05] Кластеризация категорий...")

# ── 5.1 Матрица признаков для кластеризации ───────────────────────────────────
# Агрегируем по всем сетям вместе (чтобы не зависеть от coverage одной сети)

cluster_features <- stickiness %>%
  group_by(category_id) %>%
  summarise(
    freq_effective     = weighted.mean(freq_effective,     w = n_obs, na.rm = TRUE),
    avg_size_effective = weighted.mean(avg_size_effective, w = n_obs, na.rm = TRUE),
    promo_share        = weighted.mean(promo_share,        w = n_obs, na.rm = TRUE),
    volatility         = weighted.mean(volatility_effective, w = n_obs, na.rm = TRUE),
    avg_spell          = weighted.mean(avg_spell_length,   w = n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(complete.cases(.))  # только категории без NA во всех признаках

# Матрица: только числовые признаки, стандартизируем
feat_matrix <- cluster_features %>%
  select(freq_effective, avg_size_effective, promo_share, volatility, avg_spell) %>%
  scale()   # z-score стандартизация

rownames(feat_matrix) <- cluster_features$category_id

# ── 5.2 Выбор оптимального числа кластеров (silhouette) ──────────────────────
k_range <- 2:min(N_CLUSTERS + 2, nrow(feat_matrix) - 1)

silhouette_scores <- map_dbl(k_range, function(k) {
  km  <- kmeans(feat_matrix, centers = k, nstart = 25, iter.max = 100)
  sil <- silhouette(km$cluster, dist(feat_matrix))
  mean(sil[, 3])
})

best_k <- k_range[which.max(silhouette_scores)]
message(glue("  Оптимальное число кластеров по silhouette: {best_k}"))

# График silhouette
p_sil <- tibble(k = k_range, silhouette = silhouette_scores) %>%
  ggplot(aes(x = k, y = silhouette)) +
  geom_line(colour = "#2c7bb6", linewidth = 1.2) +
  geom_point(size = 3, colour = "#2c7bb6") +
  geom_vline(xintercept = best_k, linetype = "dashed", colour = "red") +
  labs(
    title = "Выбор числа кластеров: средняя ширина силуэта",
    x     = "Число кластеров k",
    y     = "Средний silhouette score"
  )

ggsave(file.path(PATH_PLOTS, "07_silhouette_scores.png"),
       p_sil, width = 7, height = 5, dpi = 150)

# ── 5.3 Финальная кластеризация ───────────────────────────────────────────────
set.seed(42)
km_final <- kmeans(feat_matrix, centers = best_k, nstart = 50, iter.max = 200)

cluster_features <- cluster_features %>%
  mutate(cluster = as.factor(km_final$cluster))

# Центроиды кластеров (в исходных единицах)
cluster_centroids <- cluster_features %>%
  group_by(cluster) %>%
  summarise(
    n_cat              = n(),
    freq_effective     = mean(freq_effective),
    avg_size_effective = mean(avg_size_effective),
    promo_share        = mean(promo_share),
    volatility         = mean(volatility),
    avg_spell          = mean(avg_spell),
    categories         = paste(category_id, collapse = ", "),
    .groups = "drop"
  )

# Таблица кластеров
gt_clusters <- cluster_centroids %>%
  select(-categories) %>%
  gt(rowname_col = "cluster") %>%
  tab_header(
    title    = glue("Кластеры категорий (K={best_k}, K-means)"),
    subtitle = "Центроиды кластеров по метрикам ценовой динамики"
  ) %>%
  cols_label(
    n_cat              = "Категорий",
    freq_effective     = "Частота изменений",
    avg_size_effective = "Ср. размер ΔP",
    promo_share        = "Доля акций",
    volatility         = "Волатильность",
    avg_spell          = "Ср. длина спелла"
  ) %>%
  fmt_percent(columns = c(freq_effective, avg_size_effective,
                           promo_share, volatility),
              decimals = 1) %>%
  fmt_number(columns = avg_spell, decimals = 1) %>%
  fmt_integer(columns = n_cat) %>%
  opt_stylize(style = 6)

save_gt_table(gt_clusters, cluster_centroids, "07_cluster_centroids")

# Таблица: какие категории в каких кластерах
write_csv(cluster_features %>% select(category_id, cluster, everything()),
          file.path(PATH_TABLES, "08_category_cluster_assignment.csv"))

# ── 5.4 Визуализация кластеров (PCA-биплот) ───────────────────────────────────
p_clusters <- fviz_cluster(
  km_final,
  data          = feat_matrix,
  palette       = "Set2",
  geom          = c("point", "text"),
  ellipse.type  = "convex",
  repel         = TRUE,
  ggtheme       = theme_price(),
  main          = glue("PCA-проекция кластеров категорий (K={best_k})")
)

ggsave(file.path(PATH_PLOTS, "08_cluster_pca.png"),
       p_clusters, width = 10, height = 7, dpi = 150)

message("  Кластеризация завершена.")


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


# ================== 07. ROBUSTNESS CHECKS =======================================

message("\n[07] Проверки устойчивости...")

# ── 7.1 Robustness 1: Только регулярные цены (без промо) ──────────────────────
message("  [RC-1] Только регулярные цены...")

reg_data_regular <- panel %>%
  filter(!is.na(delta_regular_pct)) %>%
  mutate(
    dln_regular = log(price_regular / lag_regular),
    is_magnit   = as.integer(store_chain == "Magnit"),
    week_fct    = factor(week),
    cat_fct     = factor(category_id),
    store_fct   = factor(store_code)
  ) %>%
  filter(is.finite(dln_regular))

m_rc1 <- feols(
  dln_regular ~ is_magnit | cat_fct + week_fct,
  data    = reg_data_regular,
  cluster = ~store_fct
)

# ── 7.2 Robustness 2: Другой порог изменения (0.5%) ───────────────────────────
message("  [RC-2] Порог изменения 0.5%...")

THRESHOLD_RC2 <- 0.005

reg_data_rc2 <- panel %>%
  filter(!is.na(delta_effective_pct)) %>%
  mutate(
    dln_effective  = log(effective_price / lag_effective),
    is_magnit      = as.integer(store_chain == "Magnit"),
    is_promo_i     = as.integer(is_promo),
    changed_eff_rc2 = abs(delta_effective_pct) > THRESHOLD_RC2,
    week_fct       = factor(week),
    cat_fct        = factor(category_id),
    store_fct      = factor(store_code)
  ) %>%
  filter(is.finite(dln_effective), changed_eff_rc2)   # только периоды с изменением

m_rc2 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data_rc2,
  cluster = ~store_fct
)

# ── 7.3 Robustness 3: Исключение выбросов (|ΔP| > 50%) ───────────────────────
message("  [RC-3] Без выбросов (|ΔP| > 50%)...")

m_rc3 <- feols(
  dln_effective ~ is_magnit + is_promo_i | cat_fct + week_fct,
  data    = reg_data %>% filter(abs(dln_effective) <= 0.5),
  cluster = ~store_fct
)

# ── 7.4 Сводная таблица robustness ────────────────────────────────────────────
# Сравниваем основную модель M1 и все RC
# tidy_feols() определена выше, в секции 06
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

write_csv(rc_coefs, file.path(PATH_TABLES, "11_robustness_coefs.csv"))

# График robustness: коэффициент при Magnit
p_rc <- rc_coefs %>%
  mutate(
    ci_lo   = estimate - 1.96 * std_error,
    ci_hi   = estimate + 1.96 * std_error,
    sig     = p_value < 0.05,
    term_ru = if_else(term == "is_magnit", "Magnit (β)", "Промо (β)")
  ) %>%
  ggplot(aes(x = estimate, y = model, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.7) +
  scale_colour_manual(
    values = c("TRUE" = "#d73027", "FALSE" = "grey60"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = NULL
  ) +
  facet_wrap(~term_ru, scales = "free_x") +
  labs(
    title    = "Проверки устойчивости: коэффициенты основных регрессоров",
    subtitle = "Сравнение оценок β (±95% ДИ) по спецификациям",
    x        = "Оценка β",
    y        = NULL
  )

ggsave(file.path(PATH_PLOTS, "10_robustness_coef_plot.png"),
       p_rc, width = 12, height = 5, dpi = 150)

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


# ================== ФИНАЛЬНОЕ РЕЗЮМЕ ============================================

message("\n", strrep("=", 60))
message("  АНАЛИЗ ЗАВЕРШЁН")
message(strrep("=", 60))
message(glue("
  Данные:
    Файлов загружено      : {length(xlsx_files)}
    Строк в панели        : {nrow(panel):,}
    Уникальных товаров    : {n_distinct(panel$product_id):,}
    Категорий             : {n_distinct(panel$category_id)}
    Сетей                 : {n_distinct(panel$store_chain)}
    Диапазон дат          : {min(panel$date)} — {max(panel$date)}

  Сохранённые объекты:
    data/processed/price_panel.rds      — готовая панель
    output/tables/                      — gt-таблицы (html + csv)
    output/plots/                       — графики (png)

  Ключевые модели:
    M1  — Δln(P_eff) ~ Magnit + Promo | FE(cat+week)
    M2  — Δln(P_eff) ~ Magnit + Promo | FE(cat+week+store)
    M3  — + взаимодействие Magnit×Promo
    RC1 — только регулярные цены
    RC2 — порог изменения {THRESHOLD_RC2*100}%
    RC3 — без выбросов |ΔP| > 50%
"))

message(strrep("=", 60))
