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