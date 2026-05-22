2# ==============================================================================
# АНАЛИЗ ДИНАМИКИ ЦЕН НА ПРОДУКТОВОМ РЫНКЕ РФ
# Пятёрочка vs Магнит — панельный анализ ценовой липкости
#
# Авторы: Абдуазизов, Андреасян, Андреев
# ==============================================================================

# ── Установка и загрузка библиотек ───────────────────────────────────────────
packages <- c(
  "tidyverse", "readxl", "gt", "gtExtras", "officer",
  "fixest", "lmtest", "sandwich", "cluster", "factoextra",
  "scales", "glue", "here", "ggrepel"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  message("Устанавливаем пакеты: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

invisible(lapply(packages, library, character.only = TRUE))

# ── Глобальные параметры ──────────────────────────────────────────────────────
CHANGE_THRESHOLD <- 0.01
SHEET_NAME       <- "Combined"
N_CLUSTERS       <- 4

PATH_RAW       <- file.path(getwd(), "data", "raw")
PATH_PROCESSED <- file.path(getwd(), "data", "processed")
PATH_TABLES    <- file.path(getwd(), "output", "tables")
PATH_PLOTS     <- file.path(getwd(), "output", "plots")

message("=== ПУТИ ПОСЛЕ ИСПРАВЛЕНИЯ ===")
message("Working directory : ", getwd())
message("PATH_RAW          : ", PATH_RAW)
message("PATH_PROCESSED    : ", PATH_PROCESSED)
message("PATH_TABLES       : ", PATH_TABLES)
message("PATH_PLOTS        : ", PATH_PLOTS)
message("Содержимое raw/:")
print(list.files(PATH_RAW, pattern = "\\.xlsx$", full.names = FALSE))

# ── Цветовая палитра и тема ───────────────────────────────────────────────────
CHAIN_COLOURS <- c("Pyaterochka" = "#E63329", "Magnit" = "#E91E8C")

theme_price <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}
theme_set(theme_price())

# ================== 01. LOAD AND CLEAN =========================================
message("\n[01] === ЗАПУСК ЗАГРУЗКИ И ОЧИСТКИ ДАННЫХ ===")

# ── 1.1 Поиск файлов ──────────────────────────────────────────────────────────
message("1.1 Поиск xlsx-файлов...")

xlsx_files <- list.files(
  path        = PATH_RAW,
  pattern     = "^combined_full_.*\\.xlsx$",
  full.names  = TRUE,
  ignore.case = TRUE
)

message(glue("   Найдено файлов: {length(xlsx_files)}"))

if (length(xlsx_files) == 0) {
  stop(glue("Файлы не найдены в {PATH_RAW}"))
} else {
  message(paste("   ->", basename(xlsx_files), collapse = "\n"))
}

# ── 1.2 Чтение всех файлов + унификация колонок ─────────────────────────────
message("1.2 Чтение Excel-файлов и унификация названий колонок...")

read_price_file <- function(path) {
  df <- read_excel(path, sheet = SHEET_NAME, col_types = "text")
  df$source_file <- basename(path)

  df <- df %>%
    rename_with(~ case_when(
      .x == "store_cha"   ~ "store_chain",
      .x == "store_cod"   ~ "store_code",
      .x == "price_regul" ~ "price_regular",
      .x == "price_reg"   ~ "price_regular",
      .x == "price_disc"  ~ "price_discount",
      .x == "price_dis"   ~ "price_discount",
      TRUE                ~ .x
    ), .cols = everything())

  if ("store_chain" %in% names(df)) {
    df <- df %>% mutate(store_chain = case_when(
      str_detect(store_chain, regex("пят|pyat|5ka|pyater", ignore_case = TRUE)) ~ "Pyaterochka",
      str_detect(store_chain, regex("магн|magnit",          ignore_case = TRUE)) ~ "Magnit",
      TRUE ~ store_chain
    ))
  }

  df
}

raw <- map_dfr(xlsx_files, read_price_file)

message(glue("Загружено строк: {nrow(raw)} | Колонок: {ncol(raw)}"))
message("Колонки после унификации:")
print(colnames(raw))

# ── 1.3 Приведение типов + ключевые переменные ───────────────────────────────
message("1.3 Приведение типов + создание effective_price и is_promo...")

parse_price <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)

  x_orig <- as.character(x)

  x <- x_orig %>%
    str_replace_all("[^0-9,.-]", "") %>%
    str_replace_all(",", ".") %>%
    str_trim()

  x <- ifelse(x == "" | str_detect(x, "^\\.$"), NA_character_, x)
  x <- ifelse(str_detect(x, "^[0-9]"), x, NA_character_)

  num <- as.numeric(x)

  bad <- which(is.na(num) & !is.na(x_orig))
  if (length(bad) > 0 && length(bad) < 30) {
    message("Проблемные значения price_regular (первые 20):")
    print(data.frame(
      original = x_orig[bad[1:min(20, length(bad))]],
      cleaned  = x[bad[1:min(20, length(bad))]]
    ))
  }

  num
}

message("Диагностика цен до парсинга (первые 30 regular):")
print(head(raw$price_regular, 30))

panel <- raw %>%
  filter(!is.na(store_chain), !is.na(product_id), !is.na(date)) %>%
  mutate(
    date           = as.Date(date),
    price_regular  = parse_price(price_regular),
    price_discount = parse_price(price_discount),
    across(c(category_id, store_code, product_id, store_chain), as.character)
  ) %>%
  # Нормализация ориентации колонок цен (работает для обеих сетей).
  # Инвариант после этого блока: price_regular = обычная цена,
  #                               price_discount = акционная цена (< price_regular) или NA.
  # У Магнита в исходных данных колонки перевёрнуты: price_regular хранит
  # фактическую (возможно акционную) цену, а price_discount — зачёркнутую
  # оригинальную (выше). У Пятёрочки ориентация правильная, но проверка
  # срабатывает и для неё на случай аномальных строк.
  mutate(
    .tmp_reg  = price_regular,
    .tmp_disc = price_discount,
    .inverted = !is.na(.tmp_disc) & .tmp_disc > .tmp_reg & .tmp_reg > 0,
    price_regular  = if_else(.inverted, .tmp_disc, .tmp_reg),
    price_discount = if_else(.inverted, .tmp_reg,  .tmp_disc)
  ) %>%
  select(-.tmp_reg, -.tmp_disc, -.inverted) %>%
  mutate(
    effective_price = coalesce(
      if_else(!is.na(price_discount) & price_discount < price_regular & price_discount > 0,
              price_discount, NA_real_),
      price_regular
    ),
    is_promo = !is.na(price_discount) & price_discount < price_regular & price_discount > 0,
    week     = floor_date(date, "week", week_start = 1)
  ) %>%
  filter(price_regular > 0, effective_price > 0)

message(glue("Строк после очистки: {nrow(panel)}"))
message(glue("   Уникальных товаров: {n_distinct(panel$product_id)}"))
message(glue("   NA в price_regular: {sum(is.na(panel$price_regular))}"))

# ── 1.4 Удаление дублей + финальная подготовка ───────────────────────────────
message("1.4 Удаление дублей и сортировка...")

panel <- panel %>%
  distinct(store_chain, store_code, product_id, date, .keep_all = TRUE) %>%
  arrange(store_chain, store_code, product_id, date)

message(glue("Строк после удаления дублей: {nrow(panel)}"))

# ── 1.5 Лаги, изменения цен и спеллы ───────────────────────────────────────
message("1.5 Расчёт лагов, изменений и спеллов...")

panel <- panel %>%
  group_by(store_chain, store_code, product_id) %>%
  mutate(
    lag_regular   = lag(price_regular),
    lag_effective = lag(effective_price),

    delta_regular_pct   = (price_regular   - lag_regular)   / lag_regular,
    delta_effective_pct = (effective_price - lag_effective) / lag_effective,

    changed_regular   = !is.na(delta_regular_pct)   & abs(delta_regular_pct)   > CHANGE_THRESHOLD,
    changed_effective = !is.na(delta_effective_pct) & abs(delta_effective_pct) > CHANGE_THRESHOLD,

    price_change_event = changed_effective | is.na(lag_effective),
    spell_id           = cumsum(price_change_event)
  ) %>%
  ungroup()

# ── 1.6 Длительность спеллов ───────────────────────────────────────────────
message("1.6 Расчёт длительности спеллов...")

spell_lengths <- panel %>%
  group_by(store_chain, store_code, product_id, spell_id) %>%
  summarise(
    spell_length_weeks = n(),
    spell_start_date   = min(date),
    spell_end_date     = max(date),
    .groups = "drop"
  )

panel <- panel %>%
  left_join(spell_lengths, by = c("store_chain", "store_code", "product_id", "spell_id"))

# ── 1.7 Дополнительные полезные переменные ────────────────────────────────
panel <- panel %>%
  mutate(
    log_price  = log(effective_price),
    year_month = format(date, "%Y-%m")
  )

# ── 1.7b Маппинг магазинов → районы СПб ─────────────────────────────────────
district_mapping <- tribble(
  ~store_code, ~district,
  # Пятёрочка
  "3448",  "Центральный",
  "L718",  "Центральный",
  "Q334",  "Центральный",
  "Q107",  "Центральный",
  "3AHB",  "Петроградский",
  "Q791",  "Петроградский",
  "326H",  "Невский",
  "Y350",  "Невский",
  "J286",  "Невский",
  "3AVK",  "Невский",
  "5546",  "Невский",
  # Магнит
  "783090", "Центральный",
  "733771", "Центральный",
  "699422", "Центральный",
  "200523", "Петроградский",
  "599166", "Петроградский",
  "838011", "Петроградский",
  "189030", "Петроградский",
  "780135", "Невский",
  "497884", "Невский",
  "708025", "Невский",
  "780019", "Невский",
  "974019", "Невский"
)

panel <- panel %>%
  left_join(district_mapping, by = "store_code")

message(glue("Районы: {sum(!is.na(panel$district))} из {nrow(panel)} строк получили маппинг"))

# ── 1.8 Сохранение ───────────────────────────────────────────────────────
message("1.8 Сохранение очищенной панели...")

saveRDS(panel, file.path(PATH_PROCESSED, "price_panel.rds"))
write_csv(panel, file.path(PATH_PROCESSED, "price_panel.csv"), na = "")

message(glue("ГОТОВО! Сохранена панель: {nrow(panel)} строк"))
message(glue("   Уникальных товаров: {n_distinct(panel$product_id)}"))
message(glue("   Уникальных магазинов: {n_distinct(panel$store_code)}"))
message(glue("   Период: {min(panel$date)} -- {max(panel$date)}"))

# ── Финальное резюме ─────────────────────────────────────────────────────
message("\n", strrep("=", 80))
message("                  ЗАГРУЗКА И ОЧИСТКА УСПЕШНО ЗАВЕРШЕНА")
message(strrep("=", 80))
message(glue("
  Файлов обработано          : {length(xlsx_files)}
  Строк в итоговой панели    : {nrow(panel)}
  Уникальных товаров         : {n_distinct(panel$product_id)}
  Магазинов                  : {n_distinct(panel$store_code)}
  Категорий                  : {n_distinct(panel$category_id)}
  Сетей                      : {n_distinct(panel$store_chain)}
  Период                     : {min(panel$date)} -- {max(panel$date)}
"))
