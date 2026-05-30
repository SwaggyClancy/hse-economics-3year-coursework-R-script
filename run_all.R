# ==============================================================================
# ГЛАВНЫЙ СКРИПТ: запускает весь анализ последовательно
# Авторы: Абдуазизов, Андреасян, Андреев | Версия: 2026-04
# ==============================================================================
# Запуск: setwd("путь/к/проекту") затем source("run_all.R")
# или из терминала: Rscript run_all.R
# ==============================================================================

t_start_total <- proc.time()

# ── Рабочая директория ────────────────────────────────────────────────────────
# Если запускаете через Rscript из папки проекта — всё ОК.
# Если через RStudio: раскомментируйте и укажите путь:
# setwd("D:/coding/projects/hse-economics-3year-coursework-R-script")

message(strrep("=", 70))
message("  ЗАПУСК ПОЛНОГО АНАЛИЗА")
message(strrep("=", 70))
message("  Рабочая директория: ", getwd())
message("  Время старта: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(strrep("=", 70))

# ── Вспомогательная функция запуска с замером времени ────────────────────────
run_script <- function(path, label) {
  message("\n", strrep("-", 70))
  message("  >> ", label)
  message(strrep("-", 70))
  t0 <- proc.time()
  tryCatch(
    source(path, encoding = "UTF-8"),
    error = function(e) {
      message("\n!!! ОШИБКА в ", label, " !!!")
      message(conditionMessage(e))
      message("Прерывание анализа.")
      stop(e)
    }
  )
  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(glue::glue("\n  << {label} завершён за {elapsed} сек."))
}

# ── Последовательный запуск всех скриптов ─────────────────────────────────────
run_script("scripts/01_load_and_clean.R",        "[01] Загрузка и очистка")
run_script("scripts/02_descriptive_metrics.R",   "[02] Описательные метрики")
run_script("scripts/03_correlations_and_sync.R", "[03] Корреляции и синхронность")
run_script("scripts/04_variance_decomposition.R","[04] Декомпозиция дисперсии")
run_script("scripts/05_clustering.R",            "[05] Кластеризация категорий")
run_script("scripts/06_panel_regression.R",      "[06] Панельная регрессия")
run_script("scripts/07_robustness.R",             "[07] Проверки устойчивости")
run_script("scripts/08_frequency_regressions.R", "[08] Регрессии на частоту")
run_script("scripts/09_report.R",                "[09] HTML-отчёт")

# ── Итоговое резюме ───────────────────────────────────────────────────────────
elapsed_total <- round((proc.time() - t_start_total)[["elapsed"]], 1)

message("\n", strrep("=", 70))
message("  АНАЛИЗ УСПЕШНО ЗАВЕРШЁН")
message(strrep("=", 70))
message(glue::glue("  Общее время: {elapsed_total} сек."))
message(glue::glue("  Конец: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"))
message(strrep("=", 70))

# ── Обновление метки времени в README ────────────────────────────────────────
# Подробное описание файлов хранится в output/README.txt (статичный файл).
# Здесь только добавляем строку с датой последнего запуска.
readme_path <- file.path("output", "README.txt")
if (file.exists(readme_path)) {
  readme_lines <- readLines(readme_path, encoding = "UTF-8", warn = FALSE)
  ts_line <- paste0("Последний запуск run_all.R: ",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  # обновляем или добавляем строку с временной меткой
  ts_idx <- grep("^Последний запуск", readme_lines)
  if (length(ts_idx) > 0) {
    readme_lines[ts_idx[1]] <- ts_line
  } else {
    readme_lines <- c(readme_lines, "", ts_line)
  }
  con <- file(readme_path, encoding = "UTF-8")
  writeLines(readme_lines, con)
  close(con)
  message("\n  Метка времени обновлена -> output/README.txt")
}
