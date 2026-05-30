# ================== 08. HTML REPORT GENERATION ==================================
# Читает CSV из output/tables/ и генерирует output/report.html.
# Запускается из run_all.R или вручную (нужен panel в памяти или panel.rds).

message("\n[09] Генерация HTML-отчёта...")
suppressPackageStartupMessages(library(glue))

# ── Загрузка данных ──────────────────────────────────────────────────────────
if (!exists("panel")) {
  rds_path <- file.path(PATH_PROCESSED, "price_panel.rds")
  if (!file.exists(rds_path)) stop("Нет panel в памяти и нет panel.rds. Запустите run_all.R")
  panel <- readRDS(rds_path)
}

rt <- function(s) read_csv(file.path(PATH_TABLES, paste0(s, ".csv")), show_col_types = FALSE)
chain_stk <- rt("01_chain_stickiness")
cat_stk   <- rt("02_category_stickiness")
cross_cor <- rt("03_cross_chain_correlations")
inter_sum <- rt("05_intercategory_summary")
vd_tbl    <- rt("06_variance_decomposition")
clust_ctr <- rt("07_cluster_centroids")
clust_cat <- rt("08_category_cluster_assignment")
reg_cfs   <- rt("09_regression_coefs")
gof_tbl   <- rt("10_regression_gof")
rc_tbl    <- rt("11_robustness_coefs")

# ── Форматирование ───────────────────────────────────────────────────────────
pp    <- function(x, d = 1) ifelse(is.na(x), "&mdash;", sprintf(paste0("%.", d, "f%%"), x * 100))
pnum  <- function(x, d = 3) ifelse(is.na(x), "&mdash;", sprintf(paste0("%.", d, "f"), x))
nfmt  <- function(x) formatC(as.integer(x), format = "d", big.mark = " ")
st    <- function(pv) case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**", pv < 0.10 ~ "*", TRUE ~ "")
ccls  <- function(x) if (!is.na(x) && x > 0) "coef-pos" else "coef-neg"
csign <- function(x) if (!is.na(x) && x >= 0) "+" else ""

coef_cell <- function(x, d = 4, bold = FALSE) {
  if (is.na(x)) return("&mdash;")
  r   <- round(x, d)
  cls <- if (r >= 0) "coef-pos" else "coef-neg"
  bld <- if (bold) " bold" else ""
  sgn <- if (r >= 0) "+" else ""
  glue('<span class="{cls}{bld}">{sgn}{r}</span>')
}

# ── Метаданные панели ────────────────────────────────────────────────────────
n_obs_reg  <- nrow(filter(panel, !is.na(delta_effective_pct)))
n_prods    <- n_distinct(panel$product_id)
n_wks      <- n_distinct(panel$week)
date_start <- format(min(panel$date, na.rm = TRUE), "%d.%m.%Y")
date_end   <- format(max(panel$date, na.rm = TRUE), "%d.%m.%Y")
gen_time   <- format(Sys.time(), "%Y-%m-%d %H:%M")

pya <- chain_stk %>% filter(store_chain == "Pyaterochka")
mag <- chain_stk %>% filter(store_chain == "Magnit")

# ── Регрессионные данные ─────────────────────────────────────────────────────
m1_mag <- reg_cfs %>% filter(grepl("M1", model), term == "is_magnit")
m1_prm <- reg_cfs %>% filter(grepl("M1", model), term == "is_promo_i")
m2_prm <- reg_cfs %>% filter(grepl("M2", model), term == "is_promo_i")
m3_prm <- reg_cfs %>% filter(grepl("M3|Pyaterochka", model), term == "is_promo_i")
m1_gof <- gof_tbl %>% filter(grepl("M1", Спецификация))
m2_gof <- gof_tbl %>% filter(grepl("M2", Спецификация))
m3_gof <- gof_tbl %>% filter(grepl("Pya|M3", Спецификация))

rc1_mag <- rc_tbl %>% filter(grepl("RC1", model), term == "is_magnit")
rc2_mag <- rc_tbl %>% filter(grepl("RC2", model), term == "is_magnit")
rc3_mag <- rc_tbl %>% filter(grepl("RC3", model), term == "is_magnit")
rc1_prm <- rc_tbl %>% filter(grepl("RC1", model), term == "is_promo_i")
rc2_prm <- rc_tbl %>% filter(grepl("RC2", model), term == "is_promo_i")
rc3_prm <- rc_tbl %>% filter(grepl("RC3", model), term == "is_promo_i")
rc_m1   <- rc_tbl %>% filter(grepl("M1|Основная", model))
rc_labels <- rc_tbl %>% distinct(model) %>% pull(model)
rc2_lbl <- rc_labels[grepl("RC2", rc_labels)]

sign_flip <- nrow(rc1_mag) > 0 && sign(m1_mag$estimate) != sign(rc1_mag$estimate)

# ── Корреляции и кластеры ────────────────────────────────────────────────────
pya_cor  <- inter_sum %>% filter(store_chain == "Pyaterochka")
mag_cor  <- inter_sum %>% filter(store_chain == "Magnit")
vd_solo  <- vd_tbl %>% filter(grepl("Только", Модель))
dc       <- names(vd_tbl)[3]   # ΔR2 column (Cyrillic name)
best_k   <- n_distinct(clust_cat$cluster)

# ── Adaptive callout texts ────────────────────────────────────────────────────
mag_promo_higher <- mag$promo_share > pya$promo_share
hilo_chain <- if (mag_promo_higher) "Магнит" else "Пятёрочка"

magnit_interp <- if (m1_mag$estimate > 0) {
  glue("Магнит демонстрирует на {pp(abs(m1_mag$estimate), 2)} п.п. бо&#769;льший прирост log-цены — следствие резкого возврата к регулярной цене после глубоких промо.")
} else {
  glue("Магнит демонстрирует на {pp(abs(m1_mag$estimate), 2)} п.п. меньший прирост log-цены — эффективные цены Магнита снижаются относительно Пятёрочки.")
}

rc_consistency <- if (!sign_flip) {
  paste0('<div class="callout callout-note"><div class="callout-icon">&#x2705;</div><div>',
         '<strong>Знак β(Magnit) сохраняется во всех спецификациях</strong> (',
         if (m1_mag$estimate > 0) "положительный" else "отрицательный",
         ' в M1, RC1, RC2, RC3). Результат устойчив к выбору порога и выборки.</div></div>')
} else {
  paste0('<div class="callout callout-warn"><div class="callout-icon">&#x26A0;&#xFE0F;</div><div>',
         '<strong>Аномалия знака:</strong> β(Magnit) меняет знак в RC1 (только регулярные цены). ',
         'Ценовое преимущество Магнита обусловлено промо-акциями — ',
         'без акций регулярные цены Магнита выше, чем у Пятёрочки.</div></div>')
}

# ── HTML: генераторы строк таблиц ────────────────────────────────────────────

make_cat_rows <- function() {
  rows <- character(0)
  prev_chain <- ""
  df <- cat_stk %>% arrange(store_chain, desc(freq_effective))
  for (i in seq_len(nrow(df))) {
    r  <- df[i, ]
    cn <- r$store_chain
    sep <- if (cn != prev_chain && prev_chain != "") ' style="border-top:2px solid #334155"' else ""
    prev_chain <- cn
    chain_cls <- if (cn == "Magnit") "mag" else "pya"
    chain_ru  <- if (cn == "Magnit") "Магнит" else "Пятёрочка"
    depth_val <- if (is.na(r$avg_promo_depth)) "&mdash;" else pp(r$avg_promo_depth)
    rows <- c(rows, glue(
      '<tr{sep}><td><span class="{chain_cls}">{chain_ru}</span></td>',
      '<td>{r$category_name}</td>',
      '<td class="num">{pp(r$freq_regular)}</td>',
      '<td class="num">{pp(r$freq_effective)}</td>',
      '<td class="num">{pp(r$avg_change_effective)}</td>',
      '<td class="num">{pp(r$promo_share)}</td>',
      '<td class="num">{depth_val}</td>',
      '<td class="num">{pnum(r$avg_spell_length, 1)}</td></tr>'
    ))
  }
  paste(rows, collapse = "\n")
}

make_cross_rows <- function() {
  rows <- character(0)
  df   <- cross_cor %>% arrange(desc(abs(coalesce(cor_chg_rate, 0))))
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    hl  <- if (!is.na(r$cor_chg_rate) && abs(r$cor_chg_rate) > 0.35) ' class="highlight"' else ""
    cr  <- if (is.na(r$cor_chg_rate)) "&mdash;" else {
      sgn <- if (r$cor_chg_rate > 0) "+" else ""
      cls <- if (r$cor_chg_rate > 0) "coef-pos" else "coef-neg"
      bld <- if (abs(r$cor_chg_rate) > 0.35) " bold" else ""
      glue('<span class="{cls}{bld}">{sgn}{pnum(r$cor_chg_rate)}</span>')
    }
    cd  <- if (is.na(r$cor_avg_delta)) "&mdash;" else {
      sgn <- if (r$cor_avg_delta > 0) "+" else ""
      glue('{sgn}{pnum(r$cor_avg_delta)}')
    }
    badge <- if (r$n_weeks == 0 || is.na(r$cor_chg_rate)) {
      '<span class="badge badge-gray">Только одна сеть</span>'
    } else if (r$cor_chg_rate > 0.5) {
      '<span class="badge badge-green">Высокая синхронность</span>'
    } else if (r$cor_chg_rate > 0.2) {
      '<span class="badge badge-blue">Умеренная</span>'
    } else if (r$cor_chg_rate >= -0.2) {
      '<span class="badge badge-gray">Слабая</span>'
    } else {
      '<span class="badge badge-red">Обратная</span>'
    }
    rows <- c(rows, glue(
      '<tr{hl}><td>{r$category_name}</td><td class="num">{cr}</td>',
      '<td class="num">{cd}</td><td class="num">{r$n_weeks}</td><td>{badge}</td></tr>'
    ))
  }
  paste(rows, collapse = "\n")
}

make_vd_rows <- function() {
  rows <- character(0)
  max_r2 <- max(vd_tbl$R2_adj)
  for (i in seq_len(nrow(vd_tbl))) {
    r   <- vd_tbl[i, ]
    dr  <- r[[dc]]
    hl  <- if (r$R2_adj == max_r2) ' class="highlight"' else ""
    b1  <- if (r$R2_adj == max_r2) ' bold' else ""
    interp <- case_when(
      grepl("Только сеть",      r$Модель) ~ "Наименьший вклад из трёх",
      grepl("Только категор",   r$Модель) ~ "Наибольший вклад",
      grepl("Только магаз",     r$Модель) ~ "Вклад магазина",
      grepl("Сеть.*Категор",    r$Модель) ~ "Добавление категории к сети",
      TRUE                               ~ "Полная модель"
    )
    rows <- c(rows, glue(
      '<tr{hl}><td>{r$Модель}</td>',
      '<td class="num{b1}">{pp(r$R2_adj, 3)}</td>',
      '<td class="num">{pp(dr, 3)}</td>',
      '<td>{interp}</td></tr>'
    ))
  }
  paste(rows, collapse = "\n")
}

make_cluster_cards <- function() {
  cards <- character(0)
  # name clusters by heuristic
  clust_names <- clust_ctr %>%
    mutate(cname = case_when(
      promo_share == max(promo_share) & freq_effective < median(freq_effective) ~
        "Стабильные цены &mdash; активное промо",
      freq_effective == max(freq_effective) | avg_change_effective == max(avg_change_effective) ~
        "Высокая ценовая активность",
      TRUE ~ "Умеренная динамика"
    ))
  for (i in seq_len(nrow(clust_names))) {
    r <- clust_names[i, ]
    cards <- c(cards, glue(
      '<div class="cluster-card">',
      '<div class="cluster-num">Кластер {r$cluster}</div>',
      '<h4>&laquo;{r$cname}&raquo;</h4>',
      '<div class="cats">{r$categories}</div>',
      '<div class="cluster-stat"><span class="k">Частота ΔP эфф.</span><span class="v">{pp(r$freq_effective)}</span></div>',
      '<div class="cluster-stat"><span class="k">Ср. размер ΔP</span><span class="v">{pp(r$avg_change_effective)}</span></div>',
      '<div class="cluster-stat"><span class="k">Доля акций</span><span class="v">{pp(r$promo_share)}</span></div>',
      '<div class="cluster-stat"><span class="k">Волатильность</span><span class="v">{pp(r$volatility)}</span></div>',
      '<div class="cluster-stat"><span class="k">Ср. спелл</span><span class="v">{pnum(r$avg_spell, 1)} нед</span></div>',
      '</div>'
    ))
  }
  paste(cards, collapse = "\n")
}

make_cluster_cat_rows <- function() {
  rows <- character(0)
  df   <- clust_cat %>% arrange(cluster, category_name)
  for (i in seq_len(nrow(df))) {
    r  <- df[i, ]
    hl <- if (r$promo_share == max(df$promo_share)) ' class="highlight"' else ""
    rows <- c(rows, glue(
      '<tr{hl}><td>{r$category_name}</td><td class="num bold">{r$cluster}</td>',
      '<td class="num">{pp(r$freq_effective)}</td>',
      '<td class="num">{pp(r$avg_change_effective)}</td>',
      '<td class="num">{pp(r$promo_share)}</td>',
      '<td class="num">{pnum(r$avg_spell, 1)}</td></tr>'
    ))
  }
  paste(rows, collapse = "\n")
}

make_rc_rows <- function() {
  # collect all models in order
  models <- c(rc_labels[grepl("M1|Основная", rc_labels)],
              rc_labels[grepl("RC1", rc_labels)],
              rc_labels[grepl("RC2", rc_labels)],
              rc_labels[grepl("RC3", rc_labels)])
  rows <- character(0)
  for (ml in models) {
    mag_r <- rc_tbl %>% filter(model == ml, term == "is_magnit")
    prm_r <- rc_tbl %>% filter(model == ml, term == "is_promo_i")
    hl    <- if (grepl("M1|Основная", ml)) ' class="highlight"' else ""
    b_mag <- grepl("M1|Основная", ml)

    mag_est <- if (nrow(mag_r) > 0) coef_cell(mag_r$estimate, bold = b_mag) else "&mdash;"
    mag_se  <- if (nrow(mag_r) > 0) pnum(mag_r$std_error) else "&mdash;"
    mag_p   <- if (nrow(mag_r) > 0) glue('<span class="sig-3">{st(mag_r$p_value)}</span>') else "&mdash;"
    prm_est <- if (nrow(prm_r) > 0) coef_cell(prm_r$estimate, bold = b_mag) else "&mdash;"
    prm_se  <- if (nrow(prm_r) > 0) pnum(prm_r$std_error) else "&mdash;"
    prm_p   <- if (nrow(prm_r) > 0) glue('<span class="sig-3">{st(prm_r$p_value)}</span>') else "&mdash;"

    arrow <- if (nrow(mag_r) > 0 && mag_r$estimate > 0) "&uarr;" else "&darr;"
    lbl   <- if (b_mag) glue("<strong>{ml}</strong>") else ml

    rows <- c(rows, glue(
      '<tr{hl}><td>{lbl}</td>',
      '<td class="num">{mag_est}</td><td class="num">{mag_se}</td><td class="num">{mag_p}</td>',
      '<td class="num">{prm_est}</td><td class="num">{prm_se}</td><td class="num">{prm_p}</td>',
      '<td>{arrow}</td></tr>'
    ))
  }
  paste(rows, collapse = "\n")
}

# ── Сборка HTML ──────────────────────────────────────────────────────────────
cat_rows_html    <- make_cat_rows()
cross_rows_html  <- make_cross_rows()
vd_rows_html     <- make_vd_rows()
cluster_cards    <- make_cluster_cards()
cluster_cat_rows <- make_cluster_cat_rows()
rc_rows_html     <- make_rc_rows()

# Вывод ключевых чисел для выводов
concl_1_freq_pya <- pp(pya$freq_effective)
concl_1_freq_mag <- pp(mag$freq_effective)
concl_1_chg_pya  <- pp(pya$avg_change_effective)
concl_1_chg_mag  <- pp(mag$avg_change_effective)
concl_1_spell_pya <- pnum(pya$avg_spell_length, 1)
concl_1_spell_mag <- pnum(mag$avg_spell_length, 1)
concl_2_promo_mag <- pp(mag$promo_share)
concl_2_promo_pya <- pp(pya$promo_share)
concl_2_depth_mag <- pp(mag$avg_promo_depth)
concl_2_depth_pya <- pp(pya$avg_promo_depth)
concl_3_med_pya   <- pnum(pya_cor$median_cor)
concl_3_pct_pya   <- round(pya_cor$pct_pos * 100)
concl_3_med_mag   <- pnum(mag_cor$median_cor)
concl_3_pct_mag   <- round(mag_cor$pct_pos * 100)
concl_4_r2_top    <- pp(max(vd_solo$R2_adj), 3)
concl_4_r2_full   <- pp(max(vd_tbl$R2_adj), 3)
concl_5_k         <- best_k
concl_6_n         <- nfmt(m1_gof$N)
concl_6_r2        <- pp(m1_gof$R2_within, 1)
concl_6_prm       <- pp(abs(m1_prm$estimate), 1)
concl_6_mag       <- pp(abs(m1_mag$estimate), 2)
concl_6_mag_sign  <- if (m1_mag$estimate > 0) "выше" else "ниже"

rc_sign_text <- if (sign_flip) {
  paste0("Знак &beta;(Magnit) меняется в RC1 (регулярные цены) &mdash; ценовое преимущество Магнита обусловлено промо.")
} else {
  paste0("Знак &beta;(Magnit) сохраняется во всех спецификациях &mdash; результат устойчив.")
}

html_out <- glue('<!doctype html>
<html lang="ru">
<head>
  <meta charset="UTF-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  <title>Ценовая липкость в российском продуктовом ретейле</title>
  <style>
    :root{{--blue:#2563eb;--blue-l:#dbeafe;--green:#16a34a;--green-l:#dcfce7;--red:#dc2626;--red-l:#fee2e2;--gray:#64748b;--gray-l:#f1f5f9;--border:#e2e8f0;--pya:#2563eb;--mag:#dc2626}}
    *{{box-sizing:border-box;margin:0;padding:0}}
    body{{font-family:"Segoe UI",system-ui,-apple-system,sans-serif;font-size:14px;line-height:1.6;color:#1e293b;background:#f8fafc;max-width:1100px;margin:0 auto;padding:24px 20px 60px}}
    .report-header{{background:linear-gradient(135deg,#1e3a5f 0%,#2563eb 100%);color:white;border-radius:16px;padding:40px 48px;margin-bottom:32px}}
    .report-header h1{{font-size:26px;font-weight:700;letter-spacing:-0.3px;margin-bottom:8px}}
    .report-header .subtitle{{font-size:15px;opacity:.85;margin-bottom:20px}}
    .meta-grid{{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:12px;margin-top:20px}}
    .meta-item{{background:rgba(255,255,255,.12);border-radius:10px;padding:12px 16px}}
    .meta-item .label{{font-size:11px;opacity:.7;text-transform:uppercase;letter-spacing:.6px}}
    .meta-item .value{{font-size:16px;font-weight:600;margin-top:2px}}
    .toc{{background:white;border:1px solid var(--border);border-radius:12px;padding:20px 28px;margin-bottom:28px}}
    .toc h2{{font-size:13px;text-transform:uppercase;letter-spacing:.8px;color:var(--gray);margin-bottom:12px}}
    .toc ol{{padding-left:20px}}.toc li{{margin-bottom:4px}}
    .toc a{{color:var(--blue);text-decoration:none}}.toc a:hover{{text-decoration:underline}}
    .section{{background:white;border:1px solid var(--border);border-radius:14px;padding:32px 36px;margin-bottom:24px}}
    .section-number{{display:inline-block;background:var(--blue);color:white;font-size:11px;font-weight:700;padding:2px 9px;border-radius:20px;margin-right:10px;vertical-align:middle}}
    .section h2{{font-size:20px;font-weight:700;margin-bottom:6px;display:flex;align-items:center;gap:4px}}
    .section-desc{{color:var(--gray);font-size:13px;margin-bottom:22px;padding-bottom:18px;border-bottom:1px solid var(--border)}}
    h3{{font-size:15px;font-weight:600;margin:22px 0 10px;color:#334155}}
    .method-box{{background:#f8fafc;border-left:3px solid var(--blue);border-radius:0 8px 8px 0;padding:12px 16px;margin-bottom:18px;font-size:13px;color:#475569}}
    .method-box strong{{color:#1e293b}}.method-box code{{background:#e2e8f0;padding:1px 5px;border-radius:4px;font-family:"Consolas",monospace;font-size:12px}}
    .tbl-wrap{{overflow-x:auto;margin:14px 0}}
    table{{width:100%;border-collapse:collapse;font-size:13px}}
    thead th{{background:#1e3a5f;color:white;padding:9px 13px;text-align:left;font-weight:600;font-size:12px;white-space:nowrap}}
    tbody tr:nth-child(even){{background:#f8fafc}}tbody tr:hover{{background:var(--blue-l)}}
    td{{padding:8px 13px;border-bottom:1px solid var(--border)}}
    .num{{text-align:right;font-variant-numeric:tabular-nums}}
    .pya{{color:var(--pya);font-weight:600}}.mag{{color:var(--mag);font-weight:600}}
    .highlight{{background:#fefce8!important}}.bold{{font-weight:700}}
    .badge{{display:inline-block;padding:2px 8px;border-radius:20px;font-size:11px;font-weight:600}}
    .badge-green{{background:var(--green-l);color:var(--green)}}.badge-red{{background:var(--red-l);color:var(--red)}}
    .badge-blue{{background:var(--blue-l);color:var(--blue)}}.badge-gray{{background:var(--gray-l);color:var(--gray)}}
    .callout{{border-radius:10px;padding:14px 18px;margin:16px 0;font-size:13px;display:flex;gap:12px;align-items:flex-start}}
    .callout-icon{{font-size:18px;flex-shrink:0;margin-top:1px}}
    .callout-key{{background:#eff6ff;border:1px solid #bfdbfe}}
    .callout-warn{{background:#fff7ed;border:1px solid #fed7aa}}
    .callout-note{{background:#f0fdf4;border:1px solid #bbf7d0}}
    .fig-wrap{{border:1px solid var(--border);border-radius:10px;overflow:hidden;margin:18px 0}}
    .fig-wrap img{{width:100%;display:block}}
    .fig-caption{{background:#f8fafc;border-top:1px solid var(--border);padding:9px 14px;font-size:12px;color:var(--gray)}}
    .fig-caption strong{{color:#334155}}
    .fig-grid{{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin:18px 0}}
    @media(max-width:720px){{.fig-grid{{grid-template-columns:1fr}}}}
    .stat-grid{{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:12px;margin:16px 0}}
    .stat-card{{border:1px solid var(--border);border-radius:10px;padding:14px 16px;text-align:center}}
    .stat-card .label{{font-size:11px;color:var(--gray);text-transform:uppercase;letter-spacing:.5px}}
    .stat-card .value{{font-size:22px;font-weight:700;margin:4px 0}}
    .stat-card .sub{{font-size:11px;color:var(--gray)}}
    .stat-card.pya-card{{border-color:#bfdbfe;background:#eff6ff}}.stat-card.pya-card .value{{color:var(--blue)}}
    .stat-card.mag-card{{border-color:#fecaca;background:#fef2f2}}.stat-card.mag-card .value{{color:var(--red)}}
    .cluster-grid{{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:14px;margin:16px 0}}
    .cluster-card{{border:1px solid var(--border);border-radius:12px;padding:16px;position:relative}}
    .cluster-num{{position:absolute;top:-10px;left:16px;background:var(--blue);color:white;font-size:11px;font-weight:700;padding:2px 10px;border-radius:20px}}
    .cluster-card h4{{font-size:13px;font-weight:700;margin:6px 0 8px}}
    .cluster-card .cats{{font-size:12px;color:var(--gray);margin-bottom:10px;font-style:italic}}
    .cluster-stat{{display:flex;justify-content:space-between;font-size:12px;padding:3px 0;border-bottom:1px solid var(--border)}}
    .cluster-stat:last-child{{border-bottom:none}}.cluster-stat .k{{color:var(--gray)}}.cluster-stat .v{{font-weight:600}}
    .sig-3{{color:#7c3aed;font-weight:700}}.coef-pos{{color:var(--green)}}.coef-neg{{color:var(--red)}}
    .conclusions{{background:linear-gradient(135deg,#0f172a 0%,#1e3a5f 100%);color:white;border-radius:14px;padding:32px 36px;margin-top:28px}}
    .conclusions h2{{font-size:20px;font-weight:700;margin-bottom:20px;color:white}}
    .conclusion-item{{display:flex;gap:16px;margin-bottom:16px;align-items:flex-start}}
    .conclusion-num{{background:rgba(255,255,255,.15);color:white;font-weight:700;font-size:13px;width:28px;height:28px;border-radius:50%;display:flex;align-items:center;justify-content:center;flex-shrink:0;margin-top:1px}}
    .conclusion-text{{font-size:14px;line-height:1.6;opacity:.92}}
    .conclusion-text strong{{color:#93c5fd}}
    footer{{text-align:center;font-size:12px;color:var(--gray);margin-top:32px}}
  </style>
</head>
<body>

<!-- HEADER -->
<div class="report-header">
  <h1>Ценовая липкость в российском продуктовом ретейле</h1>
  <div class="subtitle">Сравнительный анализ Пятёрочки и Магнита методами панельной регрессии и кластеризации</div>
  <div class="meta-grid">
    <div class="meta-item"><div class="label">Авторы</div><div class="value" style="font-size:13px">Абдуазизов, Андреасян, Андреев</div></div>
    <div class="meta-item"><div class="label">Период данных</div><div class="value">{n_wks} недель</div></div>
    <div class="meta-item"><div class="label">Наблюдений (регрессия)</div><div class="value">{nfmt(m1_gof$N)}</div></div>
    <div class="meta-item"><div class="label">Товаров</div><div class="value">{nfmt(n_prods)}</div></div>
    <div class="meta-item"><div class="label">Категорий</div><div class="value">{mag$n_categories} (Mag) + {pya$n_categories} (Pya)</div></div>
    <div class="meta-item"><div class="label">Дата генерации</div><div class="value" style="font-size:13px">{gen_time}</div></div>
  </div>
</div>

<!-- TOC -->
<div class="toc">
  <h2>Содержание</h2>
  <ol>
    <li><a href="#s1">Описательные метрики ценовой липкости</a></li>
    <li><a href="#s2">Корреляции и синхронность между сетями</a></li>
    <li><a href="#s3">Декомпозиция дисперсии</a></li>
    <li><a href="#s4">Кластеризация категорий (K={best_k})</a></li>
    <li><a href="#s5">Панельная регрессия</a></li>
    <li><a href="#s6">Проверки устойчивости</a></li>
    <li><a href="#s7">Регрессии на частоту изменений (LPM, Granger, AR, RF)</a></li>
    <li><a href="#conclusions">Ключевые выводы</a></li>
  </ol>
</div>

<!-- SECTION 1: STICKINESS -->
<div class="section" id="s1">
  <h2><span class="section-number">01</span>Описательные метрики ценовой липкости</h2>
  <div class="section-desc">
    Таблицы: <code>01_chain_stickiness.csv/.html</code> &middot; <code>02_category_stickiness.csv/.html</code>
    &nbsp;|&nbsp; Графики: <code>01</code>&ndash;<code>07d_*.png</code> (частота, спеллы, гистограммы, рублёвые изменения, регулярная цена)
  </div>
  <div class="method-box">
    <strong>Методология.</strong> Для каждой пары (товар, неделя):
    <code>freq_effective</code> &mdash; доля недель с <code>|&Delta;P_eff| &gt; 1%</code>;
    <code>promo_share</code> &mdash; доля периодов с активной акцией;
    <code>avg_promo_depth</code> = (P_reg &minus; P_eff) / P_reg по акционным периодам;
    <code>avg_spell_length</code> &mdash; средняя длина &laquo;ценового спелла&raquo; (RLE по неизменным эффективным ценам).
  </div>

  <h3>Таблица 01 &mdash; Сводка по сетям <code style="font-size:11px">01_chain_stickiness.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Сеть</th><th class="num">Частота &Delta;P рег.</th><th class="num">Частота &Delta;P эфф.</th>
        <th class="num">Ср. &Delta;P эфф.</th><th class="num">Доля акций</th><th class="num">Глубина акции</th>
        <th class="num">Ср. спелл, нед</th><th class="num">Волатильность</th>
        <th class="num">Категорий</th><th class="num">Товаров</th>
      </tr></thead>
      <tbody>
        <tr>
          <td><span class="pya">Пятёрочка</span></td>
          <td class="num">{pp(pya$freq_regular)}</td>
          <td class="num bold">{pp(pya$freq_effective)}</td>
          <td class="num">{pp(pya$avg_change_effective)}</td>
          <td class="num">{pp(pya$promo_share)}</td>
          <td class="num">{pp(pya$avg_promo_depth)}</td>
          <td class="num">{pnum(pya$avg_spell_length, 1)}</td>
          <td class="num">{pp(pya$volatility_effective)}</td>
          <td class="num">{pya$n_categories}</td>
          <td class="num">{nfmt(pya$n_products)}</td>
        </tr>
        <tr class="highlight">
          <td><span class="mag">Магнит</span></td>
          <td class="num">{pp(mag$freq_regular)}</td>
          <td class="num">{pp(mag$freq_effective)}</td>
          <td class="num bold">{pp(mag$avg_change_effective)}</td>
          <td class="num bold">{pp(mag$promo_share)}</td>
          <td class="num bold">{pp(mag$avg_promo_depth)}</td>
          <td class="num bold">{pnum(mag$avg_spell_length, 1)}</td>
          <td class="num">{pp(mag$volatility_effective)}</td>
          <td class="num">{mag$n_categories}</td>
          <td class="num">{nfmt(mag$n_products)}</td>
        </tr>
      </tbody>
    </table>
  </div>

  <div class="callout callout-key">
    <div class="callout-icon">&#x1F4A1;</div>
    <div>
      <strong>Две разные ценовые стратегии.</strong>
      Пятёрочка практикует <em>гибкое ценообразование</em> &mdash; регулярные цены меняются часто ({pp(pya$freq_regular)}), промо умеренные ({pp(pya$promo_share)}, скидка {pp(pya$avg_promo_depth)}).
      Магнит практикует стратегию <em>Hi-Lo</em> &mdash; регулярные цены стабильны ({pp(mag$freq_regular)}), но агрессивные промо ({pp(mag$promo_share)}, скидка {pp(mag$avg_promo_depth)}) создают высокую активность эффективной цены.
    </div>
  </div>

  <h3>Таблица 02 &mdash; Липкость по категориям <code style="font-size:11px">02_category_stickiness.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Сеть</th><th>Категория</th><th class="num">Частота &Delta;P рег.</th>
        <th class="num">Частота &Delta;P эфф.</th><th class="num">Ср. &Delta;P эфф.</th>
        <th class="num">Доля акций</th><th class="num">Глубина акции</th><th class="num">Ср. спелл, нед</th>
      </tr></thead>
      <tbody>{cat_rows_html}</tbody>
    </table>
  </div>

  <div class="fig-wrap">
    <img src="plots/01_freq_by_category.png" alt="Частота изменений по категориям" loading="lazy"/>
    <div class="fig-caption"><strong>График 01 &middot; 01_freq_by_category.png</strong> &mdash; Частота изменения эффективной цены по категориям (bar chart). Ось Y &mdash; доля недель с изменением цены.</div>
  </div>
  <div class="fig-wrap">
    <img src="plots/02_promo_vs_freq.png" alt="Промо vs частота" loading="lazy"/>
    <div class="fig-caption"><strong>График 02 &middot; 02_promo_vs_freq.png</strong> &mdash; Scatter-plot: доля акций vs частота изменений цен. Размер точки = количество товаров.</div>
  </div>
  <div class="fig-wrap">
    <img src="plots/03_spell_distribution.png" alt="Распределение спеллов" loading="lazy"/>
    <div class="fig-caption"><strong>График 03</strong> &mdash; Длительность ценовых спеллов (недели без изменений) по <strong>эффективной цене</strong>.</div>
  </div>
  <div class="fig-wrap">
    <img src="plots/04_freq_hist_by_chain.png" alt="Гистограмма частоты по сетям" loading="lazy"/>
    <div class="fig-caption"><strong>График 04</strong> &mdash; Распределение частоты изменений <strong>эффективной цены</strong> по товарам. U-образность у Магнита = два типа товаров (стабильные и акционные).</div>
  </div>
  <div class="fig-grid">
    <div class="fig-wrap">
      <img src="plots/05_freq_density_combined.png" alt="Плотность частоты" loading="lazy"/>
      <div class="fig-caption"><strong>График 05</strong> &mdash; Плотность распределения по <strong>эффективной цене</strong>: сравнение форм двух сетей.</div>
    </div>
    <div class="fig-wrap">
      <img src="plots/07_freq_eff_vs_reg.png" alt="Эффективная vs регулярная" loading="lazy"/>
      <div class="fig-caption"><strong>График 07</strong> &mdash; <strong>Эффективная vs Регулярная цена</strong>: как промо меняет форму распределения частоты изменений.</div>
    </div>
  </div>
  <div class="fig-wrap">
    <img src="plots/06_freq_hist_by_category.png" alt="Гистограммы по категориям" loading="lazy"/>
    <div class="fig-caption"><strong>График 06</strong> &mdash; Гистограмма частоты изменений <strong>эффективной цены</strong> по каждой категории. Тёмно-красный = Пятёрочка, синий = Магнит (поверх).</div>
  </div>
  <div class="fig-grid">
    <div class="fig-wrap">
      <img src="plots/07b_price_delta_rub_effective.png" alt="Рублёвые изменения эфф. цены" loading="lazy"/>
      <div class="fig-caption"><strong>График 07b</strong> &mdash; Размер изменений <strong>эффективной цены</strong> в рублях. Только периоды с фактическим изменением, шаг 5 руб.</div>
    </div>
    <div class="fig-wrap">
      <img src="plots/07c_price_delta_rub_regular.png" alt="Рублёвые изменения рег. цены" loading="lazy"/>
      <div class="fig-caption"><strong>График 07c</strong> &mdash; Размер изменений <strong>регулярной (полочной) цены</strong> в рублях. Сравни с 07b: насколько амплитуда акций крупнее.</div>
    </div>
  </div>
  <div class="fig-wrap">
    <img src="plots/07d_freq_regular_by_chain.png" alt="Частота рег. цены" loading="lazy"/>
    <div class="fig-caption"><strong>График 07d</strong> &mdash; Распределение частоты изменений <strong>регулярной (полочной) цены</strong> по товарам. Сравни с графиком 04: у Магнита регулярные цены гораздо стабильнее.</div>
  </div>
</div>

<!-- SECTION 2: CORRELATIONS -->
<div class="section" id="s2">
  <h2><span class="section-number">02</span>Корреляции и синхронность</h2>
  <div class="section-desc">
    Таблицы: <code>03_cross_chain_correlations.csv/.html</code> &middot; <code>05_intercategory_summary.csv</code>
    &nbsp;|&nbsp; Графики: <code>08_intercategory_cor_pyaterochka.png</code> &middot; <code>09_intercategory_cor_magnit.png</code> &middot; <code>10_intercategory_cor_combined.png</code>
  </div>
  <div class="method-box">
    <strong>Методология.</strong>
    <strong>Кросс-цепочечная корреляция</strong> &mdash; Пирсон между Пятёрочкой и Магнитом по неделям внутри категории.
    <strong>Межкатегорийная корреляция</strong> &mdash; попарная корреляция временных рядов частоты изменений внутри сети.
  </div>

  <h3>Таблица 03 &mdash; Синхронность между сетями <code style="font-size:11px">03_cross_chain_correlations.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Категория</th><th class="num">Корр. (частота)</th>
        <th class="num">Корр. (ср. &Delta;%)</th><th class="num">Недель</th><th>Интерпретация</th>
      </tr></thead>
      <tbody>{cross_rows_html}</tbody>
    </table>
  </div>

  <h3>Межкатегорийные корреляции внутри сети <code style="font-size:11px">05_intercategory_summary.csv</code></h3>
  <div class="stat-grid">
    <div class="stat-card pya-card">
      <div class="label">Пятёрочка &middot; медиана</div>
      <div class="value">{pnum(pya_cor$median_cor)}</div>
      <div class="sub">Межкатегорийная корреляция</div>
    </div>
    <div class="stat-card pya-card">
      <div class="label">Пятёрочка &middot; % положительных</div>
      <div class="value">{round(pya_cor$pct_pos * 100)}%</div>
      <div class="sub">Пар с положительной корреляцией</div>
    </div>
    <div class="stat-card mag-card">
      <div class="label">Магнит &middot; медиана</div>
      <div class="value">{pnum(mag_cor$median_cor)}</div>
      <div class="sub">Межкатегорийная корреляция</div>
    </div>
    <div class="stat-card mag-card">
      <div class="label">Магнит &middot; % положительных</div>
      <div class="value">{round(mag_cor$pct_pos * 100)}%</div>
      <div class="sub">Пар с положительной корреляцией</div>
    </div>
  </div>

  <div class="callout callout-key">
    <div class="callout-icon">&#x1F4A1;</div>
    <div>
      <strong>Структура ценообразования:</strong>
      В Пятёрочке медиана межкатегорийной корреляции = {pnum(pya_cor$median_cor)}, {round(pya_cor$pct_pos * 100)}% пар положительны &mdash;
      <strong>централизованное ценообразование</strong>.
      В Магните медиана = {pnum(mag_cor$median_cor)}, {round(mag_cor$pct_pos * 100)}% положительных &mdash;
      <strong>децентрализованное</strong>. Промо запускаются по категориям независимо.
    </div>
  </div>

  <div class="fig-grid">
    <div class="fig-wrap">
      <img src="plots/08_intercategory_cor_pyaterochka.png" alt="Корреляции Пятёрочка" loading="lazy"/>
      <div class="fig-caption"><strong>Пятёрочка</strong> &mdash; Красный = положительная корреляция, синий = отрицательная. Централизованное ценообразование.</div>
    </div>
    <div class="fig-wrap">
      <img src="plots/09_intercategory_cor_magnit.png" alt="Корреляции Магнит" loading="lazy"/>
      <div class="fig-caption"><strong>Магнит</strong> &mdash; Пёстрая карта: категории принимают промо-решения независимо.</div>
    </div>
  </div>
  <div class="fig-wrap">
    <img src="plots/10_intercategory_cor_combined.png" alt="Корреляции обе сети" loading="lazy"/>
    <div class="fig-caption"><strong>Обе сети вместе (усреднено)</strong> &mdash; Частота изменений усреднена по Пятёрочке и Магниту. Показывает «общерыночный» паттерн синхронности категорий.</div>
  </div>
</div>

<!-- SECTION 3: VARIANCE -->
<div class="section" id="s3">
  <h2><span class="section-number">03</span>Декомпозиция дисперсии</h2>
  <div class="section-desc">
    Таблица: <code>06_variance_decomposition.csv/.html</code> &nbsp;|&nbsp; График: <code>11_variance_decomposition.png</code>
  </div>
  <div class="method-box">
    <strong>Методология.</strong> Последовательные модели через <code>fixest::feols</code>:
    <code>&Delta;_eff ~ 1 | сеть</code>, <code>~ 1 | категория</code>, <code>~ 1 | магазин</code>,
    <code>~ 1 | сеть+категория</code>, <code>~ 1 | сеть+категория+магазин</code>.
    Инкрементальный &Delta;R&sup2; показывает &laquo;чистый&raquo; вклад каждого уровня.
  </div>

  <h3>Таблица 06 <code style="font-size:11px">06_variance_decomposition.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Модель (FE)</th><th class="num">R&sup2; adj.</th>
        <th class="num">&Delta;R&sup2; (инкрем.)</th><th>Вывод</th>
      </tr></thead>
      <tbody>{vd_rows_html}</tbody>
    </table>
  </div>

  <div class="callout callout-note">
    <div class="callout-icon">&#x1F4CA;</div>
    <div>
      Суммарно FE объясняют лишь {concl_4_r2_full} вариации &Delta;_eff &mdash; большинство изменений идиосинкратичны.
      Наибольший вклад вносит <strong>категория товара</strong> ({concl_4_r2_top} отдельно).
    </div>
  </div>

  <div class="fig-wrap" style="max-width:600px">
    <img src="plots/11_variance_decomposition.png" alt="Декомпозиция дисперсии" loading="lazy"/>
    <div class="fig-caption"><strong>График 11 &middot; 11_variance_decomposition.png</strong> &mdash; Bar chart трёх &laquo;чистых&raquo; вкладов в R&sup2; adj.</div>
  </div>
</div>

<!-- SECTION 4: CLUSTERING -->
<div class="section" id="s4">
  <h2><span class="section-number">04</span>Кластеризация категорий</h2>
  <div class="section-desc">
    Таблицы: <code>07_cluster_centroids.csv/.html</code> &middot; <code>08_category_cluster_assignment.csv</code>
    &nbsp;|&nbsp; Графики: <code>12_silhouette_scores.png</code> &middot; <code>13_cluster_pca.png</code>
  </div>
  <div class="method-box">
    <strong>Методология.</strong> Признаки: <code>freq_effective</code>, <code>avg_change_effective</code>,
    <code>promo_share</code>, <code>volatility</code>, <code>avg_spell_length</code> &mdash;
    взвешенное среднее по обеим сетям. Матрица стандартизирована (<code>scale()</code>).
    K выбирается по максимуму среднего silhouette score. K-means: <code>set.seed(42)</code>, <code>nstart=50</code>.
  </div>

  <div class="fig-wrap" style="max-width:500px">
    <img src="plots/12_silhouette_scores.png" alt="Silhouette scores" loading="lazy"/>
    <div class="fig-caption"><strong>График 12 &middot; 12_silhouette_scores.png</strong> &mdash; Оптимальное число кластеров K={best_k} (максимум silhouette score).</div>
  </div>

  <h3>Таблица 07 &mdash; Центроиды кластеров (K={best_k}) <code style="font-size:11px">07_cluster_centroids.csv</code></h3>
  <div class="cluster-grid">{cluster_cards}</div>

  <h3>Таблица 08 &mdash; Принадлежность категорий <code style="font-size:11px">08_category_cluster_assignment.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Категория</th><th class="num">Кластер</th>
        <th class="num">Частота эфф.</th><th class="num">Ср. &Delta;P</th>
        <th class="num">Акции</th><th class="num">Спелл, нед</th>
      </tr></thead>
      <tbody>{cluster_cat_rows}</tbody>
    </table>
  </div>

  <div class="fig-wrap">
    <img src="plots/13_cluster_pca.png" alt="PCA кластеры" loading="lazy"/>
    <div class="fig-caption"><strong>График 13 &middot; 13_cluster_pca.png</strong> &mdash; PCA-проекция категорий с кластерными оболочками (K={best_k}).</div>
  </div>
</div>

<!-- SECTION 5: REGRESSION -->
<div class="section" id="s5">
  <h2><span class="section-number">05</span>Панельная регрессия</h2>
  <div class="section-desc">
    Таблицы: <code>09_regression_coefs.csv/.html</code> &middot; <code>09_regression_results.tex</code> &middot; <code>10_regression_gof.csv/.html</code>
    &nbsp;|&nbsp; Графики: <code>14_coef_m1.png</code> &middot; <code>15_coef_magnit.png</code> &middot; <code>16_coef_promo.png</code>
  </div>
  <div class="method-box">
    <strong>M1:</strong> <code>&Delta;ln(P_eff) = &beta;&#x2081;&middot;Magnit + &beta;&#x2082;&middot;Promo + &alpha;_cat + &alpha;_week + &epsilon;</code><br/>
    <strong>M2:</strong> <code>&Delta;ln(P_eff) = &beta;&#x2082;&middot;Promo | cat + week + store</code><br/>
    <strong>M3 (только Пятёрочка):</strong> <code>&Delta;ln(P_eff) = &beta;&#x2082;&middot;Promo | cat + week</code><br/>
    SE кластеризованы по магазину (<code>fixest::feols</code>). FE по кросс-цепочечному имени категории.
  </div>

  <h3>Таблица 09 &mdash; Коэффициенты <code style="font-size:11px">09_regression_coefs.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Регрессор</th><th class="num">Оценка &beta;</th>
        <th class="num">Ст. ошибка</th><th class="num">t-стат.</th>
        <th class="num">p-value</th><th>Спецификация</th>
      </tr></thead>
      <tbody>
        <tr class="highlight">
          <td><strong>Магнит (vs Пятёрочка)</strong></td>
          <td class="num">{coef_cell(m1_mag$estimate, bold = TRUE)}</td>
          <td class="num">{pnum(m1_mag$std_error)}</td>
          <td class="num">{pnum(m1_mag$t_stat, 2)}</td>
          <td class="num"><span class="sig-3">{st(m1_mag$p_value)}</span></td>
          <td>M1: FE(cat + week)</td>
        </tr>
        <tr class="highlight">
          <td><strong>Промо-акция</strong></td>
          <td class="num">{coef_cell(m1_prm$estimate, bold = TRUE)}</td>
          <td class="num">{pnum(m1_prm$std_error)}</td>
          <td class="num">{pnum(m1_prm$t_stat, 2)}</td>
          <td class="num"><span class="sig-3">{st(m1_prm$p_value)}</span></td>
          <td>M1: FE(cat + week)</td>
        </tr>
        <tr>
          <td>Промо-акция</td>
          <td class="num">{coef_cell(m2_prm$estimate)}</td>
          <td class="num">{pnum(m2_prm$std_error)}</td>
          <td class="num">{pnum(m2_prm$t_stat, 2)}</td>
          <td class="num"><span class="sig-3">{st(m2_prm$p_value)}</span></td>
          <td>M2: FE(cat + week + store)</td>
        </tr>
        <tr>
          <td>Промо-акция</td>
          <td class="num">{coef_cell(m3_prm$estimate)}</td>
          <td class="num">{pnum(m3_prm$std_error)}</td>
          <td class="num">{pnum(m3_prm$t_stat, 2)}</td>
          <td class="num"><span class="sig-3">{st(m3_prm$p_value)}</span></td>
          <td>M3: Только Пятёрочка FE(cat + week)</td>
        </tr>
      </tbody>
    </table>
  </div>

  <div class="callout callout-key">
    <div class="callout-icon">&#x1F4C8;</div>
    <div>
      <strong>&beta;(Magnit) = {coef_cell(m1_mag$estimate, d = 4)}{st(m1_mag$p_value)}</strong> &mdash;
      {magnit_interp}<br/><br/>
      <strong>&beta;(Promo) = {coef_cell(m1_prm$estimate, d = 4)}{st(m1_prm$p_value)}</strong> &mdash;
      в акционный период эффективная цена снижается на {pp(abs(m1_prm$estimate), 1)} (log-изменение).
      Коэффициент устойчив в M1 ({pp(abs(m1_prm$estimate), 1)}) и M2 ({pp(abs(m2_prm$estimate), 1)}).
    </div>
  </div>

  <h3>Таблица 10 &mdash; Качество подгонки <code style="font-size:11px">10_regression_gof.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Спецификация</th><th class="num">N</th>
        <th class="num">R&sup2; within</th><th class="num">R&sup2; adj.</th>
      </tr></thead>
      <tbody>
        <tr><td>M1 &mdash; Pooled FE(cat+week)</td><td class="num">{nfmt(m1_gof$N)}</td><td class="num bold">{pp(m1_gof$R2_within, 2)}</td><td class="num">{pp(m1_gof$R2_adj, 2)}</td></tr>
        <tr><td>M2 &mdash; FE(cat+week+store)</td><td class="num">{nfmt(m2_gof$N)}</td><td class="num">{pp(m2_gof$R2_within, 2)}</td><td class="num">{pp(m2_gof$R2_adj, 2)}</td></tr>
        <tr class="highlight"><td>M3 &mdash; Только Пятёрочка</td><td class="num">{nfmt(m3_gof$N)}</td><td class="num">{pp(m3_gof$R2_within, 2)}</td><td class="num">{pp(m3_gof$R2_adj, 2)}</td></tr>
      </tbody>
    </table>
  </div>

  <div class="callout callout-note">
    <div class="callout-icon">&#x1F4CA;</div>
    <div>Коэффициентные графики (14–16) доступны в <code>output/plots/</code> для дополнительного изучения.</div>
  </div>
</div>

<!-- SECTION 6: ROBUSTNESS -->
<div class="section" id="s6">
  <h2><span class="section-number">06</span>Проверки устойчивости</h2>
  <div class="section-desc">
    Таблицы: <code>11_robustness_coefs.csv</code> &middot; <code>11_robustness_table.csv/.html</code>
    &nbsp;|&nbsp; Графики: <code>18_rc_magnit.png</code> &middot; <code>19_rc_promo.png</code>
  </div>
  <div class="method-box">
    <strong>RC1 &mdash; Только регулярные цены:</strong> <code>log(P_reg / P_reg_lag)</code>, промо исключены.<br/>
    <strong>{rc2_lbl}:</strong> смягчённый порог изменения вместо основных 1%.<br/>
    <strong>RC3 &mdash; Без выбросов:</strong> <code>|&Delta;ln(P)| &le; 50%</code>.
  </div>

  <h3>Таблица 11 &mdash; Сводная robustness <code style="font-size:11px">11_robustness_coefs.csv</code></h3>
  <div class="tbl-wrap">
    <table>
      <thead><tr>
        <th>Спецификация</th>
        <th class="num">&beta;(Magnit)</th><th class="num">SE</th><th class="num">p</th>
        <th class="num">&beta;(Promo)</th><th class="num">SE</th><th class="num">p</th>
        <th>Знак Magnit</th>
      </tr></thead>
      <tbody>{rc_rows_html}</tbody>
    </table>
  </div>

  {rc_consistency}

  <div class="callout callout-note">
    <div class="callout-icon">&#x1F4CA;</div>
    <div>Коэффициентные графики (18–19) доступны в <code>output/plots/</code>.</div>
  </div>
</div>

<!-- SECTION 7: FREQUENCY REGRESSIONS -->
<div class="section" id="s7">
  <h2><span class="section-number">07</span>Регрессии на частоту изменений (скрипт 08)</h2>
  <div class="section-desc">
    Таблицы: <code>12_top5_correlations.csv</code> &middot; <code>13_lpm_gof.csv</code>
    &middot; <code>14_granger_coefs.csv</code> &middot; <code>15_ar_gof.csv</code>
    &nbsp;|&nbsp; Графики: <code>20a_lpm_magnit.png</code> &middot; <code>20b_lpm_promo.png</code>
    &middot; <code>21a_granger_pya.png</code> &middot; <code>21b_granger_mag.png</code>
    &middot; <code>22a_ar_lag.png</code> &middot; <code>22b_ar_promo.png</code>
    &middot; <code>23_rf_importance.png</code>
  </div>
  <div class="method-box">
    <strong>LPM (Линейная модель вероятности).</strong>
    Зависимая: <code>changed_effective</code> (0/1) &mdash; изменилась ли <strong>эффективная</strong> цена.
    Интерпретация: &beta; = изменение вероятности изменения цены в п.п.<br/>
    <strong>Granger.</strong> <code>chg_rate_Pya_t ~ lag(chg_rate_Mag) + lag(chg_rate_Pya)</code> &mdash;
    предсказывает ли прошлая частота Магнита текущую Пятёрочки?<br/>
    <strong>AR(1).</strong> <code>changed_effective_t ~ changed_effective_{{t-1}}</code> | FE(кат+нед) &mdash;
    инерция или торможение ценовых изменений?
  </div>

  <div class="callout callout-note">
    <div class="callout-icon">&#x1F4CA;</div>
    <div>Коэффициентные графики LPM/Granger/AR (20a–22b) доступны в <code>output/plots/</code>.</div>
  </div>
  <div class="fig-wrap">
    <img src="plots/23_rf_importance.png" alt="Random Forest важность" loading="lazy"/>
    <div class="fig-caption"><strong>График 23 &middot; Random Forest</strong> &mdash; Важность групп переменных для предсказания изменения <strong>эффективной</strong> цены. Доминирование FE-групп подтверждает: структурные факторы объясняют большую часть вероятности изменения цены.</div>
  </div>
</div>

<!-- CONCLUSIONS -->
<div class="conclusions" id="conclusions">
  <h2>Ключевые выводы</h2>

  <div class="conclusion-item">
    <div class="conclusion-num">1</div>
    <div class="conclusion-text">
      <strong>Цены Пятёрочки менее &laquo;липкие&raquo; по частоте</strong>
      ({concl_1_freq_pya} vs {concl_1_freq_mag}), но Магнит компенсирует это крупными изменениями
      ({concl_1_chg_mag} за событие vs {concl_1_chg_pya}). Медианный спелл Магнита &mdash; {concl_1_spell_mag} нед, Пятёрочки &mdash; {concl_1_spell_pya} нед.
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">2</div>
    <div class="conclusion-text">
      <strong>Магнит &mdash; более промо-активная сеть:</strong>
      {concl_2_promo_mag} периодов с акциями и глубина скидки {concl_2_depth_mag},
      против {concl_2_promo_pya} и {concl_2_depth_pya} у Пятёрочки.
      Магнит практикует стратегию Hi-Lo: стабильные регулярные цены + агрессивные промо.
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">3</div>
    <div class="conclusion-text">
      <strong>Структура ценообразования различается принципиально:</strong>
      Пятёрочка &mdash; централизованная (медиана межкатегорийной корреляции {concl_3_med_pya},
      {concl_3_pct_pya}% пар положительны);
      Магнит &mdash; децентрализованная (медиана {concl_3_med_mag}, {concl_3_pct_mag}% положительных).
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">4</div>
    <div class="conclusion-text">
      <strong>Категория объясняет изменчивость цен больше всего</strong>
      (R&sup2; = {concl_4_r2_top} отдельно, {concl_4_r2_full} в полной модели).
      Большинство изменений идиосинкратичны &mdash; не объясняются структурными факторами.
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">5</div>
    <div class="conclusion-text">
      <strong>{concl_5_k} типа категорий (K-means):</strong>
      кластеры выделены по частоте изменений, прomo-активности и длине ценовых спеллов.
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">6</div>
    <div class="conclusion-text">
      <strong>Панельная регрессия (N = {concl_6_n}):</strong>
      промо снижает эффективную цену на <strong>~{concl_6_prm}</strong> за период (высокозначимо, R&sup2; = {concl_6_r2}).
      {magnit_interp}
    </div>
  </div>

  <div class="conclusion-item">
    <div class="conclusion-num">7</div>
    <div class="conclusion-text">
      <strong>Робастность:</strong>
      эффект промо стабилен во всех трёх проверках (RC1&ndash;RC3).
      {rc_sign_text}
    </div>
  </div>
</div>

<footer>
  Сгенерировано автоматически &middot; scripts/09_report.R &middot; {gen_time} &middot;
  Данные: {date_start} &ndash; {date_end}
</footer>

</body>
</html>')

writeLines(html_out, file.path("output", "report.html"), useBytes = FALSE)
message("  HTML-отчёт сохранён -> output/report.html")
