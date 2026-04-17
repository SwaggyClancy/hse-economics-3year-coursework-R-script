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

