
tab_legenda <- readRDS("data-raw/pegar_legenda_yt/tab_legendas.rds")
tab_capas <- readRDS("data-raw/pegar_capas/tab_capas.rds")


tab_legenda |>
  dplyr::left_join(
    tab_capas,
    by = c("podcast_tipo", "podcast_num")
  ) |>
  readr::write_csv("inst/app/dados_nahurodo.csv")
