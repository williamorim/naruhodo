
tab_legenda <- readRDS("data-raw/pegar_legenda_yt/tab_legendas.rds")
tab_capas <- readRDS("data-raw/pegar_capas/tab_capas.rds")


tab_legenda |>
  dplyr::mutate(
    legenda_tratada = legenda_texto |>
      tolower() |>
      stringi::stri_trans_general("Latin-ASCII")
  ) |>
  dplyr::left_join(
    tab_capas,
    by = c("podcast_tipo", "podcast_num")
  ) |>
  readr::write_csv("inst/app/dados_nahurodo.csv")
