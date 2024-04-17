url <- "https://www.b9.com.br/shows/naruhodo?pagina={pag}#anchor-tabs"

links <- c()

for (pag in 1:32) {

  Sys.sleep(1)

  res <- url |>
    glue::glue() |>
    httr2::request() |>
    httr2::req_perform()

  img_links <- res |>
    httr2::resp_body_html() |>
    rvest::html_elements(css = ".c-podcast-image-card") |>
    rvest::html_attr("src")

  links <- c(links, img_links)

}

tibble::tibble(
  podcast_img_link = c(
    links,
    "https://assets.b9.com.br/wp-content/uploads/2019/08/naruhodo-capa-retangulo-196-700x394.png"
  )
) |>
  dplyr::filter(
    !stringr::str_detect(podcast_img_link, "replay")
  ) |>
  dplyr::mutate(
    podcast_tipo = dplyr::case_when(
      stringr::str_detect(
        podcast_img_link,
        "entrevista"
      ) ~ "Entrevista",
      stringr::str_detect(
        podcast_img_link,
        "extra|especil"
      ) ~ "Extra",
      TRUE ~ "Regular"
    ),
    podcast_num = podcast_img_link |>
      stringr::str_extract("-[0-9]*[-.a]") |>
      stringr::str_remove_all("[-.a]") |>
      stringr::str_sub(1, 4) |>
      stringr::str_remove("^00") |>
      stringr::str_remove("^0") |>
      stringr::str_sub(1, 3) |>
      as.numeric()
  ) |>
  dplyr::arrange(
    podcast_tipo, podcast_num
  ) |>
  dplyr::filter(podcast_img_link != "Extra") |>
  dplyr::distinct(podcast_tipo, podcast_num, .keep_all = TRUE) |>
  saveRDS("data-raw/pegar_capas/tab_capas.rds")










