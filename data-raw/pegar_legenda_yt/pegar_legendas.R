
# Get channel id ----------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

res_channel <- tuber::yt_search(
  term = "Cientística & Podcast Naruhodo",
  type = "channel"
)

saveRDS(res_channel$channelId, "data-raw/pegar_legenda_yt/channel_id.rds")


# Get videos --------------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

channel_id <- readRDS("data-raw/pegar_legenda_yt/channel_id.rds")

res_videos_pt1 <- tuber::yt_search(
  term = "Naruhodo",
  channel_id = channel_id,
  type = "video",
  published_after = "2020-01-01T00:00:00Z",
  published_before = "2022-01-01T00:00:00Z"
)

res_videos_pt2 <- tuber::yt_search(
  term = "Naruhodo",
  channel_id = channel_id,
  type = "video",
  published_after = "2022-01-01T00:00:00Z",
  published_before = "2025-01-01T00:00:00Z"
)

tab_videos <- rbind(
  res_videos_pt1,
  res_videos_pt2
) |>
  dplyr::as_tibble() |>
  dplyr::select(
    video_name = title,
    video_id = video_id,
    video_pub_date = publishedAt
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(video_pub_date) |>
  # dplyr::filter(stringr::str_detect(video_name, "Naruhodo")) |> View()
  dplyr::filter(
    stringr::str_detect(
      video_name,
      "Naruhodo #|Naruhodo Entrevista|Naruhodo Extra"
    )
  ) |>
  dplyr::filter(
    !stringr::str_detect(
      video_name,
      "#shorts"
    )
  ) |>
  dplyr::mutate(
    podcast_num = as.numeric(stringr::str_extract(video_name, "#[0-9]+"))
  )

saveRDS(tab_videos, "data-raw/pegar_legenda_yt/tab_videos_v1.rds")


# Get missing videos ------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

channel_id <- readRDS("data-raw/pegar_legenda_yt/channel_id.rds")
tab_videos <- readRDS("data-raw/pegar_legenda_yt/tab_videos_v1.rds")

total_episodes <- 1:414
missing_episodes <- total_episodes[!total_episodes %in% tab_videos$podcast_num]

tab_missing <- NULL

for (ep in missing_episodes) {

  res <- tuber::yt_search(
    term = glue::glue("Naruhodo #{ep}"),
    channel_id = channel_id,
    type = "video",
    published_after = "2020-01-01T00:00:00Z",
    max_results = 1,
    get_all = FALSE
  )

  Sys.sleep(1)

  tab_missing <- rbind(
    tab_missing,
    res
  )

}

tab_videos_completa <- tab_missing |>
  tibble::as_tibble() |>
  dplyr::select(
    video_name = title,
    video_id = video_id,
    video_pub_date = publishedAt
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    podcast_num = as.numeric(stringr::str_extract(video_name, "[0-9]+")),
  ) |>
  rbind(tab_videos) |>
  dplyr::mutate(
    podcast_tipo = dplyr::case_when(
      stringr::str_detect(tolower(video_name), "entrevista") ~ "Entrevista",
      stringr::str_detect(tolower(video_name), "extra") ~ "Extra",
      TRUE ~ "Regular"
    ),
    .before = "podcast_num",
    legenda_track_id = NA_character_,
    legenda = NA_character_
  ) |>
  dplyr::arrange(podcast_tipo, podcast_num)

total_episodes[!total_episodes %in% tab_videos_completa$podcast_num]

saveRDS(tab_videos_completa, "data-raw/pegar_legenda_yt/tab_videos_completa.rds")

# Pegar url das legendas  -------------------------------------------------

tab_videos_completa <- readRDS("data-raw/pegar_legenda_yt/tab_videos_completa.rds")

pegar_url_legenda <- function(video_id) {

  Sys.sleep(2)

  res <- httr2::request("https://www.youtube.com/watch") |>
    httr2::req_url_query(v = video_id) |>
    httr2::req_perform()

  res |>
    httr2::resp_body_string() |>
    stringr::str_extract('"https://www.youtube.com/api/timedtext(.+?)"') |>
    stringr::str_remove_all('"') |>
    stringi::stri_unescape_unicode()
}

tab <- tab_videos_completa |>
  dplyr::mutate(
    legenda_url = purrr::map_chr(
      video_id,
      pegar_url_legenda,
      .progress = TRUE
    )
  )

saveRDS(tab, "data-raw/pegar_legenda_yt/tab_videos_com_legenda.rds")

sem_url <- tab |>
  dplyr::filter(is.na(legenda_url)) |>
  dplyr::pull(video_id)


# Pegar legenda -----------------------------------------------------------

tab <- readRDS("data-raw/pegar_legenda_yt/tab_videos_com_legenda.rds")

pegar_legenda <- function(url) {

  if (is.na(url)) {
    return(NA_character_)
  }

  res_legenda <- httr2::request(url) |>
    httr2::req_perform()

  res_legenda |>
    httr2::resp_body_xml() |>
    xml2::xml_text()
}


tab_legenda <- tab |>
  dplyr::mutate(
    legenda = purrr::map_chr(
      legenda_url,
      pegar_legenda,
      .progress = TRUE
    )
  )

tab_legenda |> dplyr::filter(is.na(legenda))

tab_legenda |>
  dplyr::select(
    video_name,
    video_id,
    video_pub_date,
    podcast_tipo,
    legenda_url,
    legenda_texto = legenda
  ) |>
  saveRDS("data-raw/dados_nahurodo.rds")















