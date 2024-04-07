
# Get channel id ----------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

res_channel <- tuber::yt_search(
  term = "Cientística & Podcast Naruhodo",
  type = "channel"
)

saveRDS(res_channel$channelId, "data-raw/channel_id.rds")


# Get videos --------------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

channel_id <- readRDS("data-raw/channel_id.rds")

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

saveRDS(tab_videos, "data-raw/tab_videos_v1.rds")


# Get missing videos ------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

channel_id <- readRDS("data-raw/channel_id.rds")
tab_videos <- readRDS("data-raw/tab_videos_v1.rds")

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
    .before = "podcast_num"
  ) |>
  dplyr::arrange(podcast_tipo, podcast_num)

total_episodes[!total_episodes %in% tab_videos_completa$podcast_num]

saveRDS(tab_videos_completa, "data-raw/tab_videos_completa.rds")

# Pegar legenda -----------------------------------------------------------

tuber::yt_oauth(
  app_id = Sys.getenv("gcp_app_id"),
  app_secret = Sys.getenv("gcp_app_secret")
)

tab_videos_completa <- readRDS("data-raw/tab_videos_completa.rds")

safe_list_caption_tracks <- purrr::possibly(
  tuber::list_caption_tracks,
  otherwise = NULL
)

pegar_id_legenda <- function(video_id) {

  res <- safe_list_caption_tracks(
    video_id = video_id,
    language = "pt"
  )

  Sys.sleep(1)

  if (is.null(res)) {
    return(NA)
  } else if (nrow(res) == 0) {
    return(NA)
  } else {
    return(res$id)
  }

}

legenda_id <- c()

for (video_id in tab_videos_completa$video_id) {
  legenda_id <- c(legenda_id, pegar_id_legenda(video_id))
}





