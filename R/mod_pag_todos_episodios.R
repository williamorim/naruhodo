#' pag_todos_episodios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pag_todos_episodios_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("tipo"),
      label = "Tipo do episódio",
      inline = TRUE,
      choices = c("Regular", "Entrevista", "Extra")
    ),
    reactable::reactableOutput(ns("tabela"))
  )
}

#' pag_todos_episodios Server Functions
#'
#' @noRd
mod_pag_todos_episodios_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$tabela <- reactable::renderReactable({
      dados |>
        dplyr::filter(
          podcast_tipo == input$tipo
        ) |>
        dplyr::mutate(
          video_link = glue::glue(
            "https://www.youtube.com/watch?v={video_id}"
          )
        ) |>
        dplyr::select(
          video_name,
          video_link
        ) |>
        reactable::reactable(
          sortable = FALSE,
          pagination = FALSE,
          columns = list(
            video_name = reactable::colDef(
              name = "Episódio",
              filterable = TRUE
            ),
            video_link = reactable::colDef(
              name = "Link",
              maxWidth = 450,
              cell = function(value) {
                tags$a(href = value, value, target = "_blank")
              },
              html = TRUE
            )
          )
        )
    })

  })
}

## To be copied in the UI
# mod_pag_todos_episodios_ui("pag_todos_episodios_1")

## To be copied in the server
# mod_pag_todos_episodios_server("pag_todos_episodios_1")
