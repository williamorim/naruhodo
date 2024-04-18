#' pag_procurar_episodio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pag_procurar_episodio_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(3, 4),
      radioButtons(
        inputId = ns("tipo"),
        label = "Tipo do episódio",
        inline = TRUE,
        choices = c("Regular", "Entrevista", "Extra")
      ),
      textInput(
        inputId = ns("texto"),
        label = "Termo",
        value = ""
      )
    ),
    reactable::reactableOutput(ns("tabela"))
  )
}

#' pag_procurar_episodio Server Functions
#'
#' @noRd
mod_pag_procurar_episodio_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela <- reactable::renderReactable({

      termo <- input$texto |>
        tolower() |>
        stringr::str

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
# mod_pag_procurar_episodio_ui("pag_procurar_episodio_1")

## To be copied in the server
# mod_pag_procurar_episodio_server("pag_procurar_episodio_1")
