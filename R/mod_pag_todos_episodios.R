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
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        radioButtons(
          inputId = ns("tipo"),
          label = "Tipo do episódio",
          choices = c("Regular", "Entrevista", "Extra")
        )
      ),
      h2("Todos os episódios", class = "mb-4"),
      reactable::reactableOutput(ns("tabela"))
    )
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
          pagination = TRUE,
          defaultPageSize = 20,
          language = reactable::reactableLang(
            searchPlaceholder = "Procurar",
            searchLabel = "",
            noData = "Nenhuma informação encontrada",
            pageNext = "Próxima",
            pagePrevious = "Anterior",
            pageNumbers = "{page} de {pages}",
            pageInfo = "{rowStart}\u2013{rowEnd} de {rows} episódios",
            pageSizeOptions = "Mostrar {rows}",
            pageNextLabel = "Próxima página",
            pagePreviousLabel = "Página anterior",
            pageNumberLabel = "Página {page}",
            pageJumpLabel = "Ir para a página",
            pageSizeOptionsLabel = "Episódios por página",
            detailsExpandLabel = "Expandir detalhes"
          ),
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
