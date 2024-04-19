#' pag_procurar_episodio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pag_procurar_episodio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        radioButtons(
          inputId = ns("tipo"),
          label = "Tipo do episódio",
          choices = c("Regular", "Entrevista", "Extra")
        ),
        textInput(
          inputId = ns("texto"),
          label = "Termo",
          value = ""
        ),
        actionButton(
          inputId = ns("pesquisar"),
          label = "Pesquisar",
          icon = icon("magnifying-glass")
        )
      ),
      h2("Procurar por termo", class = "mb-4"),
      uiOutput(ns("num_encontrados"))  |>
        shinycssloaders::withSpinner(
          color = "#f93d36",
          type = 6,
          proxy.height = "100px"
        ),
      uiOutput(ns("cards"))
    )
  )
}

#' pag_procurar_episodio Server Functions
#'
#' @noRd
mod_pag_procurar_episodio_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    termo <- reactive({
      texto <- glue::glue(" {input$texto} ")

      texto |>
        tolower() |>
        stringi::stri_trans_general("Latin-ASCII")
    })

    tab_epi <- eventReactive(input$pesquisar, {
      dados |>
        dplyr::filter(
          podcast_tipo == input$tipo
        ) |>
        dplyr::mutate(
          num_ocorrencias = stringr::str_count(
            legenda_tratada,
            stringr::fixed(termo())
          ),
          video_link = glue::glue("https://www.youtube.com/watch?v={video_id}")
        ) |>
        dplyr::filter(
          num_ocorrencias > 0
        ) |>
        dplyr::arrange(desc(num_ocorrencias))
    })

    output$num_encontrados <- renderUI({
      validate(need(
        isTruthy(input$pesquisar),
        'Clique no botão "Pesquisar"" para ver os episódios.'
      ))

      num <- nrow(tab_epi())

      texto <- if (num == 0) {
        " Nenhum episódio encontrado"
      } else if (num == 1) {
        " 1 episódio encontrado"
      } else {
        glue::glue(" {num} episódios encontrados")
      }

      p(
        icon("magnifying-glass"),
        texto
      )

    })

    output$cards <- renderUI({

      req(nrow(tab_epi()) > 0)

      tab <- tab_epi()

      bslib::layout_column_wrap(
        width = "650px",
        !!!purrr::pmap(
          list(
            episodio = tab$video_name,
            video_link = tab$video_link,
            img_link = tab$podcast_img_link,
            num_ocorrencias = tab$num_ocorrencias
          ),
          ~ naru_card(..1, ..2, ..3, ..4)
        )
      )

    })

  })
}

naru_card <- function(episodio, video_link, img_link, num_ocorrencias) {
  tags$a(
    class = "narucard",
    bslib::value_box(
      title = episodio,
      style = "max-width: 800px;",
      value = glue::glue("Número de ocorrência do termo: {num_ocorrencias}"),
      showcase = img(
        src = img_link,
        width = "100%",
        style = "max-width: 380px;"
      ),
      min_height = "200px",
      fill = FALSE
    ),
    href = video_link,
    target = "_blank"
  )

}

## To be copied in the UI
# mod_pag_procurar_episodio_ui("pag_procurar_episodio_1")

## To be copied in the server
# mod_pag_procurar_episodio_server("pag_procurar_episodio_1")
