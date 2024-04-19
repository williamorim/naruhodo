#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dados <- readr::read_csv(
    app_sys("app/dados_nahurodo.csv")
  )

  mod_pag_todos_episodios_server("pag_todos_episodios_1", dados)
  mod_pag_procurar_episodio_server("pag_procurar_episodio_1", dados)

}
