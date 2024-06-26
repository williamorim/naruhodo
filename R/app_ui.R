#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "Ache um Naruhodo!",
      bg = "#f93d36",
      underline = TRUE,
      theme = bslib::bs_theme(
        "navbar-light-color" = "white",
        "navbar-light-hover-color" = "#ffffffdd",
        "navbar-light-active-color" = "white"
      ),
      bslib::nav_panel(
        title = "Todos os episódios",
        mod_pag_todos_episodios_ui("pag_todos_episodios_1")
      ),
      bslib::nav_panel(
        title = "Procurar por termo",
        mod_pag_procurar_episodio_ui("pag_procurar_episodio_1")
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        tags$a(
          href = "https://wamorim.com",
          target = "_blank",
          "Desenvolvido por William Amorim"
        )
      ),
      bslib::nav_item(
        tags$a(
          href = "https://github.com/williamorim/naruhodo",
          target = "_blank",
          bsicons::bs_icon("github")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    # favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Ache um Naruhodo!"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
