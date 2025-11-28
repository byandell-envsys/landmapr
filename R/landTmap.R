#' Shiny Server for Thematic Map Plot
#'
#' @param id shiny identifier
#' @param mainpar reactiveValue with height
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landTmap
#' @importFrom shiny fluidPage mainPanel moduleServer NS reactive req
#'             sliderInput sidebarPanel titlePanel uiOutput
#' @importFrom tmap renderTmap tmapOutput
landTmapServer <- function(id, mainpar, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Toggle on TMAP View.
    # See <https://r-graph-gallery.com/package/tmap.html>.
    tmap::tmap_mode("view")
    
    valid_places <- shiny::reactive({
      if(shiny::isTruthy(places()) && nrow(places())) {
        sf::st_make_valid(places())
      } else {
        NULL
      }
    })
    color <- shiny::reactive({
      shiny::req(valid_places())
      color <- unique(valid_places()$color)
      names(color) <- color
      color
    })
    # Output Main Plot
    output$tmap_wrapper <- tmap::renderTmap({
      # https://rdrr.io/cran/tmap/man/renderTmap.html
      tmap_wrapper(valid_places())
    })
    output$tmap <- shiny::renderUI({
      shiny::req(mainpar$height)
      if(shiny::isTruthy(valid_places()) && nrow(valid_places())) {
        tmap::tmapOutput(ns("tmap_wrapper"),
                         height = paste0(mainpar$height, "px"))
      } else {
        NULL
      }
    })
  })
}
#' Shiny Module Output for landTmap
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landTmap
#' @export
landTmapOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("tmap"))
}
#' Shiny Module App for landTmap
#' @return nothing returned
#' @rdname landTmap
#' @export
landTmapApp <- function() {
  census_geometry <- readRDS("data/census_geometry.rds")

  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      censusInput("census"),
      shiny::sliderInput("height", "Height:", 300, 800, 500, 100)
    ),
    shiny::mainPanel(
      landTmapOutput("landTmap")
    )
  ) 
  server <- function(input, output, session) {
    census_places <- censusServer("census", census_geometry)
    landTmapServer("landTmap", input, census_places)
  }
  shiny::shinyApp(ui, server)
}