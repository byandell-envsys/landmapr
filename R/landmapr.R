#' Shiny Server for landmapr Package
#'
#' @param id shiny identifier
#' @param nativeLandSlug,nativeLandUS,census_geometry static data frames
#' @return reactive server
#' @export
#' @rdname landmapr
#' @importFrom shiny fluidPage mainPanel moduleServer NS radioButtons reactive 
#'             req sidebarPanel tagList titlePanel 
landmaprServer <- function(id,
                           nativeLandSlug, nativeLandUS, census_geometry) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    census_places <- censusServer("census", census_geometry)
    nativeLand_places <- nativeLandServer("nativeLand",
      nativeLandSlug, nativeLandUS, census_geometry)
    
    places <- shiny::reactive({
      order_places(nativeLand_places(), census_places())
    })
    
    landGgplotServer("landGgplot", input, places)
    landTmapServer("landTmap", input, places)
    landTableServer("landTable", places)
    
    output$geo <- shiny::renderUI({
      if(shiny::req(input$dynamic) == "Static") {
        landGgplotInput(ns("landGgplot"))
      }
    })
    output$landPlot <- shiny::renderUI({
      switch(shiny::req(input$dynamic),
        Static  = landGgplotOutput(ns("landGgplot")),
        Dynamic = landTmapOutput(ns("landTmap")))
    })
    
  })
}
#' Shiny Module Input for landmapr
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landmapr
#' @export
landmaprInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    censusInput(ns("census")),
    nativeLandInput(ns("nativeLand")),
    shiny::radioButtons(ns("dynamic"), "", c("Static", "Dynamic"),
                        inline = TRUE),
    shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100),
    shiny::uiOutput(ns("geo"))
  )
}
#' Shiny Module Output for landmapr
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landmapr
#' @export
landmaprOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("landPlot"))
}
#' Shiny Module UI for landmapr
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landmapr
#' @export
landmaprUI <- function(id) {
  ns <- shiny::NS(id)
  landTableOutput(ns("landTable"))
}
#' Shiny Module App for landmapr
#' @return nothing returned
#' @rdname landmapr
#' @export
landmaprApp <- function() {
  nativeLandSlug <- readRDS("data/NativeLandSlug.rds")
  nativeLandUS <- readRDS("data/nativeLandUS.rds")
  census_geometry <- readRDS("data/census_geometry.rds")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      landmaprInput("landmapr")
    ),
    shiny::mainPanel(
      landmaprOutput("landmapr"),
      landmaprUI("landmapr")
    )
  ) 
  server <- function(input, output, session) {
    landmaprServer("landmapr",
      nativeLandSlug, nativeLandUS, census_geometry)
  }
  shiny::shinyApp(ui, server)
}