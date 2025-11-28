#' Shiny Server for censusMaps Package
#'
#' @param id shiny identifier
#' @param census_geometry static data frames
#' @return reactive server
#' @export
#' @rdname censusMaps
#' @importFrom shiny fluidPage mainPanel moduleServer NS radioButtons reactive 
#'             req sidebarPanel tagList titlePanel 
censusMapsServer <- function(id, census_geometry) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    places <- censusServer("census", census_geometry)

    landGgplotServer("landGgplot", input, places)
    landTmapServer("landTmap", input, places)
    landTableServer("landTable", places)
    
    output$geo <- shiny::renderUI({
      if(shiny::req(input$dynamic) == "Static") {
        landGgplotInput(ns("landGgplot"))
      }
    }
                                    )
    output$landPlot <- shiny::renderUI({
      switch(shiny::req(input$dynamic),
        Static  = landGgplotOutput(ns("landGgplot")),
        Dynamic = landTmapOutput(ns("landTmap")))
    })
    
  })
}
#' Shiny Module Input for censusMaps
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname censusMaps
#' @export
censusMapsInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    censusInput(ns("census")),
    shiny::radioButtons(ns("dynamic"), "", c("Static", "Dynamic"),
                        inline = TRUE),
    shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100),
    shiny::uiOutput(ns("geo"))
  )
}
#' Shiny Module Output for censusMaps
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname censusMaps
#' @export
censusMapsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("landPlot"))
}
#' Shiny Module UI for censusMaps
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname censusMaps
#' @export
censusMapsUI <- function(id) {
  ns <- shiny::NS(id)
  landTableOutput(ns("landTable"))
}
#' Shiny Module App for censusMaps
#' @return nothing returned
#' @rdname censusMaps
#' @export
censusMapsApp <- function() {
  census_geometry <- readRDS("data/census_geometry.rds")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      censusMapsInput("censusMaps")
    ),
    shiny::mainPanel(
      censusMapsOutput("censusMaps"),
      censusMapsUI("censusMaps")
    )
  ) 
  server <- function(input, output, session) {
    censusMapsServer("censusMaps", census_geometry)
  }
  shiny::shinyApp(ui, server)
}