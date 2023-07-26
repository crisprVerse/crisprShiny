#' @title UI component for crisprShiny App
#' @description UI component for crisprShiny App. Not intended for direct use.
#' 
#' @param id Module ID.
#' @param cssFile Optional path of CSS file to be included in rendering app.
#'     `NULL` gives default styling, found in `inst/www/styling.css`.
#' 
#' @return Shiny module UI component.
#' 
#' @examples
#' library(crisprShiny)
#' ui <- function(id){
#'     fluidPage(
#'         crisprUI(id)
#'     )
#' }
#' 
#' server <- function(id, gs){
#'     function(input, output, session){
#'         observeEvent(gs, {
#'             crisprServer(
#'                 id,
#'                 guideSet=gs,
#'                 geneModel=NULL,
#'                 useFilterPresets=TRUE
#'             )
#'         })
#'     }
#' }
#' 
#' myApp <- function(gs){
#'     shinyApp(ui=ui("id"), server=server("id", gs))
#' }
#' 
#' if (interactive()){
#'     data("guideSetExample_basic", package="crisprShiny")
#'     myApp(guideSetExample_basic)
#' }
#'
#' @export
crisprUI <- function(id,
                     cssFile=NULL
){
    if (is.null(cssFile)){
        cssFile <- system.file( # best practice?
            "www/styling.css",
            package="crisprShiny",
            mustWork=TRUE
        )
    }
    qtip2File <- system.file(
        "www/qtip2.js",
        package="crisprShiny",
        mustWork=TRUE
    )
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::includeCSS(cssFile),
        shiny::includeScript(qtip2File),
        shinyjs::useShinyjs(),
        waiter::use_waiter(),
        shiny::uiOutput(ns("fullUI"))
    )
}
