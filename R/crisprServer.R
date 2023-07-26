#' @title Server component for crisprShiny App
#' @description Server component for crisprShiny App. Not intended for direct
#'     use. (intended to be passed as server component of module in shinyApp function)
#' 
#' @param id Module ID.
#' @param title Optional title to display at head of app.
#' @param guideSet A GuideSet object.
#' @param geneModel A GRangesList object obtained using
#'     \code{crisprDesign::TxDb2GRangesList}.
#' @param useFilterPresets Whether to use preset filter values on app launch.
#'     See details section for \code{?crisprShiny}.
#'     
#' @return Shiny module server component.
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
#' @importFrom crisprDesign GuideSet2DataFrames crisprNuclease
#' @importFrom DT renderDataTable
#' @importFrom shinyjs toggleElement
crisprServer <- function(id,
                         guideSet,
                         geneModel,
                         title=NULL,
                         useFilterPresets=TRUE
){
    shiny::moduleServer(id, function(input, output, session){
        
        ## variables
        nuclease <- crisprDesign::crisprNuclease(guideSet)
        
        filter_defaults <- shiny::reactiveVal({
            .getFilterDefaults(
                guideSet=guideSet,
                useFilterPresets=useFilterPresets
            )
        })
        
        filter_saved <- shiny::reactiveVal({
            shiny::req(filter_defaults())
            .setFilterValues(
                defaults=filter_defaults(),
                input=input
            )
        })
        
        guideSet_filtered <- shiny::reactiveVal({
            shiny::req(filter_saved())
            .applyGuideSetFilters(
                guideSet=guideSet,
                filterValues=filter_saved()
            )
        })
        
        dataframes <- shiny::reactive({
            crisprDesign::GuideSet2DataFrames(guideSet_filtered())
        })
        
        guideIds <- shiny::reactive({
            dataframes()[["primary"]][["ID"]]
        })
        
        
        ## modules =========================================================
        
        output$fullUI <- shiny::renderUI({
            shiny::req(dataframes())
            .fullUI(
                session=session,
                title=title,
                dataframes=dataframes()
            )
        })
        
        output$annotations <- shiny::renderUI({
            .annotationsModule(
                session=session,
                dataframes=dataframes()
            )
        })
        
        
        shiny::observeEvent(guideIds(), {
            shiny::req(guideIds())
            shiny::updateSelectizeInput(
                session=session,
                inputId="annotationsGuideSelect",
                choices=guideIds(),
                selected=guideIds()[1],
                server=TRUE
            )
        })
        
        
        
        ## table of on-targets =============================================
        
        output$primaryTable <- .onTargetsModule(session)
        
        
        onTargetsDataTable <- shiny::reactive(
            .onTargetsDataTable(
                data=dataframes()[["primary"]],
                nuclease=nuclease
            )
        )
        
        output$primaryDataTable <- DT::renderDataTable(onTargetsDataTable())
        
        
        ## show on-target filters
        shiny::observeEvent(input$onTargetsFilters, {
            # move to utils_filters.R? (also observer)
            shiny::showModal(
                .guideSetFilters(
                    session=session,
                    guideSet=guideSet,
                    savedValues=filter_saved()
                )
            )
        })
        ## apply on-target filters
        shiny::observeEvent(input$filterButton_confirm, {
            filter_saved(
                .setFilterValues(defaults=filter_defaults(),
                                 input=input)
            )
            guideSet_filtered(
                .applyGuideSetFilters(
                    guideSet=guideSet,
                    filterValues=filter_saved()
                )
            )
            shiny::removeModal()
        })
        ## reset on-target filters
        shiny::observeEvent(input$filterButton_reset, {
            .grnaDesignResetOnTargetsFilters(
                session=session,
                defaults=filter_defaults()
            )
        })
        # cancel on-target filters
        shiny::observeEvent(input$filterButton_cancel, {
            shiny::removeModal()
        })
        
        
        
        
        ## guide browser plot ==============================================
        output$guideBrowser <- shiny::renderUI({
            .guideBrowserUI(session)
        })
        
        output$guideBrowserPlot <- shiny::bindEvent(
            shiny::renderUI({
                gene <- input$guideBrowserGene
                plotWindow <- input$guideBrowserWindow
                guideStacking <- input$guideBrowserStacking
                shiny::req(plotWindow, guideStacking)
                .renderGuideBrowserPlotAndCoords(
                    guideSet=guideSet_filtered(),
                    gene=gene,
                    geneModel=geneModel,
                    selectedRows=input$primaryDataTable_rows_selected,
                    plotWindow=plotWindow,
                    guideStacking=guideStacking
                )
            }),
            input$guideBrowserButton,
            ignoreInit=TRUE
        )
        
        shiny::observeEvent(input$guideBrowserGene, {
            genes <- S4Vectors::mcols(guideSet)[["gene_symbol"]]
            genes <- c("", sort(unique(genes)))
            shiny::updateSelectizeInput(
                session=session,
                inputId="guideBrowserGene",
                choices=genes,
                selected="",
                server=TRUE
            )
        }, once=TRUE)
        
        
        
        ## alignments ======================================================
        output$alignmentsTab <- .renderAlignmentsTab(
            session=session,
            alignments=dataframes()$alignments
        )
        
        output$alignmentsParameters <- shiny::renderUI({
            # can't remove renderUI?
            .renderAlignmentsParameters(
                session=session,
                primaryData=dataframes()$primary,
                alignmentsData=dataframes()$alignments,
                nuclease=nuclease,
                savedParameters=shiny::isolate(alignments_parameters())
            )
        })
        
        ## reactive of saved parameters
        alignments_parameters <- shiny::eventReactive(
            input$alignmentsOptionsButton, {
                .getAlignmentParameters(
                    primaryData=dataframes()$primary,
                    nuclease=nuclease,
                    mmCount=input$alignmentsMismatchCount,
                    targetRegion=input$alignmentsTargetRegion,
                    canonicalPam=input$alignmentsCanonical,
                    scoreSorting=input$alignmentsScoreSorting
                )
            },
            ignoreNULL=FALSE
        )
        
        alignments_processed <- reactive({
            shiny::req(dataframes()$alignments,
                       alignments_parameters(),
                       input$annotationsGuideSelect)
            .formatAlignmentsData(
                alignments=dataframes()$alignments,
                id=input$annotationsGuideSelect,
                mmCount=alignments_parameters()$mmCount,
                targetRegion=alignments_parameters()$targetRegion,
                canonicalPam=alignments_parameters()$canonicalPam,
                scoreSorting=alignments_parameters()$scoreSorting,
                nuclease=nuclease
            )
        })
        
        output$alignmentsSummary <- shiny::renderUI({  # can't remove renderUI?
            .renderAlignmentsSummary(
                alignments=alignments_processed(),
                primaryData=dataframes()$primary,
                mmCount=alignments_parameters()$mmCount
            )
        })
        
        output$alignmentsDataTable <- DT::renderDataTable({
            .renderAlignmentsDataTable(
                alnData=alignments_processed(),
                nuclease=nuclease
            )
        })
        
        output$alignmentsBrowser <- shiny::renderUI({
            .renderAlignmentsBrowser(
                alignments=alignments_processed(),
                selectedRow=input$alignmentsDataTable_rows_selected,
                guideSet=guideSet_filtered(),
                geneModel=geneModel
            )
        })
        
        
        ## gene annotation =================================================
        output$geneAnnotationTab <- .renderGeneAnnotationTab(
            session=session,
            data=dataframes()[["geneAnnotation"]]
        )
        
        geneAnnotationDT <- shiny::reactive(
            .geneAnnotationDataTable(
                data=dataframes()[["geneAnnotation"]],
                id=input$annotationsGuideSelect
            )
        )
        
        output$geneAnnotationDataTable <- DT::renderDataTable(
            geneAnnotationDT()
        )
        
        
        
        ## TSS annotation ==================================================
        output$tssAnnotationTab <- .renderTssAnnotationTab(
            session=session,
            data=dataframes()[["tssAnnotation"]]
        )
        
        tssAnnotationDT <- shiny::reactive(
            .tssAnnotationDataTable(
                data=dataframes()[["tssAnnotation"]],
                id=input$annotationsGuideSelect
            )
        )
        
        output$tssAnnotationDataTable <- DT::renderDataTable(tssAnnotationDT())
        
        
        
        ## enzyme annotation ===============================================
        output$enzymeAnnotationTab <- .renderEnzymeAnnotationTab(
            session=session,
            data=dataframes()[["enzymeAnnotation"]]
        )
        
        enzymeAnnotationDT <- shiny::reactive(
            .enzymeAnnotationDataTable(
                data=dataframes()[["enzymeAnnotation"]],
                id=input$annotationsGuideSelect
            )
        )
        
        output$enzymeAnnotationDataTable <- DT::renderDataTable(
            enzymeAnnotationDT()
        )
        
        
        
        ## snps ============================================================
        output$snpsTab <- .renderSnpsTab(
            session=session,
            data=dataframes()[["snps"]]
        )
        
        snpsDT <- shiny::reactive(
            .snpsDataTable(
                data=dataframes()[["snps"]],
                id=input$annotationsGuideSelect
            )
        )
        
        output$snpsDataTable <- DT::renderDataTable(snpsDT())
        
        
        
        ## end module ======================================================
        shiny::reactive(NULL)
    })
}
