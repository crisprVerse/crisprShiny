test_that(".renderAlignmentsTab renders", {
    server <- function(input, output, session){
        alignments <- shiny::reactive("not null")
        output$out <- .renderAlignmentsTab(session, alignments())
    }
    testServer(server, {
        html <- output$out$html
        expect_true(!is.null(html))
    })
})


test_that(".renderAlignmentsParameters renders", {
    savedParameters <- list(
        mmCount=2,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd"
    )
    gs <- guideSetExample_kras[1:50]
    df <- GuideSet2DataFrames(gs)
    server <- function(input, output, session){
        output$out <- .renderAlignmentsParameters(
            session=session,
            primaryData=df$primary,
            alignmentsData=df$alignments,
            nuclease=crisprNuclease(gs),
            savedParameters=savedParameters
        )
    }
    testServer(server, {
        html <- output$out$html
        expect_true(!is.null(html))
    })
})


test_that(".renderAlignmentsSummary renders", {
    gs <- guideSetExample_kras["spacer_1"]
    df <- GuideSet2DataFrames(gs)
    server <- function(input, output, session){
        output$out <- .renderAlignmentsSummary(
            alignments=df$alignments,
            primaryData=df$primary,
            mmCount=2
        )
    }
    testServer(server, {
        html <- output$out$html
        expect_true(!is.null(html))
        expect_true(grepl("On-target", html))
        expect_true(grepl("1-mismatch", html))
        expect_true(grepl("2-mismatch", html))
        expect_true(grepl("3-mismatch", html))
        expect_true(grepl("NA", html)) # mmCount set to 2, 3-mm is NA
    })
})




## move to observers...
server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs[1:50])[["alignments"]] ## remove indices
        nuclease <- crisprDesign::crisprNuclease(gs)
        alignmentsDT <- shiny::reactive(
            .renderAlignmentsDataTable(
                alnData=df$alignments,
                nuclease=nuclease
            )
        )
        output$alignmentsDataTable <- DT::renderDataTable(alignmentsDT())
    }
}

test_that("Alignments DT is not available for minimal GuideSet", {
    gs <- guideSetExample_basic
    testServer(server(gs), {
        ## throws other error
        # expect_error(alignmentsDT(), regexp=NA)
        # expect_error(output$alignmentsDataTable, regexp=NA)
    })
})


test_that("Alignments DT is expected for annotated CRISPRko GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(gs), {
        # x <- alignmentsDT()$x
        ## data
        cols <- c("ID", "alignmentType", "mismatchesAndPam", "score_cfd",
                  "score_mit", "genomicContext", "pam_site")
        # expect_setequal(colnames(x$data), cols)
        # expect_equal(nrow(x$data), 4) # get actual value
        ## DT settings
        # expect_equal(x$filter, "none")
        # expect_equal(x$selection$mode, "none")
        # html <- output$alignmentsDataTable$html
    })
})





test_that(".renderAlignmentsBrowser renders", {
    alns <- .formatAlignmentsData(
        alignments=GuideSet2DataFrames(guideSetExample_kras)[["alignments"]],
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(guideSetExample_kras)
    )
    selectedRow <- 1
    geneModel <- txdb_kras
    server <- function(input, output, session){
        out <- shiny::reactive(
            .renderAlignmentsBrowser(
                alignments=alns,
                selectedRow=selectedRow,
                guideSet=guideSetExample_kras,
                geneModel=geneModel
            ))
        output$out <- shiny::renderUI(out())
    }
    testServer(server, {
        # expect_snapshot(out())
        html <- output$out$html
        expect_true(!is.null(html))
    })
})
