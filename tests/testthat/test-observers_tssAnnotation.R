server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs)[["tssAnnotation"]] ## remove indices
        tssAnnotationDT <- shiny::reactive(
            .tssAnnotationDataTable(
                data=df,
                id=names(gs)[1]
            )
        )
        output$tssAnnotationDataTable <- DT::renderDataTable(tssAnnotationDT())
    }
}



test_that("TSS Annotation DT is not available for minimal GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(gs), {
        expect_error(tssAnnotationDT(), regexp=NA)
        expect_error(output$tssAnnotationDataTable, regexp=NA)
    })
})


test_that("TSS Annotation DT is empty for annotated CRISPRko GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(guideSetExample_kras), {
        x <- tssAnnotationDT()$x
        ## data
        cols <- c("ID", colnames(tssAnnotation(gs)))
        expect_setequal(colnames(x$data), cols)
        expect_equal(nrow(x$data), 0)
        ## DT settings
        expect_equal(x$filter, "none")
        expect_true("FixedColumns" %in% x$extensions)
        expect_equal(x$selection$mode, "none")
        output$tssAnnotationDataTable
    })
})

## additional tests for other GuideSet examples (be, ai)