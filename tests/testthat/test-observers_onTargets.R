data("tooltipAnnotation")

server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs)[["primary"]]
        nuclease <- crisprDesign::crisprNuclease(gs)
        onTargetsDataTable <- shiny::reactive(
            .onTargetsDataTable(
                data=df,
                nuclease=nuclease
            )
        )
        output$primaryDataTable <- DT::renderDataTable(onTargetsDataTable())
    }
}

# test_that(".onTargetsModule generates expected UI", {
#     expect_snapshot(.onTargetsModule(MockShinySession$new()))
#     # testServer(server, {
#     #     expect_snapshot(.onTargetsModule(session))
#     # })
# })



test_that("On-targets DT is expected for minimal GuideSet", {
    gs_list <- list(guideSetExample_basic)#,
                    # ntc_simple) # doesnt work?
    lapply(gs_list, function(gs){
        testServer(server(gs), {
            x <- onTargetsDataTable()$x
            ## data
            cols <- c("ID", "chr", "strand", colnames(mcols(gs)))
            expect_setequal(colnames(x$data), cols)
            expect_equal(nrow(x$data), length(gs))
            ## DT settings
            expect_equal(x$filter, "none")
            expect_true(all(c("Buttons", "FixedColumns") %in% x$extensions))
            expect_equal(x$selection$mode, "multiple")
            expect_error(output$primaryDataTable, regexp=NA)
        })
    })
})


test_that("On-targets DT is expected for fully-annotated CRISPRko GuideSet", {
    gs_list <- list(guideSetExample_kras,
                    ntc_kras)
    lapply(gs_list, function(gs){
        testServer(server(guideSetExample_kras), {
            x <- onTargetsDataTable()$x
            ## data
            cols <- c("ID", "chr", "strand", colnames(mcols(gs)))
            cols[cols == "score_cfd"] <- "score_cfd_aggregate"
            cols[cols == "score_mit"] <- "score_mit_aggregate"
            listCols <- c("alignments", "enzymeAnnotation", "geneAnnotation",
                          "tssAnnotation", "snps")
            cols <- setdiff(cols, listCols)
            expect_setequal(colnames(x$data), cols)
            # expect_equal(nrow(x$data), length(gs)) # NTCs are filtered out...
            ## DT settings
            expect_equal(x$filter, "none")
            expect_true(all(c("Buttons", "FixedColumns") %in% x$extensions))
            expect_equal(x$selection$mode, "multiple")
            output$primaryDataTable
        })
    })
})

## additional tests for other GuideSet examples (be)