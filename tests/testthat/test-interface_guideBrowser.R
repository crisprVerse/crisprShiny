test_that(".guideBrowserUI renders", {
    server <- function(input, output, session){
        out <- shiny::reactive(.guideBrowserUI(session))
        output$out <- shiny::renderUI(out())
    }
    # testServer(server, {
    #     expect_snapshot(out())
    #     output$out
    # })
})



server <- function(gs, gene, geneModel, selectedRows){
    function(input, output, session){
        output$out <- shiny::renderUI({
            .renderGuideBrowserPlotAndCoords(
                guideSet=gs,
                gene="KRAS",
                geneModel=txdb_human,
                selectedRows=1,
                plotWindow="full",
                guideStacking="full"
            )
        })
    }
}

test_that(".renderGuideBrowserPlotAndCoords renders", {
    skip("Long render time for browser plot")
    
    testServer(server(
        gs=guideSetExample_kras,
        gene="KRAS",
        geneModel=txdb_human,
        selectedRows=1
    ), {
        expect_error(output$out, regexp=NA)
    })
    testServer(server(
        gs=guideSetExample_kras,
        gene="",
        geneModel=txdb_human,
        selectedRows=1
    ), {
        expect_error(output$out, regexp=NA)
    })
    testServer(server(
        gs=guideSetExample_kras,
        gene="KRAS",
        geneModel=NULL,
        selectedRows=1
    ), {
        expect_error(output$out, regexp=NA)
    })
    testServer(server(
        gs=guideSetExample_kras,
        gene="KRAS",
        geneModel=txdb_human,
        selectedRows=integer(0)
    ), {
        expect_error(output$out, regexp=NA)
    })
})


## not yet imlemented at this level
# test_that(".renderGuideBrowserPlotAndCoords requires gene_symbol column", {
#     gs <- guideSetExample_kras
#     mcols(gs)[["gene_symbol"]] <- NULL
#     testServer(server(
#         gs=gs,
#         gene="KRAS",
#         geneModel=txdb_human,
#         selectedRows=1
#     ), {
#         output$out
#     })
# })






test_that(".renderBrowserErrorMessage returns correct error message", {
    expect_snapshot(.renderBrowserErrorMessage(list(error="index")))
    expect_snapshot(.renderBrowserErrorMessage(list(error="chr")))
    expect_snapshot(.renderBrowserErrorMessage(list(error="render")))
})
