server <- function(gs){
    function(input, output, session){
        defaults <- shiny::reactive(
            .getFilterDefaults(gs,
                               useFilterPresets=FALSE)
        )
        filterUI <- shiny::reactive(
            .guideSetFilters(
                session=session,
                guideSet=gs,
                savedValues=defaults()
            )
        )
        output$out <- renderUI({
            filterUI()
        })
    }
}


test_that(".guideSetFilters renders", {
    testServer(server(guideSetExample_basic), {
        # expect_snapshot(filterUI())
        expect_error(output$out, regexp=NA)
    })
    testServer(server(guideSetExample_kras), {
        # expect_snapshot(filterUI())
        expect_error(output$out, regexp=NA)
    })
    testServer(server(guideSetExample_kras_be), {
        # expect_snapshot(filterUI())
        expect_error(output$out, regexp=NA)
    })
})
