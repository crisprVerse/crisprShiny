## use shinytest package


server <- function(input, output, session){
    defaults <- shiny::reactive(.getFilterDefaults(
        guideSetExample_kras,
        useFilterPresets=FALSE
    ))
    filterValues <- shiny::reactive(.setFilterValues(
        defaults=defaults(),
        input=input
    ))
    output$txt <- renderText(paste(filterValues()))
    observeEvent(input$test, {
        .grnaDesignResetOnTargetsFilters(session, defaults())
        # print(paste(filterValues()))
    })
}



test_that(".grnaDesignResetOnTargetsFilters sets filters to default values", {
    testServer(server, {
        .grnaDesignResetOnTargetsFilters(session, defaults())
        expect_true(identical(filterValues(), defaults()))
        session$setInputs(filter_polyT=TRUE,
                          filter_percentGC=c(40, 60),
                          filter_score_exon=1)
        expect_false(identical(filterValues(), defaults()))
        # .grnaDesignResetOnTargetsFilters(session, defaults())
        session$setInputs(test=TRUE)
        # session$flushReact()
        # print(output$txt)
        # expect_e(identical(filterValues(), defaults()))
    })
})
