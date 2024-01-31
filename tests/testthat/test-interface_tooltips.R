test_that(".addTooltip adds tooltip icon and text to label", {
    # skip test for now
    # expect_equal(...)
})

test_that(".addTooltip does not alter label when tooltip text is not defined", {
    label <- "label"
    expect_equal(.addTooltip(label=label,
                             inputId="BAD_ID"),
                 label)
})



# .initiateDatatableTooltips - must test in Shiny session...



# .getHeaderCallback
test_that(".getHeaderCallback returns a JS_EVAL object", {
    expect_equal(class(.getHeaderCallback(0)), "JS_EVAL")
    expect_equal(class(.getHeaderCallback(1)), "JS_EVAL")
})

test_that(".getHeaderCallback defines tooltip format for each column", {
    tooltip_pattern <- "#TOOLTIP-"
    expect_true(!grepl(tooltip_pattern, .getHeaderCallback(0)))
    
    results <- .getHeaderCallback(5)
    for (i in seq_len(5)){
        pattern <- paste0(tooltip_pattern, i)
        expect_true(grepl(pattern, results))
    }
})



test_that(".dtTooltipText retrieves tooltip text", {
    expect_true(is.character(.dtTooltipText("ID")))
    expect_true(.dtTooltipText("ID") != "\\'ID\\' column")
    expect_true(grepl("azimuth method", .dtTooltipText("score_azimuth"),
                      ignore.case=TRUE))
    expect_true(grepl("^aggregate", .dtTooltipText("score_cfd_aggregate"),
                      ignore.case=TRUE))
    bad <- "NOT_A_COLNAME"
    expect_equal(.dtTooltipText(bad), paste0("\\'", bad, "\\' column"))
})
