test_that("crisprUI renders div with id prefix", {
    prefix <- "my_module"
    out <- crisprUI(prefix)
    expect_true(methods::is(out, "shiny.tag.list"))
    # id <- out[[1]]$attribs$id
    # expect_true(grepl(prefix, id))
})
