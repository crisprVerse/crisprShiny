.grnaDesignResetOnTargetsFilters <- function(session,
                                             defaults
){
    ## nucleotide content
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_polyT",
        value=defaults[["polyT"]]
    )
    shiny::updateSliderInput(
        session=session,
        inputId="filter_percentGC",
        value=defaults[["percentGC"]]
    )
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_canonicalPam",
        value=defaults[["canonicalPam"]]
    )
    ## alignments
    alnNames <- grep("^n[0-9](_[cp])?$", names(defaults), value=TRUE)
    for (i in alnNames){
        id <- paste0("filter_", i)
        shiny::updateNumericInput(
            session=session,
            inputId=id,
            value=defaults[[i]]
        )
    }
    ## scores
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_conservation",
        value=defaults[["conservation"]]
    )
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_excludeNaScore",
        value=defaults[["excludeNaScore"]]
    )
    scoreNames <- grep("^score_", names(defaults), value=TRUE)
    for (i in scoreNames){
        id <- paste0("filter_", i)
        shiny::updateSliderInput(
            session=session,
            inputId=id,
            value=defaults[[i]]
        )
    }
    ## genomic features
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_inRepeats",
        value=defaults[["inRepeats"]]
    )
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_snps",
        value=defaults[["hasSNP"]]
    )
    shiny::updateCheckboxInput(
        session=session,
        inputId="filter_pfam",
        value=defaults[["pfam"]]
    )
    ## isoforms
    shiny::updateSelectInput(
        session=session,
        inputId="filter_isoforms",
        selected=defaults[["isoforms"]]
    )
    shiny::updateSliderInput(
        session=session,
        inputId="filter_percentCDS",
        value=defaults[["percentCDS"]]
    )
    shiny::updateSliderInput(
        session=session,
        inputId="filter_percentCodingIsoforms",
        value=defaults[["percentCodingIsoforms"]]
    )
    ## promoter
    shiny::updateSelectInput(
        session=session,
        inputId="filter_promoter",
        selected=defaults[["promoter"]]
    )
    shiny::updateSliderInput(
        session=session,
        inputId="filter_distToTss",
        value=defaults[["distToTss"]]
    )
}
