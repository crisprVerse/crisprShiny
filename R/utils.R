
SYMBOLS <- list(
    PRIME="\u2032"
)



.validateGeneModel <- function(x
){
    if (is.null(x)){
        return()
    }
    stopifnot("geneModel must be a GRangesList object." = {
        methods::is(x, "GRangesList")
    })
    fields <- c("transcripts", "exons", "cds", "fiveUTRs", "threeUTRs",
                "introns", "tss")
    if (!all(fields %in% names(x))){
        stop("geneModel is missing required genomic regions. ",
             "Use crisprDesign::TxDb2GRangesList to get a properly formatted ",
             "GRangesList object.")
    }
}






# grammar
.plural <- function(word="",
                    amount
){
    plural <- switch(word,
                     "is"="are",
                     "was"="were",
                     "does"="do",
                     "goes"="go",
                     "has"="have",
                     paste0(word, "s")) # some require "-es", "-ies", etc
    if (grepl("chs$", plural)){
        plural <- gsub("s$", "es", plural)
    }
    word <- ifelse(amount == 1, word, plural)
    return(word)
}



.getCrisprNuclease <- function(nuclease
){
    nuclease <- grep(
        nuclease,
        crisprBase::getAvailableCrisprNucleases(),
        value=TRUE,
        ignore.case=TRUE)
    nuclease <- getExportedValue("crisprBase", nuclease)
    return(nuclease)
}






## enables addition of tooltip to shiny widget's label attribute
.widgetWrapper <- function(widget,
                           ns,
                           inputId,
                           label,
                           tab="none",
                           ...
){
    label <- .addTooltip(
        label=label,
        inputId=inputId
    )
    widget <- getExportedValue("shiny", widget)
    
    widget(
        inputId=ns(inputId),
        label=label,
        ...
    )
}



## actionButton wrapper
.actionButton <- function(...
){
    shiny::actionButton(
        class="app-btn",
        ...
    )
}




### Accessors -----------------------------------------------------------------


.getNoncanonicalPams <- function(nuclease){
    allPams <- crisprBase::pams(nuclease, as.character=TRUE, primary=FALSE)
    canonicalPams <- crisprBase::pams(nuclease, as.character=TRUE, primary=TRUE)
    noncanonicalPams <- setdiff(allPams, canonicalPams)
    return(noncanonicalPams)
}

