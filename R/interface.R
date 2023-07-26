.fullUI <- function(session,
                    title,
                    dataframes
){
    ns <- session$ns
    
    shiny::wellPanel(
        class="gsm-wellpanel",
        shiny::renderUI({
            shiny::column(
                width=12,
                align="center",
                class="gsm-wellpanel-header",
                style="margin-bottom: 20px;",
                title
            )
        }),
        shiny::div(
            class="gsm-wellpanel-content",
            shiny::uiOutput(ns("primaryTable")),
            shiny::renderUI({
                ## temporary: require gene_symbol (also include gene_id?)
                shiny::req("gene_symbol" %in% colnames(dataframes[["primary"]]))
                shiny::tagList(
                    shiny::hr(class="hr-intramodule"),
                    shiny::uiOutput(ns("guideBrowser"))
                )
            }),
            shiny::renderUI({
                annotationNames <- c("alignments",
                                     "geneAnnotation",
                                     "tssAnnotation",
                                     "enzymeAnnotation",
                                     "snps")
                hasAnnotations <- any(annotationNames %in% names(dataframes))
                hasData <- nrow(dataframes[["primary"]]) > 0
                shiny::req(hasAnnotations, hasData)
                shiny::uiOutput(ns("annotations"))
            })
        )
    )
}



.annotationsModule <- function(session,
                               dataframes
){
    shiny::req(dataframes)
    guideIds <- dataframes[["primary"]][["ID"]]
    
    ns <- session$ns
    annotations <- names(dataframes)
    annotationNames <- c("alignments",
                         "geneAnnotation",
                         "tssAnnotation",
                         "enzymeAnnotation",
                         "snps")
    hasAnnotations <- any(annotationNames %in% annotations)
    if (!hasAnnotations){
        return()
    }
    
    .guideSetModuleAnnotationTab <- function(title,
                                             content,
                                             hasAnnotation
    ){
        if (hasAnnotation){
            shiny::tabPanel(
                title=title,
                shiny::div(
                    style="margin: 10px;", # define as class
                    content
                )
            )
        }
    }
    
    shiny::div(
        class="gsm-tabset",
        shiny::tabsetPanel(
            id=ns("annotations"),
            type="pills",
            header=shiny::fluidRow(
                id="annotationsHeader",
                class="input",
                shiny::column(
                    width=4,
                    .widgetWrapper(
                        widget="selectizeInput",
                        ns=ns,
                        inputId="annotationsGuideSelect",
                        label="Select sgRNA",
                        choices=guideIds, #character(0),
                        selected=guideIds[1],
                        width="100%"
                    )
                )
            ),
            .guideSetModuleAnnotationTab(
                title="On- and Off-targets",
                content=shiny::uiOutput(ns("alignmentsTab")),
                hasAnnotation="alignments" %in% annotations
            ),
            .guideSetModuleAnnotationTab(
                title="Gene Annotation",
                content=shiny::uiOutput(ns("geneAnnotationTab")),
                hasAnnotation="geneAnnotation" %in% annotations
            ),
            .guideSetModuleAnnotationTab(
                title="TSS Annotation",
                content=shiny::uiOutput(ns("tssAnnotationTab")),
                hasAnnotation="tssAnnotation" %in% annotations
            ),
            .guideSetModuleAnnotationTab(
                title="Restriction Enzymes",
                content=shiny::uiOutput(ns("enzymeAnnotationTab")),
                hasAnnotation="enzymeAnnotation" %in% annotations
            ),
            .guideSetModuleAnnotationTab(
                title="SNPs",
                content=shiny::uiOutput(ns("snpsTab")),
                hasAnnotation="snps" %in% annotations
            )
        )
    )
}
