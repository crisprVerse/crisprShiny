## widgets =====================================================================

#' @importFrom shinyBS tipify
.addTooltip <- function(label,
                        inputId,
                        placement="top"
){
    tooltip <- NULL # create function for widget tooltip text
    if (!is.null(tooltip)){
        label <- shiny::span(
            label,
            shinyBS::tipify(
                shiny::icon(
                    "info-circle",
                    class="tooltip-icon",
                    verify_fa=FALSE
                ),
                title=tooltip,
                placement=placement
            )
        )
    }
    return(label)
}



## datatable columns ===========================================================


#' @importFrom shinyjs runjs
.initiateDatatableTooltips <- function(data
){
    for(i in seq_len(ncol(data))){
        createTooltip <- sprintf(paste(
            "for(var i = 1; i <= %d; i++){",
            "  var div;",
            "  var id = 'TOOLTIP-' + i.toString();",
            "  if(document.getElementById(id) === null){",
            "    div = document.createElement('div');",
            "    div.setAttribute('id', id);",
            "    div.style.display = 'none';",
            "    document.body.appendChild(div);",
            "  }",
            "}",
            sep = "\n"
        ), i)
        writeTooltip <- sprintf(paste(
            "var div = document.getElementById('TOOLTIP-%s');",
            "var html = '%s';",
            "div.innerHTML = html;",
            sep = "\n"
        ), i, .dtTooltipText(colnames(data)[i]))
        
        shinyjs::runjs(createTooltip)
        shinyjs::runjs(writeTooltip)
    }
}




#' @importFrom htmlwidgets JS
.getHeaderCallback <- function(ncols
){
    formatting <- sprintf(paste(
        "{",
        "  overwrite: true,",
        "  content: {",
        "    text: $('#TOOLTIP-%s').clone()",
        "  },",
        "  show: {",
        "    ready: false",
        "  },",
        "  position: {",
        "    my: 'bottom %%s',", # where tip appears in tooltip
        "    at: 'top center'", # where tip points to in colname div
        "  },",
        "  style: {",
        "    classes: 'tooltip-new'", # tooltip class name (change)
        "  }",
        "}",
        sep = "\n"
    ), seq_len(ncols))
    formatting <- sprintf(formatting,
                          ifelse(seq_len(ncols) < ncols/2, "left", "right"))
    formatting <- sprintf("var tooltips = [%s];",
                          paste0(formatting, collapse=","))
    
    headerCallback <- c(
        "function(thead, data, start, end, display){",
        "  var ncols = ", ncols, ";",
        formatting,
        "  for(var i = 0; i < ncols; i++){",
        "    $('th:eq(' + i + ')', thead).qtip(tooltips[i]);",
        "  }",
        "}"
    )
    htmlwidgets::JS(headerCallback)
}



## tooltip text ================================================================

.dtTooltipText <- function(colname
){
    if (grepl("^score_", colname)){
        tooltip <- .dtTooltipText_score(colname)
    } else if (colname %in% names(tooltipAnnotation)){
        tooltip <- tooltipAnnotation[[colname]]
    } else {
        tooltip <- paste0("\\'", colname, "\\' column")   # tooltip not defined
    }
    return(tooltip)
}




.dtTooltipText_score <- function(colname
){
    scoreMethods <- crisprScore::scoringMethodsInfo
    colname <- gsub("^score_", "", colname)
    prefix <- ""
    if (grepl("_aggregate$", colname)){ # off-target scores in on-targets table
        prefix <- "Aggregate"
        colname <- gsub("_aggregate$", "", colname)
    }
    if (colname %in% scoreMethods$method){
        index <- scoreMethods$method == colname
        type <- scoreMethods$type[index]
        if (nzchar(prefix)){
            type <- tolower(type)
        }
        prefix <- paste(prefix, type, "score")
        colname <- scoreMethods$label[index]
    } else { # score label not found
        prefix <- "Score"
        colname <- paste0("\\'", colname, "\\'")
    }
    tooltip <- paste(prefix, "using the", colname, "method")
    return(tooltip)
}