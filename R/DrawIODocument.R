

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function for indenting XML output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_indent <- function(depth) {
  paste0(rep("  ", depth), collapse = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DrawIODocument Class
#'
#' This is a slightly specialized subclass of \code{DrawIOElement}.
#'
#' DrawIO documents *must* have a root element
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DrawIODocument <- R6::R6Class(
  "DrawIODocument", inherit = DrawIOElement,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Create a document
    #'
    #' @param .name node name
    #' @param host host. default 'app.diagrams.net'
    #' @param modified Last modified date
    #' @param agent user agent
    #' @param etag etag
    #' @param version version
    #' @param type default: "device"
    #' @param ... extra arguments passed to \code{DrawIOElement}
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(.name    = 'mxfile',
                          host     = "app.diagrams.net" ,
                          modified = "2020-04-14T06:47:56.100Z" ,
                          agent    = "5.0 (Macintosh)" ,
                          etag     = "4dx1W4AIjCXgZhGWsQ6w" ,
                          version  = "12.9.12" ,
                          type     = "device", ...) {
      super$initialize(
        .name   = .name,
        host    = host,
        agent   = agent,
        version = version,
        type    = type,
        ...
      )
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save DrawIO document
    #'
    #' @param filename filename
    #' @param include_declaration default: TRUE
    #' @param ... extra arguments passed to parent class
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename, include_declaration = TRUE, ...) {
      super$save(filename, include_declaration = include_declaration, ...)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Print the DrawIO string
    #'
    #' @param include_declaration default: TRUE
    #' @param ... extra arguments passed to parent class
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(include_declaration = TRUE, ...) {
      super$print(include_declaration = include_declaration, ...)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Character representation
    #'
    #' @param ... extra arguments passed to parent class
    #' @param depth current rendering depth
    #' @param include_declaration default: TRUE
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(..., depth = 0, include_declaration = TRUE) {
      super$as_character(include_declaration = include_declaration, depth = depth, ...)
    }

  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname DrawIODocument
#' @usage NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_doc <- function(.name, ...) {
  DrawIODocument$new(.name = .name, ...)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Experiments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  rect_style <- "rounded=0;whiteSpace=wrap;html=1;"

  doc <- DrawIODocument$new()

  cell0 <- drawio_elem('mxCell', id = 0)
  cell1 <- drawio_elem('mxCell', id = 1, parent = 0)
  cell2 <- drawio_elem('mxCell', id = 2, parent = 1, vertex = 1, style = rect_style, value = "")
  cell2$mxGeometry(x = 360, y = 430, width = 120, height = 60, as = 'geometry')

  root <- drawio_elem('root', cell0, cell1, cell2)

  doc$
    diagram(id = 'aaa')$
    mxGraphModel()$
    append(root)

  doc

  doc$save("working/first.xml")
}

