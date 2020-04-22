

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Build a properly formatted style string
#'
#' Drop NULLs
#'
#' @param ... named and unnamed arguments converted to a style string
#'
#' @examples{
#' \dontrun{
#' build_style(a = 1, b = 2)
#' }
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
build_style <- function(...) {
  params <- list(...)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove any NULL values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  valid_idx <- vapply(params, Negate(is.null), logical(1))
  params <- params[valid_idx]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split named and unnamed values and handle differently
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(names(params))) {
    names(params) <- rep('', length(params))
  }
  unnamed_params <- params[names(params) == '']
  named_params   <- params[names(params) != '']

  unnamed_params <- as.character(unnamed_params)
  named_params   <- paste(names(named_params), unname(named_params), sep = '=')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse it all into a ";"-separate style string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style <- paste(c(unnamed_params, named_params), collapse = ";")
  paste0(style, ";")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DrawIO element builder
#'
#' @import R6
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DrawIOElement <- R6::R6Class(
  "DrawIOElement",

  public = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field .name node name e.g. 'mxPoint', 'root'
    #' @field attribs list of attributes for this node e.g. \code{width = 100}
    #' @field children list of child nodes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .name    = NULL,
    attribs  = NULL,
    children = NULL,

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Create a drawio element
    #'
    #' @param .name node name
    #' @param ... named elements are treated as attributes and the rest are
    #'        added as children for this node
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(.name, ...) {
      self$.name    <- .name
      self$attribs  <- list()
      self$children <- list()

      self$update(...)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Update the XML Element.
    #'
    #' @param ... what to update in this element
    #'
    #' \itemize{
    #' \item{Named arguments are considered attributes and will overwrite
    #'     existing attributes with the same name. Set to NULL to delete the attribute}
    #' \item{Unnamed arguments are appended to the list of child nodes.  These
    #'     should be text, other DrawIOElements or any ojbect that can be represented
    #'     as a single text string using "as.character()"}
    #' \item{to print just the attribute name, but without a value, set to NA}
    #' }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update = function(...) {
      varargs      <- list(...)
      vararg_names <- names(varargs)
      if (is.null(vararg_names)) {
        vararg_names <- character(length = length(varargs))
      }
      has_name   <- nzchar(vararg_names)

      children <- varargs[!has_name]
      attribs  <- varargs[ has_name]

      self$attribs  <- modifyList(self$attribs, attribs, keep.null = FALSE)
      do.call(self$append, children)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Append a child node
    #'
    #' @param ... child nodes
    #' @param position default: NULL (append at end)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    append = function(..., position = NULL) {
      child_objects <- list(...)

      if (is.null(position)) {
        self$children <- append(self$children, child_objects)
      } else {
        self$children <- append(self$children, child_objects, after = position - 1)
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Create-and-add a child node to this element
    #'
    #' @param .name name of child node
    #' @param ... attributes of child node
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add = function(.name, ...) {
      if (!is.character(.name)) {
        stop("DrawIOElement$add(): 'name' must be a character string")
      }
      new_elem <- DrawIOElement$new(.name, ...)
      self$append(new_elem)
      invisible(new_elem)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Remove child objects at the given indicies
    #' @param indices indices at which to remove children
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    remove = function(indices) {
      self$children[indices] <- NULL
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Render this node at a particular depth
    #'
    #' @param depth indentation
    #' @param ... extra arguments
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character_inner = function(..., depth = 0) {
      indent1   <- create_indent(depth)
      indent2   <- create_indent(depth + 1)

      na_attribs    <- Filter(       is.na , self$attribs)
      value_attribs <- Filter(Negate(is.na), self$attribs)

      if (length(na_attribs) > 0) {
        na_attribs  <- names(na_attribs)
        na_attribs  <- paste(na_attribs, sep = " ")
        na_attribs  <- paste0(" ", na_attribs)
      } else {
        na_attribs <- NULL
      }

      if (length(value_attribs) > 0) {
        value_attribs <- paste(names(value_attribs),
                               paste0('"', unlist(value_attribs), '"'),
                               sep = "=", collapse = " ")
        value_attribs <- paste0(" ", value_attribs)
      } else {
        value_attribs <- NULL
      }

      attribs <- paste0(c(value_attribs, na_attribs), sep = "")
      open    <- glue::glue("{indent1}<{self$.name}{attribs}>")
      close   <- glue::glue("{indent1}</{self$.name}>")

      if (length(self$children) > 0) {
        children <- lapply(
          self$children,
          function(x, depth) {
            if (is.character(x)) {
              paste0(indent2, x)
            } else {
              x$as_character_inner(depth = depth)
            }
          }
          , depth = depth + 1)
        children <- unlist(children, use.names = FALSE)
      } else {
        children = NULL
      }

      if (is.null(children)) {
        open  <- glue::glue("{indent1}<{self$.name}{attribs} />")
        close <- NULL
      }

      c(open, children, close)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Character representation
    #'
    #' @param ... extra arguments passed to parent class
    #' @param depth current rendering depth
    #' @param include_declaration default: TRUE
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(..., depth = 0, include_declaration = FALSE) {
      xml_string <- paste0(self$as_character_inner(depth = depth), collapse = "\n")

      if (include_declaration) {
        xml_string <- paste('<?xml version="1.0" encoding="UTF-8"?>', xml_string, sep = "\n")
      }

      xml_string
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Print the DrawIO string
    #'
    #' @param include_declaration default: TRUE
    #' @param ... extra arguments passed to parent class
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(include_declaration = FALSE, ...) {
      cat(self$as_character(include_declaration = include_declaration, ...))
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save DrawIO fragment. Usually not useful. Use \code{DrawIODocument$save()} instead
    #'
    #' @param filename filename
    #' @param include_declaration default: TRUE
    #' @param ... extra arguments passed to parent class
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename, include_declaration = FALSE, ...) {
      xml_string <- self$as_character(include_declaration = include_declaration)

      writeLines(xml_string, filename)
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Deep copy of the R6 object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone(deep = TRUE)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add a diagram node to this element
    #'
    #' @param id default: NULL
    #' @param name diagram name. default NULL
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    diagram = function(id=NULL, name=NULL, ...) {
      self$add('diagram', id=id, name=name, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add an `mxGraphModel` node to this element
    #'
    #' @param dx,dy size
    #' @param grid,gridSize,guides,tooltips,connect,arrows,fold,page,pageScale,pageWidth,pageHeight,math,shadow refer to drawio
    #'        documentation for explanation
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mxGraphModel = function(dx=1212, dy=906, grid=1, gridSize=10, guides=1, tooltips=1, connect=1, arrows=1, fold=1, page=1, pageScale=1, pageWidth=827, pageHeight=1169, math=0, shadow=0, ...) {
      self$add('mxGraphModel', dx=dx, dy=dy, grid=grid, gridSize=gridSize, guides=guides, tooltips=tooltips, connect=connect, arrows=arrows, fold=fold, page=page, pageScale=pageScale, pageWidth=pageWidth, pageHeight=pageHeight, math=math, shadow=shadow, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Core node: add a 'root' node to this element
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    root = function(...) {
      self$add('root', ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add an 'mxCell' node to this element
    #'
    #' @param id id for this node.
    #' @param parent id of parent node
    #' @param value text value
    #' @param style style string
    #' @param vertex 0/1 to indicate if this is a vertex or not
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mxCell = function(id=NULL, parent=NULL, value=NULL, style=NULL, vertex=NULL, ...) {
      self$add('mxCell', id=id, parent=parent, value=value, style=style, vertex=vertex, ...)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add an 'mxGeometry' node to this element
    #'
    #' @param x,y,width,height dimensions
    #' @param as interpretation for this node. usually 'geometry'
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mxGeometry = function(x=NULL, y=NULL, width=NULL, height=NULL, as=NULL, ...) {
      self$add('mxGeometry', x=x, y=y, width=width, height=height, as=as, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add a 'UserObject' node to this element
    #'
    #' @param label,style,link,id See drawio documentation for more info
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    UserObject = function(label=NULL, style=NULL, link=NULL, id=NULL, ...) {
      self$add('UserObject', label=label, style=style, link=link, id=id, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Core node: Add an 'mxPoint' node to this element
    #'
    #' @param x,y coordinates of point
    #' @param as interpretation for this node. usually 'geometry'
    #' @param ... further arguments passed on to '$add()'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mxPoint = function(x=NULL, y=NULL, as=NULL, ...) {
      self$add('mxPoint', x=x, y=y, as=as, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Compound node: Add a cell + geometry representing a rectangle
    #'
    #' @param x,y,width,height location and dimensions
    #' @param colour,fill,alpha appearace
    #' @param size stroke width
    #' @param id default: NULL (auto-assigned)
    #' @param parent id of parent
    #' @param ... further arguments for the element 'style'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect = function(x, y, width, height, colour = 'black', fill = 'white', size = 1, alpha = 1, id=NULL, parent = 1, ...) {

      style <- build_style(
        rounded     = 0,
        whiteSpace  = 'wrap',
        html        = 1,
        fillColor   = r2hex(fill),
        strokeColor = r2hex(colour),
        strokeWidth = size,
        opacity     = alpha * 100,
        ...
      )

      id   <- id %||% self$rand_id()
      cell <- drawio_elem('mxCell', id = id, value = "", style = style, vertex = 1, parent = parent)

      cell$mxGeometry(x = x, y= y, width = width, height = height, as="geometry")

      self$append(cell)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Compound node: Create a cell + geometry containing text
    #'
    #' @param text string
    #' @param x,y,width,height location and dimensions
    #' @param colour,fill,alpha appearace
    #' @param fontsize default: 12
    #' @param id default: NULL (auto-assigned)
    #' @param parent id of parent.
    #' @param angle degrees. defualt: 0
    #' @param font font name default: Helvetica
    #' @param align,vertical_align,label_position,vertical_label_position alignment of text
    #' @param ... further arguments for the element 'style'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text = function(text, x, y, width = fontsize, height = fontsize, colour = 'none',
                    fill = 'none', fontsize = 12, alpha = 1, id=NULL, parent = 1,
                    angle=0, font = c('Helvetica', 'Verdana', 'Times New Roman', 'Garamond',
                                      'Comic Sans MS', 'Courier New', 'Georgia', 'Lucinda Console',
                                      'Tahoma'),
                    align = 'center',
                    vertical_align = 'middle',
                    label_position = 'center',
                    vertical_label_position = 'top', ...) {

      # The text to render is actually rendered as an HTMLish tag
      textbit <- glue::glue("<font color='{r2hex(colour)}'>{text}</font>")
      textbit <- htmltools::htmlEscape(textbit)

      font <- match.arg(font)

      style <- build_style(
        'text',
        html          = 1,
        strokeColor   = 'none',
        fillColor     = r2hex(fill),
        align         = align,
        verticalAlign = vertical_align,
        labelPosition = label_position,
        verticalLabelPosition = vertical_label_position,
        rounded       = 0,
        rotation      = -angle,
        fontSize      = fontsize,
        fontFamily    = font,
        ...
      )

      id <- id %||% self$rand_id()
      cell <- drawio_elem('mxCell', id = id, value = textbit, style = style, vertex = 1, parent = parent)
      cell$mxGeometry(x = x, y= y, width = width, height = height, as="geometry")
      self$append(cell)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Compound node: Create a cell + geometry containing a line
    #'
    #' @param x1,y1,x2,y2 extents of
    #' @param colour,alpha appearace
    #' @param angle rotation angle
    #' @param size stroke width
    #' @param id default: NULL (auto-assigned)
    #' @param parent id of parent
    #' @param dashed dashed or not? default: 0 (not dashed)
    #' @param ... further arguments for the element 'style'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    line = function(x1, y1, x2, y2, colour = 'black', size = 1, alpha = 1, id = NULL, parent = 1, dashed = 0, ...) {

      style <- build_style(
        endArrow    = 'none',
        html        = 1,
        strokeColor = r2hex(colour),
        dashed      = dashed,
        opacity     = alpha * 100,
        strokeWidth = size,
        ...
      )

      width  <- abs(x1 - x2) + 1
      height <- abs(y1 - y2) + 1

      id <- id %||% self$rand_id()
      cell <- drawio_elem('mxCell', id = id, value = "", style = style, edge = 1, parent = parent)
      geom <- cell$mxGeometry(width = width, height = height, relative = '1', as="geometry")

      geom$mxPoint(x = x1, y = y1, as = 'sourcePoint')
      geom$mxPoint(x = x2, y = y2, as = 'targetPoint')

      self$append(cell)

      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Compound node: Create a cell + geometry containing a circle
    #'
    #' @param x,y,r location and radius
    #' @param colour,fill,alpha appearace
    #' @param stroke_width stroke width
    #' @param id default: NULL (auto-assigned)
    #' @param parent parent id
    #' @param ... further arguments for the element 'style'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    circle = function(x, y, r, colour = 'none', fill = 'black', stroke_width = 0, alpha = 1, id = NULL, parent = 1, ...) {

      style <- build_style(
        'ellipse',
        html        = 1,
        aspect      = 'fixed',
        strokeColor = r2hex(colour),
        strokeWidth = stroke_width,
        fillColor   = r2hex(fill),
        opacity     = alpha * 100,
        ...
      )


      id <- id %||% self$rand_id()
      cell <- drawio_elem('mxCell', id = id, value = "", style = style, vertex = 1, parent = parent)

      cell$mxGeometry(x = x - r, y= y - r, width = r * 2, height = r * 2, as="geometry")

      self$append(cell)

      invisible(self)

    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Compound node: create a cell + geometry for a polygon or open/closed polyline
    #'
    #' Complex shapes are added as stencils which are base64 encoded HTMLish tags
    #'
    #' @param xs,ys numeric vectors of coordinates
    #' @param colour,fill,alpha appearance
    #' @param stroke_width stroke width
    #' @param id default: NULL (auto-assigned)
    #' @param parent parent id
    #' @param close if the polyline open or closed? default closed
    #' @param ... further arguments for the element 'style'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon = function(xs, ys, colour = 'black', fill = 'red', stroke_width = 1, alpha = 1, id = NULL, parent = 1, close = FALSE, ...) {

      if (colour == 'none' && fill == 'none') {
        return(invisible(self))
      } else if (colour == 'none') {
        stroke <- 'fill'
      } else if (fill == 'none') {
        stroke <- 'stroke'
      } else {
        stroke <- 'fillstroke'
      }


      stencil_markup <- coords_to_stencil(xs, ys, close = close, stroke = 'fillstroke')
      encoded        <- encode(stencil_markup)
      stencil        <- paste0("stencil(", encoded, ")")

      style <- build_style(
        shape       = stencil,
        strokeColor = r2hex(colour),
        strokeWidth = stroke_width,
        fillColor   = r2hex(fill),
        opacity     = alpha * 100,
        ...
      )


      width  <- diff(range(xs))
      height <- diff(range(ys))

      # Ensure it has finite width. Important for straight lines!
      width  <- max(1, width)
      height <- max(1, height)

      id <- id %||% self$rand_id()
      cell <- drawio_elem('mxCell', id = id, value = "", style = style, vertex = 1, parent = parent)

      cell$mxGeometry(x = min(xs), y= min(ys), width = width, height = height, as="geometry")

      self$append(cell)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Generate a random ID for an element
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rand_id = function() {
      rando <- sample(c(letters, LETTERS), 8, replace = TRUE)
      rando <- paste(rando, collapse = "")
      paste0("drawio-", rando)
    }
  ),

  private = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # When called with `$clone(deep = TRUE)`, the 'deep_clone' function is
    # called for every name/value pair in the object.
    # See: https://r6.r-lib.org/articles/Introduction.html
    # Need special handling for:
    #   - 'children' is a list of R6 objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    deep_clone = function(name, value) {
      if (name %in% c('children')) {
        lapply(value, function(x) {if (inherits(x, "R6")) x$clone(deep = TRUE) else x})
      } else {
        value
      }
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve character representation of DrawIOElement
#'
#' @param x DrawIOElement object
#' @param include_declaration Include the XML declaration at the top? Default: FALSE
#' @param ... other arguments
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.DrawIOElement <- function(x, include_declaration = FALSE, ...) {
  x$as_character(include_declaration = include_declaration, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname DrawIOElement
#' @usage NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_elem <- function(.name, ...) {
  DrawIOElement$new(.name = .name, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a set of coordinates into a stencil for custom shape definition
#'
#' @param xs,ys coordinates
#' @param close is shape closed? default: FALSE (open)
#' @param stroke determines if stentil is filled, just the outline, or both
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_to_stencil <- function(xs, ys, close = FALSE, stroke = c('stroke', 'fill', 'fillstroke')) {

  close  <- ifelse(isTRUE(close), "<close/>", "")
  stroke <- match.arg(stroke)

  width  <- diff(range(xs))
  height <- diff(range(ys))

  # Ensure it has finite width. Important for straight lines!
  width  <- max(1, width)
  height <- max(1, height)

  # coordinates are percentages of the total width + height
  xs <- (xs - min(xs))/width  * 100
  ys <- (ys - min(ys))/height * 100

  x1   <- xs[1]
  y1   <- ys[1]
  xall <- xs[-1]
  yall <- ys[-1]

  lines <- glue::glue('<line x="{xall}" y="{yall}" />')

  glue::glue(
    '<shape strokewidth="inherit" height="{height}" width="{width}"><foreground><path>',
    '<move x="{x1}" y="{y1}" />',
    paste(lines, collapse=""),
    close,
    '</path><{stroke}/></foreground></shape>'
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Experimentation zone
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  doc   <- DrawIODocument$new()
  dia   <- doc$diagram(id = 'aaa')
  graph <- dia$mxGraphModel(dx = 0, dy = 0)
  root  <- graph$root()

  root$mxCell(id = 0)
  root$mxCell(id = 1, parent = 0)
  root$rect(x = 50, y = 50, width = 100, height = 50, size = 0, colour = NA, fill = 'green', alpha = 0.2)
  root$text(x = 150, y = 50, colour = 'blue', text = "Hello Rstats", fontsize = 50)

  root$line(x1 = 100, y1=100, x2=200, y2 = 200)

  root$circle(0, 0, 20, fill = 'red')
  root$polygon(c(0, 200, 200), c(0, 0, 200), fill = 'none')

  doc$save("working/mike-02.xml")

}


