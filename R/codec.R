

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decode a drawio encode string.
#'
#' Apply (1) base64 decoding, (2) gzip inflation, and (3) URL decode
#'
#' For comparison see online tool: \url{https://jgraph.github.io/drawio-tools/tools/convert.html}
#'
#' @param encoded_string encode string in a draw io document
#'
#' @return raw text
#'
#' @import base64enc
#' @import gzmem
#' @importFrom utils URLdecode
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decode <- function(encoded_string) {
  decoded  <- base64enc::base64decode(encoded_string)
  deflated <- gzmem::mem_inflate(decoded, format = 'raw', r_guess_size = 1e6)
  raw_text <- utils::URLdecode(rawToChar(deflated))
  raw_text
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Encode a raw string into a encoded/compressed drawio string.
#'
#' Apply (1) URL encode, (2) gzip deflation, (3) base64 encoding
#'
#' For comparison see online tool: \url{https://jgraph.github.io/drawio-tools/tools/convert.html}
#'
#' @param raw_text encode string in a draw io document
#'
#' @return encoded string
#'
#' @import base64enc
#' @import gzmem
#' @importFrom utils URLdecode
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
encode <- function(raw_text) {
  url_encoded <- utils::URLencode(raw_text)
  deflated    <- gzmem::mem_compress(charToRaw(url_encoded), format = 'raw')
  b64_encoded <- base64enc::base64encode(deflated)
  b64_encoded
}

