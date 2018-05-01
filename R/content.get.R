#' @author Gregoire Versmee, Laura Versmee
#' @import httr

content.get <- function(url, token, status = FALSE, verbose = FALSE) {

  if (is.null(token)) {
    if (status) {
      return(httr::status_code(httr::GET(URLencode(url))))
    } else return(httr::content(httr::GET(URLencode(url))))
  } else {
    if (status) {
      return(httr::status_code(httr::GET(URLencode(url), httr::add_headers(Authorization=paste("bearer", token)))))
    } else  return(httr::content(httr::GET(URLencode(url), httr::add_headers(Authorization=paste("bearer", token)))))
  }
}
