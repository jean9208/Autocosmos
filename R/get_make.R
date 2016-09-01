#' get_make
#'
#' Obtain all the makes included in Autocosmos website for the current year
#' @author Jean Arreola
#' @return A character string containing all the makes in the catalog.
#' @examples
#' get_make()
#' @import rvest
#' @import xml2
#' @import dplyr



#' @export
#' @rdname  get_make
get_make <- function(){

  cat <- "http://www.autocosmos.com.mx/catalogo"

  hcat <- read_html(cat)


  hcat %>%  html_nodes(css = "span") %>%
    html_text() %>%
    .[nchar(.)>1] %>%
    .[-grep("Elige",.)] %>%
    .[-grep("Hola",.)] %>%
    .[-grep("logo",.)] %>%
    .[1:grep("Volvo",.)]
}

