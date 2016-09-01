#' get_model
#'
#' Obtain all the possible models for a selected make.
#' @author Jean Arreola
#' @return A character string containing all the models in the catalog.
#' @examples
#' make <- get_make()[1]
#' get_model(make)
#' @import rvest
#' @import xml2
#' @import dplyr



#' @export
#' @rdname  get_model
get_model <- function(make) {

  make_rv <- tolower(make)
  make_rv <- gsub(" ", "-", make_rv)
  make_rv <- gsub("á", "a", make_rv)
  make_rv <- gsub("é", "e", make_rv)
  make_rv <- gsub("í", "i", make_rv)
  make_rv <- gsub("ó", "o", make_rv)
  make_rv <- gsub("ú", "u", make_rv)

  sub <- paste0("http://www.autocosmos.com.mx/catalogo/vigente/", make_rv)

  hsub <- read_html(sub)

  hsub %>% html_nodes(css = "strong") %>% html_text() %>% (function(x) {
    z <- x
    if (length(grep("Estados", z)) > 0)
      z <- z[-grep("Estados", z)]
    if (length(grep("Breta", z)) > 0)
      z <- z[-grep("Breta", z)]
    if (length(grep("Italia", z)) > 0)
      z <- z[-grep("Italia", z)]
    if (length(grep("Alemania", z)) > 0)
      z <- z[-grep("Alemania", z)]
    if (length(grep("China", z)) > 0)
      z <- z[-grep("China", z)]
    if (length(grep("xico", z)) > 0)
      z <- z[-grep("xico", z)]
    if (length(grep("Espa", z)) > 0)
      z <- z[-grep("Espa", z)]
    if (length(grep("Eslovaquia", z)) > 0)
      z <- z[-grep("Eslovaquia", z)]
    if (length(grep("Canada", z)) > 0)
      z <- z[-grep("Canada", z)]
    if (length(grep("Tailandia", z)) > 0)
      z <- z[-grep("Tailandia", z)]
    if (length(grep("Turqu", z)) > 0)
      z <- z[-grep("Turqu", z)]
    if (length(grep("Brasil", z)) > 0)
      z <- z[-grep("Brasil", z)]
    if (length(grep("Argentina", z)) > 0)
      z <- z[-grep("Argentina", z)]
    if (length(grep("Corea", z)) > 0)
      z <- z[-grep("Corea", z)]
    if (length(grep("India", z)) > 0)
      z <- z[-grep("India", z)]
    if (length(grep("Japon", z)) > 0)
      z <- z[-grep("Japon", z)]
    if (length(grep("Francia", z)) > 0)
      z <- z[-grep("Francia", z)]
    if (length(grep("Austria", z)) > 0)
      z <- z[-grep("Austria", z)]
    if (length(grep("Colombia", z)) > 0)
      z <- z[-grep("Colombia", z)]
    if (length(grep("Indonesia", z)) > 0)
      z <- z[-grep("Indonesia", z)]
    if (length(grep("Suecia", z)) > 0)
      z <- z[-grep("Suecia", z)]
    return(z)
  })
}
