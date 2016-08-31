#' get_car_data
#'
#' Obtain data from Autocosmos according to the selecte brand and make
#' @param brand The brand according to the catalog
#' @param make The make according to the catalog
#' @author Jean Arreola
#' @return A list containig the url image, price, model prices and competitors
#' @examples
#' get_car_data( "Volkswagen", "Jetta")
#' @import rvest
#' @import dplyr
#' @import purrr
#' @import xml2




get_car_data <- function(brand,submarca) {

  brand_rv    <- tolower(brand)
  submarca_rv <- tolower(submarca)

  brand_rv    <- gsub(" ","-", brand_rv)
  submarca_rv <- gsub(" ","-", submarca_rv)

  brand_rv  <- gsub("á", "a", brand_rv )
  brand_rv  <- gsub("é", "e", brand_rv )
  brand_rv  <- gsub("í", "i", brand_rv )
  brand_rv  <- gsub("ó", "o", brand_rv )
  brand_rv  <- gsub("ú", "u", brand_rv )

  submarca_rv <- gsub("á", "a", submarca_rv)
  submarca_rv <- gsub("é", "e", submarca_rv)
  submarca_rv <- gsub("í", "i", submarca_rv)
  submarca_rv <- gsub("ó", "o", submarca_rv)
  submarca_rv <- gsub("ú", "u", submarca_rv)


  url <- paste0("http://www.autocosmos.com.mx/catalogo/vigente/",brand_rv,"/",submarca_rv)

  ht <- read_html(url)

  ht %>% html_nodes(xpath = "//img") %>%
    html_attr("src") %>%
    map_df(function(x) {
      data.frame(url = x[grep("imgcatalogo",x)])
    }
    ) %>%
    distinct() -> im

  ht %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "valorprecio", " " ))]') %>%
    html_text() %>%
    unique() -> precio


  ht %>% html_nodes(css = '.main') %>%
    html_nodes(css = ".title") %>%
    as.character() %>%
    map_df(function(x) {
      data.frame(url = x[grep(paste(brand, submarca),x)])
    }
    ) %>%
    distinct() %>%
    map_df(function(x) {
      data.frame(url = x[grep("name",x)])
    }
    ) -> ver

  ind_ver <- grep("\n",ver[,1])

  versiones <- rep("A",nrow(ver))

  for( i in 1:nrow(ver)){
    gregexpr("content",ver$url)[[i]][1] -> val
    prep <- substr(ver$url[i],val+9,nchar(as.character(ver$url[i])))
    gregexpr('\"',prep)[[1]][1] -> val2
    prep2 <- substr(prep, 1 , val2-1)
    versiones[i] <- prep2
  }

  ht %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "precio", " " ))]') %>%
    html_text() -> ver_price

  ver_pre <- data.frame(versiones = versiones, precios = ver_price)

  ht %>%  html_nodes(css = ".comparacion") %>%
    html_text() %>%
    gsub('[\r\n\t]',"",.) %>%
    .[-grep(paste(brand,submarca),.)] %>%
    .[-grep("Elige",.)] -> competencia

  lista <- list(url = im$url[1],precio = precio, versiones = ver_pre, competencia = competencia)

  return(lista)
}
