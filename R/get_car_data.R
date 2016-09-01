#' get_car_data
#'
#' Obtain data from Autocosmos according to the selecte make and model
#' @param make The make according to the catalog
#' @param model The model according to the catalog
#' @author Jean Arreola
#' @return A list containig the url image, price, version prices and competitors
#' @examples
#' make <- get_make()[1]
#' model <- get_model(make)[1]
#' get_car_data(make, model)
#' @import rvest
#' @import dplyr
#' @import purrr
#' @import xml2



#' @export
#' @rdname  get_car_data
get_car_data <- function(make,model) {

  make_rv    <- tolower(make)
  model_rv <- tolower(model)

  make_rv    <- gsub(" ","-", make_rv)
  model_rv <- gsub(" ","-", model_rv)

  make_rv  <- gsub("á", "a", make_rv )
  make_rv  <- gsub("é", "e", make_rv )
  make_rv  <- gsub("í", "i", make_rv )
  make_rv  <- gsub("ó", "o", make_rv )
  make_rv  <- gsub("ú", "u", make_rv )

  model_rv <- gsub("á", "a", model_rv)
  model_rv <- gsub("é", "e", model_rv)
  model_rv <- gsub("í", "i", model_rv)
  model_rv <- gsub("ó", "o", model_rv)
  model_rv <- gsub("ú", "u", model_rv)


  url <- paste0("http://www.autocosmos.com.mx/catalogo/vigente/",make_rv,"/",model_rv)

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
    unique() -> price


  ht %>% html_nodes(css = '.main') %>%
    html_nodes(css = ".title") %>%
    as.character() %>%
    map_df(function(x) {
      data.frame(url = x[grep(paste(make, model),x)])
    }
    ) %>%
    distinct() %>%
    map_df(function(x) {
      data.frame(url = x[grep("name",x)])
    }
    ) -> ver

  ind_ver <- grep("\n",ver[,1])

  version <- rep("A",nrow(ver))

  for( i in 1:nrow(ver)){
    gregexpr("content",ver$url)[[i]][1] -> val
    prep <- substr(ver$url[i],val+9,nchar(as.character(ver$url[i])))
    gregexpr('\"',prep)[[1]][1] -> val2
    prep2 <- substr(prep, 1 , val2-1)
    version[i] <- prep2
  }

  ht %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "precio", " " ))]') %>%
    html_text() -> ver_price

  ver_pre <- data.frame(version = version, prices = ver_price)

  ht %>%  html_nodes(css = ".comparacion") %>%
    html_text() %>%
    gsub('[\r\n\t]',"",.) %>%
    .[-grep(paste(make,model),.)] %>%
    .[-grep("Elige",.)] -> competitors

  car_list <- list(url = im$url[1],price = price, version = ver_pre, competitors = competitors)

  return(car_list)
}
