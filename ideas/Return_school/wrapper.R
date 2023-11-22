

get_school_data <- function(var1) {
  
  api_url <- "http://127.0.0.1:3116/return_schools?var1="
  get_url <- paste0(api_url, var1)
  
  response <- httr::GET(get_url)
  response_parsed <- httr::content(response, as = "text")
  x <- jsonlite::fromJSON(response_parsed)
  
  return(tidyr::as_tibble(x))
  
}


get_school_data("absolut")