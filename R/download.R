.pb_index_url = "http://physionet.org/physiobank/database/"

#' Available PhysioBank databases
#'
#' @return tibble
#' @export
#'
#' @examples
#' pb_catalog()
db_catalog <- function(){suppressWarnings({
  httr::GET(file.path(.pb_index_url, "DBS")) %>%
    httr::content("text", encoding = "UTF-8") %>%
    stringr::str_replace_all("\t{3,}", "\t") %>%
    readr::read_tsv(col_names = FALSE) %>%
    magrittr::set_colnames(c("name", "description"))
})}


#' Records in a PhysioBank database
#'
#' @param db_name
#'
#' @return character vector
#' @export
#'
db_records <- function(db_name){
  httr::GET(file.path(.pb_index_url, db_name, "RECORDS")) %>%
    httr::content("text", encoding = "UTF-8") %>%
    readr::read_lines()
}

#' Annotators in a PhysioBank database
#'
#' @param db_name
#'
#' @return tibble
#' @export
#'
db_annotators <- function(db_name){
  response <- httr::GET(file.path(.pb_index_url, db_name, "ANNOTATORS"))
  `if`(response$status_code == 404, return(NULL))
  response %>%
    httr::content("text", encoding = "UTF-8") %>%
    readr::read_tsv(col_names = FALSE) %>%
    magrittr::set_colnames(c("name", "description"))
}



#' List entire file structure of a PhysioBank database
#'
#' @param db_name
#'
#' @return character vector
#' @export
#'
ls_db <- function(db_name){
  db_url <- paste0(.pb_index_url, db_name)

  #TODO: provide exception for mimicdb


  db_dir_expand <- function(path){
    db_dir_content <- function(path) {
      xml2::read_html(path) %>%
        rvest::html_nodes("pre a") %>%
        rvest::html_text() %>%
        `[`(6:length(.)) # keeps only file/dir names, which starts at position 6
    }

    # expand dir, and paste with content
    path %>%
      purrr::map_if(~ stringr::str_ends(., "/"),
                    ~ db_dir_content(file.path(db_url, .)) %>% paste0(.x, .)) %>%
      purrr::flatten_chr() #avoids recursion, but takes a little more time
  }

  # keep expanding from root until no path left
  res <- db_dir_expand(path = "/")
  while(any(stringr::str_ends(res, "/"))){
    res <- db_dir_expand(res)
  }
  res
}

