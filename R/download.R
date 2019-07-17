.pb_index_url = "http://physionet.org/physiobank/database/"

#' Get PhysioBank
#'
#' @return tibble
#' @export
#'
#' @examples
#' pb_catalog()
pb_get_db_catalog <- function(){suppressWarnings({
  httr::GET(file.path(.pb_index_url, "DBS")) %>%
    httr::content("text", encoding = "UTF-8") %>%
    stringr::str_replace_all("\t{3,}", "\t") %>%
    readr::read_tsv(col_names = FALSE) %>%
    magrittr::set_colnames(c("name", "description"))
})}


pb_list_records <- function(db_name){
  httr::GET(file.path(.pb_index_url, db_name, "RECORDS")) %>%
    httr::content("text") %>%
    readr::read_lines()
}

pb_download_records <- function(db_name, target_path, flatten_dir = FALSE, overwrite = FALSE) {



}
